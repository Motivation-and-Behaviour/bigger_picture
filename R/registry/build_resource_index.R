#' Flatten a spec's resources into one row per declared resource
#'
#' Walks the spec in the order the reader consumes it -- top-level `resources`
#' first, then each wave in order -- so downstream ordering follows from the
#' `seq` column rather than from accumulation order.
flatten_spec_resources <- function(dataset_dir, spec) {
  describe <- function(resources, base_dir, wave, wave_label) {
    lapply(resources, function(res) {
      list(
        resource_name = as.character(res$name),
        role = as.character(res$role),
        base_dir = base_dir,
        glob = res$glob,
        reader = as.character(res$reader %||% NA_character_),
        read_opts = list(
          sheet = res$sheet %||% NULL,
          range = res$range %||% NULL,
          table = res$table %||% NULL,
          object = res$object %||% NULL
        ),
        wave = wave,
        wave_label = wave_label
      )
    })
  }

  rows <- describe(spec$resources, dataset_dir, NA_character_, NA_character_)

  for (w in spec$waves) {
    wave_subdir <- w$wave_dir
    if (is.null(wave_subdir) || !nzchar(wave_subdir)) {
      stop("Each wave must define `wave_dir`.", call. = FALSE)
    }

    # A wave without an identifier tags nothing; `read_resource_files()` keys
    # the `.wave` columns off `wave` being non-NA, matching the old reader.
    wave <- as.character(w$wave %||% NA_character_)
    wave_label <- if (is.na(wave)) {
      NA_character_
    } else {
      as.character(w$label %||% w$wave)
    }

    rows <- c(
      rows,
      describe(
        w$resources,
        fs::path(dataset_dir, wave_subdir),
        wave,
        wave_label
      )
    )
  }

  rows
}

#' Index every file a dataset spec resolves to, listing each directory once
#'
#' Resolving globs one directory walk at a time is what made ingestion slow: a
#' spec can declare hundreds of resources under a single `base_dir`, and each
#' walk of a large study tree costs about a second. This lists each distinct
#' `base_dir` once and matches every glob against that listing.
#'
#' Returns one row per resolved file, in spec order. A resource whose glob
#' matches nothing contributes no rows, so the `resource_name`/`role`/
#' `base_dir`/`file` columns are exactly `raw_dataset$meta$matches`.
build_resource_index <- function(dataset_dir, spec) {
  # Runs before anything is read: `element_name` is only a usable key while
  # resource names are unique across the whole spec.
  check_resource_names(spec)

  resources <- flatten_spec_resources(dataset_dir, spec)

  if (length(resources) == 0) {
    return(empty_resource_index())
  }

  # One directory walk -- and one path_rel() -- per distinct base_dir, reused by
  # every glob under it. Both are far more expensive than matching a pattern.
  base_dirs <- vapply(resources, function(r) as.character(r$base_dir), "")
  listings <- lapply(unique(base_dirs), function(dir) {
    files <- list_files_under(dir)
    list(files = files, rel = relative_to_base(files, dir))
  })
  names(listings) <- unique(base_dirs)

  rows <- list()
  for (i in seq_along(resources)) {
    res <- resources[[i]]
    listing <- listings[[base_dirs[[i]]]]
    paths <- resolve_glob_paths(
      res$base_dir,
      res$glob,
      files = listing$files,
      rel = listing$rel
    )

    if (length(paths) == 0) {
      next
    }

    # A glob matching several files fans out to `<name>__1`, `<name>__2`, ...;
    # a single match keeps the bare name. Tidiers index on these.
    element_name <- if (length(paths) > 1) {
      paste0(res$resource_name, "__", seq_along(paths))
    } else {
      res$resource_name
    }

    rows[[length(rows) + 1L]] <- tibble::tibble(
      resource_name = res$resource_name,
      element_name = element_name,
      role = res$role,
      base_dir = res$base_dir,
      file = paths,
      reader = res$reader,
      read_opts = rep(list(res$read_opts), length(paths)),
      wave = res$wave,
      wave_label = res$wave_label
    )
  }

  if (length(rows) == 0) {
    return(empty_resource_index())
  }

  index <- dplyr::bind_rows(rows)
  tibble::add_column(index, seq = seq_len(nrow(index)), .before = 1L)
}

empty_resource_index <- function() {
  tibble::tibble(
    seq = integer(),
    resource_name = character(),
    element_name = character(),
    role = character(),
    base_dir = character(),
    file = character(),
    reader = character(),
    read_opts = list(),
    wave = character(),
    wave_label = character()
  )
}

#' Split the data resources into batches for dynamic branching
#'
#' Reading is CPU-bound in the file readers, so batches spread across `crew`
#' workers. Batching rather than branching per file keeps the per-branch
#' overhead small relative to the work; single-resource datasets get one batch
#' and behave as they did before.
#'
#' An index with no data files is refused here: branching over an empty batch
#' table fails anyway ("cannot branch over empty target"), with a message that
#' points at targets internals instead of the missing data.
assign_read_batches <- function(index, size = 10L) {
  data_rows <- index[index$role == "data", , drop = FALSE]

  if (nrow(data_rows) == 0) {
    stop(
      "No data files matched this dataset's spec. ",
      "Check that the data directory is mounted and that each data ",
      "resource's `glob` matches the files on disk.",
      call. = FALSE
    )
  }

  groups <- ceiling(seq_len(nrow(data_rows)) / size)
  tibble::add_column(data_rows, tar_group = as.integer(groups))
}
