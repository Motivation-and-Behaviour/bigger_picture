#' Index every resource declared in a spec, with where it was declared
#'
#' Returns one row per resource, in spec order, with the wave (or top-level
#' block) it came from. Resources without a usable `name` are skipped; the
#' schema requires one, but a malformed spec should fail on its own terms
#' rather than here.
resource_name_index <- function(spec) {
  collect <- function(resources, where) {
    rows <- list()
    for (res in resources) {
      name <- res$name
      if (is.null(name) || !nzchar(name)) {
        next
      }
      rows[[length(rows) + 1L]] <- tibble::tibble(
        name = as.character(name),
        role = as.character(res$role %||% NA_character_),
        where = where
      )
    }
    rows
  }

  rows <- collect(spec$resources, "top-level `resources`")

  for (w in spec$waves) {
    wave_id <- w$wave %||% w$label
    where <- if (is.null(wave_id)) {
      "an unnamed wave"
    } else {
      paste0("wave ", wave_id)
    }
    rows <- c(rows, collect(w$resources, where))
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      name = character(),
      role = character(),
      where = character()
    ))
  }

  dplyr::bind_rows(rows)
}

#' Fail early when a dataset spec reuses a resource name
#'
#' Resource names key `raw_dataset$data` and `raw_dataset$codebook`. The reader
#' accumulates waves with `c()`, which keeps duplicate names rather than
#' overwriting them, so a reused name does not lose data -- but `$name` and
#' `[["name"]]` return only the first match, which makes every later resource
#' with that name unreachable from the tidier. Nothing else in the pipeline
#' notices, so this check is what stands between a duplicated name and a
#' quietly dropped wave.
check_resource_names <- function(spec, spec_file = NULL) {
  resources <- resource_name_index(spec)
  duplicated_names <- unique(resources$name[duplicated(resources$name)])

  if (length(duplicated_names) == 0) {
    return(invisible(spec))
  }

  spec_label <- spec$.spec_file %||%
    spec_file %||%
    paste0("BPIPD-", spec$dataset_id %||% "?", "/dataset.yaml")

  shown <- utils::head(duplicated_names, 5L)
  bullets <- vapply(
    shown,
    function(name) {
      where <- resources$where[resources$name == name]
      paste0(
        "`",
        name,
        "` is declared ",
        length(where),
        " times (",
        collapse_some(unique(where), 3L),
        ")."
      )
    },
    character(1),
    USE.NAMES = FALSE
  )

  if (length(duplicated_names) > length(shown)) {
    bullets <- c(
      bullets,
      paste0(
        "... and ",
        length(duplicated_names) - length(shown),
        " more duplicated ",
        ngettext(length(duplicated_names) - length(shown), "name", "names"),
        "."
      )
    )
  }

  names(bullets) <- rep("x", length(bullets))

  rlang::abort(c(
    paste0("Duplicate resource names in `", spec_label, "`."),
    bullets,
    i = paste0(
      "Resource names key `raw_dataset$data` and `raw_dataset$codebook`, so ",
      "only the first resource with a given name can be reached by name in ",
      "the tidier."
    ),
    i = paste0(
      "Names must be unique across the whole spec, including across waves ",
      "and across roles."
    ),
    i = paste0(
      "Give each resource a wave-specific name, e.g. `data_2010`, ",
      "`data_2014`."
    )
  ))
}

#' Collapse a character vector for an error message, truncating the tail
collapse_some <- function(x, n) {
  shown <- utils::head(x, n)
  out <- paste(shown, collapse = ", ")
  if (length(x) > length(shown)) {
    out <- paste0(out, ", and ", length(x) - length(shown), " more")
  }
  out
}
