read_resource_table <- function(
  resource, base_dir, wave = NULL, wave_label = NULL
) {
  role <- resource$role
  name <- resource$name
  paths <- resolve_glob_paths(base_dir, resource$glob)

  meta <- tibble::tibble(
    resource_name = name,
    role = role,
    base_dir = base_dir,
    file = paths
  )

  if (length(paths) == 0) {
    return(list(
      data = list(), codebook = list(), docs = character(), meta = meta
    ))
  }

  if (identical(role, "docs")) {
    return(list(data = list(), codebook = list(), docs = paths, meta = meta))
  }

  # Keep codebook paths for record-keeping, but do not read files.
  if (identical(role, "codebook")) {
    out <- list()
    for (i in seq_along(paths)) {
      nm <- name
      if (length(paths) > 1) nm <- paste0(name, "__", i)
      out[[nm]] <- paths[[i]]
    }
    return(list(data = list(), codebook = out, docs = character(), meta = meta))
  }

  # data resources
  reader <- resource$reader
  sheet <- resource$sheet %||% NULL
  range <- resource$range %||% NULL

  out <- list()
  for (i in seq_along(paths)) {
    nm <- name
    if (length(paths) > 1) nm <- paste0(name, "__", i)

    tbl <- read_tabular_file(
      paths[[i]],
      reader = reader, sheet = sheet, range = range
    )

    if (!is.null(wave)) {
      tbl <- dplyr::mutate(
        tbl,
        .wave = as.character(wave),
        .wave_label = as.character(wave_label)
      )
    }

    out[[nm]] <- tbl
  }

  if (identical(role, "data")) {
    list(data = out, codebook = list(), docs = character(), meta = meta)
  } else {
    stop("Unknown role: ", role, call. = FALSE)
  }
}
