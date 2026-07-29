list_data_files_from_spec <- function(dataset_dir, spec) {
  collect_data_files <- function(base_dir, resources) {
    if (is.null(resources)) {
      return(character())
    }

    found <- character()
    for (res in resources) {
      if (!identical(res$role, "data")) {
        next
      }
      found <- c(found, resolve_glob_paths(base_dir, res$glob))
    }

    found
  }

  # dataset-level resources
  files <- collect_data_files(dataset_dir, spec$resources)

  # wave-level resources
  if (!is.null(spec$waves)) {
    for (w in spec$waves) {
      wave_subdir <- w$wave_dir
      if (is.null(wave_subdir) || !nzchar(wave_subdir)) {
        stop("Each wave must define `wave_dir`.", call. = FALSE)
      }
      wave_dir <- fs::path(dataset_dir, wave_subdir)
      files <- c(files, collect_data_files(wave_dir, w$resources))
    }
  }

  unique(files)
}
