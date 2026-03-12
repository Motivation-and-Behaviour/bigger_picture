list_data_files_from_spec <- function(dataset_dir, spec) {
  files <- character()

  add_data_files <- function(base_dir, resources) {
    if (is.null(resources)) {
      return(invisible(NULL))
    }

    for (res in resources) {
      if (!identical(res$role, "data")) {
        next
      }
      files <<- c(files, resolve_glob_paths(base_dir, res$glob))
    }

    invisible(NULL)
  }

  # dataset-level resources
  add_data_files(dataset_dir, spec$resources)

  # wave-level resources
  if (!is.null(spec$waves)) {
    for (w in spec$waves) {
      wave_subdir <- w$wave_dir
      if (is.null(wave_subdir) || !nzchar(wave_subdir)) {
        stop("Each wave must define `wave_dir`.", call. = FALSE)
      }
      wave_dir <- fs::path(dataset_dir, wave_subdir)
      add_data_files(wave_dir, w$resources)
    }
  }

  unique(files)
}
