dataset_harmonisation_paths <- function(dataset_id) {
  dataset_dir <- bp_harmonisation_dataset_dir(dataset_id)

  list(
    dataset_dir = dataset_dir,
    variables_file = fs::path(dataset_dir, "variables.csv"),
    lookup_dir = fs::path(dataset_dir, "lookups")
  )
}

dataset_has_harmonisation <- function(dataset_id) {
  paths <- dataset_harmonisation_paths(dataset_id)

  file.exists(paths$variables_file)
}

list_harmonisation_files <- function(dataset_id) {
  paths <- dataset_harmonisation_paths(dataset_id)

  files <- paths$variables_file

  if (fs::dir_exists(paths$lookup_dir)) {
    files <- c(
      files,
      fs::dir_ls(
        paths$lookup_dir,
        recurse = TRUE, type = "file", glob = "*.csv"
      )
    )
  }

  unique(unname(files))
}

conventional_tidier_name <- function(dataset_id) {
  paste0("tidy_BPIPD_", dataset_id)
}

find_tidier_file <- function(dataset_id) {
  path <- fs::path(
    "R", "tidy", paste0(conventional_tidier_name(dataset_id), ".R")
  )

  if (!fs::file_exists(path)) {
    stop("Tidier source file not found: ", path, call. = FALSE)
  }

  path
}
