dataset_harmonisation_paths <- function(dataset_id) {
  dataset_dir <- bp_harmonisation_dataset_dir(dataset_id)

  list(
    dataset_dir = dataset_dir,
    layout_file = fs::path(dataset_dir, "layout.csv"),
    variables_file = fs::path(dataset_dir, "variables.csv"),
    lookup_dir = fs::path(dataset_dir, "lookups")
  )
}

dataset_has_harmonisation <- function(dataset_id) {
  paths <- dataset_harmonisation_paths(dataset_id)

  file.exists(paths$layout_file) && file.exists(paths$variables_file)
}

list_harmonisation_files <- function(dataset_id) {
  paths <- dataset_harmonisation_paths(dataset_id)

  files <- c(paths$layout_file, paths$variables_file)

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
