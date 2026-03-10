list_dataset_spec_files <- function(
  dataset_specs_dir = bp_dataset_specs_dir()
) {
  fs::dir_ls(
    dataset_specs_dir,
    recurse = TRUE,
    type = "file",
    regexp = "dataset[.]ya?ml$"
  )
}

read_dataset_specs <- function(files = list_dataset_spec_files()) {
  specs <- lapply(files, yaml::read_yaml)

  for (i in seq_along(specs)) {
    specs[[i]]$.spec_file <- files[[i]]
  }

  specs
}
