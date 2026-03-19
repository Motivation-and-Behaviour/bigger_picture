dataset_harmonisation_paths <- function(dataset_id) {
  dataset_dir <- bp_harmonisation_dataset_dir(dataset_id)

  list(
    dataset_dir = dataset_dir,
    variables_file = fs::path(dataset_dir, "variables.csv"),
    lookup_dir = fs::path(dataset_dir, "lookups"),
    shared_lookup_dir = bp_shared_lookup_dir()
  )
}

dataset_has_harmonisation <- function(dataset_id) {
  paths <- dataset_harmonisation_paths(dataset_id)

  file.exists(paths$variables_file)
}

declared_lookup_names_from_variables <- function(variables) {
  if (!"lookup_table" %in% names(variables)) {
    return(character())
  }

  lookup_names <- as.character(variables$lookup_table)
  lookup_names <- lookup_names[!is.na(lookup_names) & nzchar(lookup_names)]
  unique(lookup_names)
}

declared_lookup_names_from_file <- function(variables_file) {
  if (!fs::file_exists(variables_file)) {
    return(character())
  }

  variables <- readr::read_csv(variables_file, show_col_types = FALSE)
  declared_lookup_names_from_variables(variables)
}

resolve_lookup_files <- function(paths, lookup_names) {
  lookup_names <- unique(lookup_names)

  if (length(lookup_names) == 0) {
    return(tibble::tibble(
      lookup_name = character(),
      scope = character(),
      path = character()
    ))
  }

  resolved <- lapply(lookup_names, function(lookup_name) {
    dataset_path <- fs::path(paths$lookup_dir, paste0(lookup_name, ".csv"))
    shared_path <- fs::path(paths$shared_lookup_dir, paste0(lookup_name, ".csv"))

    has_dataset_lookup <- fs::file_exists(dataset_path)
    has_shared_lookup <- fs::file_exists(shared_path)

    if (has_dataset_lookup && has_shared_lookup) {
      stop(
        "Lookup table `",
        lookup_name,
        "` exists in both the dataset-specific and shared lookup directories.",
        call. = FALSE
      )
    }

    if (!has_dataset_lookup && !has_shared_lookup) {
      stop(
        "Lookup table `",
        lookup_name,
        "` was declared in variables.csv but no matching CSV file was found.",
        call. = FALSE
      )
    }

    tibble::tibble(
      lookup_name = lookup_name,
      scope = if (has_dataset_lookup) "dataset" else "shared",
      path = if (has_dataset_lookup) dataset_path else shared_path
    )
  })

  dplyr::bind_rows(resolved)
}

list_harmonisation_files <- function(dataset_id) {
  paths <- dataset_harmonisation_paths(dataset_id)
  lookup_names <- declared_lookup_names_from_file(paths$variables_file)
  lookup_files <- resolve_lookup_files(paths, lookup_names)

  unique(unname(c(paths$variables_file, lookup_files$path)))
}

conventional_tidier_name <- function(dataset_id) {
  paste0("tidy_BPIPD_", dataset_id)
}

find_tidier_file <- function(dataset_id) {
  path <- fs::path(
    "R",
    "tidy",
    paste0(conventional_tidier_name(dataset_id), ".R")
  )

  if (!fs::file_exists(path)) {
    stop("Tidier source file not found: ", path, call. = FALSE)
  }

  path
}
