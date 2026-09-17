# Source all project functions, mimicking `targets::tar_source()`.
# Note: dplyr is deliberately NOT attached here; the harmonisation engine
# must work without it (expressions resolve via the allowlist instead).
repo_root <- normalizePath(testthat::test_path("..", ".."))

for (project_file in list.files(
  file.path(repo_root, "R"),
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)) {
  source(project_file)
}

# Minimal dataschema for engine tests: the engine only reads
# `variable_name` and `data_type`.
make_test_dataschema <- function() {
  tibble::tibble(
    variable_name = c(
      "dataset_id",
      "dataset_name",
      "participant_id",
      "age_years",
      "is_active"
    ),
    data_type = c("character", "character", "character", "double", "logical")
  )
}

make_test_variables <- function(...) {
  rows <- tibble::tribble(
    ...
  )
  defaults <- tibble::tibble(
    target_variable = character(),
    status = character(),
    source_columns = character(),
    expression = character(),
    notes = character(),
    lookup_table = character()
  )
  dplyr::bind_rows(defaults, rows)
}

make_test_config <- function(variables, lookups = list()) {
  list(
    dataset_id = "99",
    variables = variables,
    shared_lookups = lookups,
    dataset_lookups = list(),
    lookups = lookups
  )
}

make_test_spec <- function() {
  list(
    dataset_id = "99",
    dataset_name = "Test Study",
    status = "in_progress"
  )
}
