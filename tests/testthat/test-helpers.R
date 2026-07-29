test_that("lookup_values maps, filters, and defaults", {
  lookup <- tibble::tibble(
    dataset_id = c("21", "21", "99"),
    target_variable = c("sex", "sex", "sex"),
    source_value = c("1", "2", "1"),
    harmonised_value = c("Male", "Female", "Other"),
    mapping_status = "compatible",
    notes = NA_character_
  )

  expect_identical(
    lookup_values(c("1", "2"), lookup, dataset_id = "21"),
    c("Male", "Female")
  )
  expect_identical(
    lookup_values("1", lookup, dataset_id = "99"),
    "Other"
  )
  expect_identical(
    lookup_values(
      c("1", "banana"),
      lookup,
      dataset_id = "21",
      default = "Unknown"
    ),
    c("Male", "Unknown")
  )
  expect_identical(
    lookup_values(c("2", NA), lookup, dataset_id = "21"),
    c("Female", NA)
  )
})

test_that("lookup_values rejects invalid lookups", {
  expect_error(lookup_values("1", "not a table"), "must be a data frame")
  expect_error(
    lookup_values("1", tibble::tibble(source_value = "1")),
    "missing required columns: harmonised_value"
  )

  duplicated_lookup <- tibble::tibble(
    source_value = c("1", "1"),
    harmonised_value = c("a", "b")
  )
  expect_error(
    lookup_values("1", duplicated_lookup),
    "duplicate source values"
  )

  filtered_out <- tibble::tibble(
    dataset_id = "21",
    source_value = "1",
    harmonised_value = "a"
  )
  expect_error(
    lookup_values("1", filtered_out, dataset_id = "99"),
    "no rows after applying"
  )
})

test_that("sum_nonmissing sums observed values and keeps all-NA as NA", {
  expect_identical(
    sum_nonmissing(c(1, NA, NA), c(2, 3, NA)),
    c(3, 3, NA)
  )
  expect_identical(
    sum_nonmissing(data.frame(a = c(1, NA), b = c(NA, NA))),
    c(1, NA)
  )
  expect_error(sum_nonmissing(), "requires at least one input")
})

test_that("list_harmonisation_var_files includes the dataset template", {
  dataset_specs_dir <- withr::local_tempdir()
  template_dir <- withr::local_tempdir()
  fs::dir_create(fs::path(dataset_specs_dir, "BPIPD-9999"))
  fs::file_create(fs::path(dataset_specs_dir, "BPIPD-9999", "variables.csv"))
  fs::file_create(fs::path(template_dir, "variables.csv"))

  files <- list_harmonisation_var_files(dataset_specs_dir, template_dir)

  expect_length(files, 2)
  expect_true(any(fs::path_has_parent(files, template_dir)))

  # A missing template directory is not an error
  expect_length(
    list_harmonisation_var_files(
      dataset_specs_dir,
      fs::path(template_dir, "does-not-exist")
    ),
    1
  )
})

test_that("sync_harmonisation_vars_file adds, reorders, and reports", {
  dataschema <- tibble::tibble(
    variable_name = c("dataset_id", "dataset_name", "participant_id", "sex")
  )
  variables_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      target_variable = "sex",
      status = "compatible",
      source_columns = NA_character_,
      expression = "sex_raw",
      notes = NA_character_,
      lookup_table = NA_character_
    ),
    variables_file
  )

  result <- sync_harmonisation_vars_file(variables_file, dataschema)

  expect_identical(result$added_n, 1L)
  expect_identical(result$added_variables, "participant_id")
  expect_true(result$changed)

  synced <- readr::read_csv(variables_file, show_col_types = FALSE)
  expect_identical(synced$target_variable, c("participant_id", "sex"))
  expect_identical(synced$status, c("in_progress", "compatible"))

  # A second sync is a no-op
  rerun <- sync_harmonisation_vars_file(variables_file, dataschema)
  expect_false(rerun$changed)
})

test_that("sync_harmonisation_vars_file validates its inputs", {
  dataschema <- tibble::tibble(
    variable_name = c("dataset_id", "dataset_name", "sex")
  )
  variables_file <- withr::local_tempfile(fileext = ".csv")
  base_row <- tibble::tibble(
    target_variable = "sex",
    status = "compatible",
    source_columns = NA_character_,
    expression = "sex_raw",
    notes = NA_character_,
    lookup_table = NA_character_
  )

  expect_error(
    sync_harmonisation_vars_file(
      variables_file,
      dataschema,
      default_status = "banana"
    ),
    "`default_status` must be one of"
  )

  readr::write_csv(dplyr::bind_rows(base_row, base_row), variables_file)
  expect_error(
    sync_harmonisation_vars_file(variables_file, dataschema),
    "duplicate `target_variable`"
  )

  unknown_row <- base_row
  unknown_row$target_variable <- "not_in_schema"
  readr::write_csv(dplyr::bind_rows(base_row, unknown_row), variables_file)
  expect_error(
    sync_harmonisation_vars_file(variables_file, dataschema),
    "targets not present in dataschema"
  )

  removed <- sync_harmonisation_vars_file(
    variables_file,
    dataschema,
    remove_unknown_variables = TRUE
  )
  expect_identical(removed$removed_variables, "not_in_schema")

  # write = FALSE reports without touching the file
  readr::write_csv(base_row, variables_file)
  before <- readLines(variables_file)
  dataschema_extra <- tibble::tibble(
    variable_name = c("dataset_id", "dataset_name", "sex", "age_years")
  )
  unwritten <- sync_harmonisation_vars_file(
    variables_file,
    dataschema_extra,
    write = FALSE
  )
  expect_true(unwritten$changed)
  expect_identical(readLines(variables_file), before)
})
