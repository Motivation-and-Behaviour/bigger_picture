make_io_dataschema <- function() {
  tibble::tibble(
    variable_name = c("dataset_id", "dataset_name", "participant_id", "sex"),
    data_type = c("character", "character", "character", "character")
  )
}

make_io_variables <- function(
  target_variable = c("participant_id", "sex"),
  status = c("compatible", "compatible"),
  expression = c("id", "sex_raw"),
  lookup_table = c(NA_character_, NA_character_)
) {
  tibble::tibble(
    target_variable = target_variable,
    status = status,
    source_columns = NA_character_,
    expression = expression,
    notes = NA_character_,
    lookup_table = lookup_table
  )
}

test_that("valid variables pass validation", {
  expect_no_error(
    validate_harmonisation_vars(
      make_io_variables(),
      make_io_dataschema(),
      character(0)
    )
  )
})

test_that("validate_harmonisation_vars rejects bad inputs", {
  dataschema <- make_io_dataschema()

  expect_error(
    validate_harmonisation_vars(
      make_io_variables()[0, ],
      dataschema,
      character(0)
    ),
    "at least one row"
  )
  expect_error(
    validate_harmonisation_vars(
      make_io_variables(status = c("compatible", "banana")),
      dataschema,
      character(0)
    ),
    "Unknown `status` values.*banana"
  )
  expect_error(
    validate_harmonisation_vars(
      make_io_variables(target_variable = c("sex", "sex")),
      dataschema,
      character(0)
    ),
    "Duplicate `target_variable`"
  )
  expect_error(
    validate_harmonisation_vars(
      make_io_variables(target_variable = c("participant_id", "not_in_schema")),
      dataschema,
      character(0)
    ),
    "targets not present in dataschema"
  )
  expect_error(
    validate_harmonisation_vars(
      make_io_variables()[1, ],
      dataschema,
      character(0)
    ),
    "missing schema variables: sex"
  )
  expect_error(
    validate_harmonisation_vars(
      make_io_variables(expression = c("id", NA)),
      dataschema,
      character(0)
    ),
    "must provide an `expression`.*sex"
  )
  expect_error(
    validate_harmonisation_vars(
      make_io_variables(lookup_table = c(NA, "missing_lookup")),
      dataschema,
      character(0)
    ),
    "lookup tables that do not exist.*missing_lookup"
  )
})

make_shared_lookup <- function() {
  tibble::tibble(
    dataset_id = c("21", "21"),
    target_variable = c("sex", "sex"),
    source_value = c("1", "2"),
    harmonised_value = c("Male", "Female"),
    mapping_status = c("compatible", "compatible"),
    notes = c(NA_character_, NA_character_)
  )
}

test_that("validate_shared_lookup_table enforces the shared schema", {
  expect_no_error(validate_shared_lookup_table(make_shared_lookup(), "sex"))

  missing_col <- make_shared_lookup()
  missing_col$mapping_status <- NULL
  expect_error(
    validate_shared_lookup_table(missing_col, "sex"),
    "missing required columns: mapping_status"
  )

  empty_source <- make_shared_lookup()
  empty_source$source_value[1] <- NA
  expect_error(
    validate_shared_lookup_table(empty_source, "sex"),
    "empty `source_value`"
  )

  bad_status <- make_shared_lookup()
  bad_status$mapping_status[1] <- "banana"
  expect_error(
    validate_shared_lookup_table(bad_status, "sex"),
    "unknown `mapping_status`.*banana"
  )

  duplicated_rows <- make_shared_lookup()
  duplicated_rows$source_value <- c("1", "1")
  expect_error(
    validate_shared_lookup_table(duplicated_rows, "sex"),
    "duplicate dataset/variable/source rows"
  )
})

test_that("merge_lookup_tables rejects duplicate lookup names", {
  shared <- list(sex = make_shared_lookup())
  dataset <- list(sex = make_shared_lookup())

  expect_error(
    merge_lookup_tables(shared, dataset, dataset_id = "21"),
    "duplicated between shared and dataset-specific"
  )
  expect_named(
    merge_lookup_tables(shared, list(other = make_shared_lookup()), "21"),
    c("sex", "other")
  )
})

test_that("read_harmonisation_variables requires the expected columns", {
  variables_file <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(target_variable = "sex", status = "compatible"),
    variables_file
  )

  expect_error(
    read_harmonisation_variables(variables_file),
    "missing required columns"
  )
})
