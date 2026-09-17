test_that("harmonise_from_tables derives, injects, and orders variables", {
  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "compatible",
    "id",
    "as.character(id)",
    NA,
    NA,
    "age_years",
    "compatible",
    "age",
    "age",
    NA,
    NA,
    "is_active",
    "compatible",
    "active",
    "active == 1",
    NA,
    NA
  )
  data <- tibble::tibble(id = c(1, 2), age = c(9.5, 10), active = c(1, 0))

  result <- harmonise_from_tables(
    analysis_base = data,
    spec = make_test_spec(),
    dataschema = make_test_dataschema(),
    harmonisation_config = make_test_config(variables)
  )

  expect_identical(names(result), make_test_dataschema()$variable_name)
  expect_identical(result$dataset_id, c("99", "99"))
  expect_identical(result$dataset_name, c("Test Study", "Test Study"))
  expect_identical(result$participant_id, c("1", "2"))
  expect_identical(result$age_years, c(9.5, 10))
  expect_identical(result$is_active, c(TRUE, FALSE))
})

test_that("skipped statuses produce typed NA columns", {
  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "in_progress",
    NA,
    NA,
    NA,
    NA,
    "age_years",
    "unavailable",
    NA,
    NA,
    NA,
    NA,
    "is_active",
    "incompatible",
    NA,
    NA,
    NA,
    NA
  )
  data <- tibble::tibble(id = c(1, 2))

  result <- harmonise_from_tables(
    analysis_base = data,
    spec = make_test_spec(),
    dataschema = make_test_dataschema(),
    harmonisation_config = make_test_config(variables)
  )

  expect_identical(result$participant_id, c(NA_character_, NA_character_))
  expect_identical(result$age_years, c(NA_real_, NA_real_))
  expect_identical(result$is_active, c(NA, NA))
})

test_that("length-1 results recycle and other lengths error", {
  expect_identical(recycle_to_n_rows(c(1, 2, 3), 3, "x"), c(1, 2, 3))
  expect_identical(recycle_to_n_rows(7, 3, "x"), c(7, 7, 7))
  expect_error(recycle_to_n_rows(c(1, 2), 3, "x"), "returned length 2")
})

test_that("typed_na_vector and cast_to_schema_type respect data types", {
  expect_identical(typed_na_vector("character", 2), c(NA_character_, NA))
  expect_identical(typed_na_vector("double", 1), NA_real_)
  expect_identical(typed_na_vector("integer", 1), NA_integer_)
  expect_identical(typed_na_vector("logical", 1), NA)
  expect_identical(typed_na_vector("date", 1), as.Date(NA))

  expect_identical(cast_to_schema_type(1L, "character"), "1")
  expect_identical(cast_to_schema_type("2.5", "double"), 2.5)
  expect_identical(cast_to_schema_type("1", "integer"), 1L)
  expect_identical(cast_to_schema_type("TRUE", "logical"), TRUE)
  expect_identical(
    cast_to_schema_type("2026-01-31", "date"),
    as.Date("2026-01-31")
  )
})

test_that("a schema variable without a mapping row errors", {
  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "compatible",
    "id",
    "as.character(id)",
    NA,
    NA
  )

  expect_error(
    harmonise_from_tables(
      analysis_base = tibble::tibble(id = 1),
      spec = make_test_spec(),
      dataschema = make_test_dataschema(),
      harmonisation_config = make_test_config(variables)
    ),
    "No mapping row found for `age_years`"
  )
})

test_that("expressions returning multi-column data frames error", {
  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "compatible",
    NA,
    "tbl",
    NA,
    NA,
    "age_years",
    "in_progress",
    NA,
    NA,
    NA,
    NA,
    "is_active",
    "in_progress",
    NA,
    NA,
    NA,
    NA
  )

  expect_error(
    harmonise_from_tables(
      analysis_base = tibble::tibble(id = 1, age = 9),
      spec = make_test_spec(),
      dataschema = make_test_dataschema(),
      harmonisation_config = make_test_config(variables)
    ),
    "one-column data frame"
  )
})

test_that("expressions cannot reach the global environment", {
  assign("leaked_helper", function(x) x * 2, envir = globalenv())
  withr::defer(rm("leaked_helper", envir = globalenv()))

  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "in_progress",
    NA,
    NA,
    NA,
    NA,
    "age_years",
    "compatible",
    "age",
    "leaked_helper(age)",
    NA,
    NA,
    "is_active",
    "in_progress",
    NA,
    NA,
    NA,
    NA
  )

  expect_error(
    harmonise_from_tables(
      analysis_base = tibble::tibble(age = 9),
      spec = make_test_spec(),
      dataschema = make_test_dataschema(),
      harmonisation_config = make_test_config(variables)
    ),
    "could not find function"
  )
})

test_that("allowlisted functions work without dplyr attached", {
  expect_false("package:dplyr" %in% search())

  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "compatible",
    "sex",
    'recode_values(sex, 1 ~ "Male", 2 ~ "Female")',
    NA,
    NA,
    "age_years",
    "compatible",
    "age",
    "if_else(age > 9, age, NA)",
    NA,
    NA,
    "is_active",
    "in_progress",
    NA,
    NA,
    NA,
    NA
  )
  data <- tibble::tibble(sex = c(1, 2), age = c(9, 10))

  result <- harmonise_from_tables(
    analysis_base = data,
    spec = make_test_spec(),
    dataschema = make_test_dataschema(),
    harmonisation_config = make_test_config(variables)
  )

  expect_identical(result$participant_id, c("Male", "Female"))
  expect_identical(result$age_years, c(NA_real_, 10))
})

test_that("lookup tables are bound as lookup_<name> in expressions", {
  lookups <- list(
    sex_map = tibble::tibble(
      source_value = c("m", "f"),
      harmonised_value = c("Male", "Female")
    )
  )
  variables <- make_test_variables(
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    ~notes,
    ~lookup_table,
    "participant_id",
    "compatible",
    "sex",
    "lookup_values(sex, lookup_sex_map)",
    NA,
    "sex_map",
    "age_years",
    "in_progress",
    NA,
    NA,
    NA,
    NA,
    "is_active",
    "in_progress",
    NA,
    NA,
    NA,
    NA
  )

  result <- harmonise_from_tables(
    analysis_base = tibble::tibble(sex = c("f", "m")),
    spec = make_test_spec(),
    dataschema = make_test_dataschema(),
    harmonisation_config = make_test_config(variables, lookups)
  )

  expect_identical(result$participant_id, c("Female", "Male"))
})

test_that("harmonisation_expression_calls collects call-position names", {
  expect_identical(
    sort(harmonisation_expression_calls(
      'recode_values(haven::as_factor(x), "a" ~ "b")'
    )),
    sort(c("recode_values", "haven::as_factor", "~"))
  )
  expect_identical(
    harmonisation_expression_calls("age"),
    character(0)
  )
})
