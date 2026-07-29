write_variables_csv <- function(path, ...) {
  readr::write_csv(make_test_variables(...), path, na = "")
  path
}

test_that("check_harmonisation_exprs reports parse and lookup errors", {
  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~expression,
    "participant_id",
    "compatible",
    "as.character(serial",
    "age_years",
    "compatible",
    "not_allowed(age_raw)",
    "sex",
    "compatible",
    "haven::as_factor(sex_raw)"
  )

  issues <- check_harmonisation_exprs(
    variables_file,
    check_format = FALSE
  )

  expect_identical(nrow(issues), 2L)
  expect_identical(
    issues$issue[issues$target_variable == "participant_id"],
    "parse_error"
  )
  expect_identical(
    issues$issue[issues$target_variable == "age_years"],
    "unknown_function"
  )
  # A namespaced call to an installed package is fine
  expect_false("sex" %in% issues$target_variable)
})

test_that("check_harmonisation_exprs returns an empty tibble when clean", {
  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~expression,
    "participant_id",
    "compatible",
    "as.character(serial)"
  )

  issues <- check_harmonisation_exprs(
    variables_file,
    check_format = FALSE
  )

  expect_identical(nrow(issues), 0L)
  expect_named(
    issues,
    c(
      "variables_file",
      "target_variable",
      "status",
      "issue",
      "message",
      "suggestion"
    )
  )
})

test_that("check_harmonisation_exprs skips non-evaluated rows", {
  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~expression,
    "age_years",
    "in_progress",
    "still_drafting(",
    "sex",
    "unavailable",
    "also_broken("
  )

  expect_identical(
    nrow(check_harmonisation_exprs(variables_file, check_format = FALSE)),
    0L
  )

  # ...but they can be opted into while drafting
  expect_identical(
    nrow(check_harmonisation_exprs(
      variables_file,
      active_only = FALSE,
      check_format = FALSE
    )),
    2L
  )
})

test_that("check_harmonisation_exprs warns when Air is unavailable", {
  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~expression,
    "participant_id",
    "compatible",
    "as.character(serial)"
  )

  expect_warning(
    check_harmonisation_exprs(variables_file, air_path = ""),
    "Air was not found"
  )
})

test_that("check_harmonisation_exprs flags unformatted expressions", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~expression,
    "participant_id",
    "compatible",
    "as.character( serial )"
  )

  issues <- check_harmonisation_exprs(variables_file)

  expect_identical(issues$issue, "unformatted")
  expect_identical(issues$suggestion, "as.character(serial)")
})

test_that("format_r_expression returns NULL for unparseable input", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  expect_null(format_r_expression("as.character(serial"))
  expect_identical(
    format_r_expression("as.character( serial )"),
    "as.character(serial)"
  )
})
