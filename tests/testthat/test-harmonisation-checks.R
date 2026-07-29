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
      "suggestion",
      "fixed"
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

test_that("check_harmonisation_exprs flags evaluated rows with no expression", {
  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~source_columns,
    ~expression,
    "age_years",
    "compatible",
    "age_raw",
    NA_character_,
    "sex",
    "in_progress",
    NA_character_,
    NA_character_
  )

  issues <- check_harmonisation_exprs(variables_file, check_format = FALSE)

  expect_identical(issues$target_variable, "age_years")
  expect_identical(issues$issue, "missing_expression")

  # Still reported when drafts are included, not doubled up
  expect_identical(
    nrow(check_harmonisation_exprs(
      variables_file,
      active_only = FALSE,
      check_format = FALSE
    )),
    1L
  )
})

test_that("check_harmonisation_exprs fixes formatting in place", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~notes,
    ~expression,
    "participant_id",
    "compatible",
    "leave, me alone",
    "as.character( serial )",
    "age_years",
    "compatible",
    NA_character_,
    "as.numeric(age_raw)"
  )

  issues <- check_harmonisation_exprs(variables_file, fix = TRUE)

  expect_identical(issues$issue, "unformatted")
  expect_true(issues$fixed)

  rewritten <- readr::read_csv(variables_file, show_col_types = FALSE)
  expect_identical(
    rewritten$expression[rewritten$target_variable == "participant_id"],
    "as.character(serial)"
  )
  # Untouched rows and columns survive the rewrite intact
  expect_identical(
    rewritten$expression[rewritten$target_variable == "age_years"],
    "as.numeric(age_raw)"
  )
  expect_identical(
    rewritten$notes[rewritten$target_variable == "participant_id"],
    "leave, me alone"
  )

  # Re-running finds nothing left to fix
  expect_identical(
    nrow(check_harmonisation_exprs(variables_file, fix = TRUE)),
    0L
  )
})

test_that("check_harmonisation_exprs fix leaves unfixable issues alone", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  variables_file <- withr::local_tempfile(fileext = ".csv")
  write_variables_csv(
    variables_file,
    ~target_variable,
    ~status,
    ~expression,
    "participant_id",
    "compatible",
    "not_allowed( serial )"
  )

  issues <- check_harmonisation_exprs(variables_file, fix = TRUE)

  expect_setequal(issues$issue, c("unknown_function", "unformatted"))
  expect_false(issues$fixed[issues$issue == "unknown_function"])
  expect_true(issues$fixed[issues$issue == "unformatted"])
})

test_that("check_harmonisation_exprs refuses to fix without a formatter", {
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

  expect_error(
    suppressWarnings(check_harmonisation_exprs(
      variables_file,
      fix = TRUE,
      air_path = ""
    )),
    "needs `check_format = TRUE`"
  )
})

test_that("line_width controls how expressions are wrapped", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  expression <- paste0(
    "recode_values(tvwd, 1 ~ 0, 2 ~ 0.5, 3 ~ 1, 4 ~ 2, 5 ~ 3, 6 ~ 4, ",
    "7 ~ 5, 8 ~ 6, 9 ~ 7)"
  )
  expect_gt(nchar(expression), 80)
  expect_lt(nchar(expression), 120)

  wide <- format_r_expression(
    expression,
    config_dir = air_expression_config_dir(line_width = 120)
  )
  narrow <- format_r_expression(
    expression,
    config_dir = air_expression_config_dir(line_width = 80)
  )

  expect_identical(wide, expression)
  expect_gt(length(strsplit(narrow, "\n")[[1]]), 1L)
})

test_that("existing line breaks do not keep an expression expanded", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  # A break straight after `(` would normally pin the call open; the
  # expression config turns that off so width alone decides.
  formatted <- format_r_expression(
    "recode_values(\n  sleepdifficulty,\n  1 ~ \"Daily\"\n)",
    config_dir = air_expression_config_dir()
  )

  expect_identical(formatted, "recode_values(sleepdifficulty, 1 ~ \"Daily\")")
})

test_that("bp_air_path finds Air outside the terminal PATH", {
  # Air lives in a VS Code extension directory that only the integrated
  # terminal puts on the PATH, so the lookup must not rely on Sys.which().
  withr::with_path(c("/usr/bin", "/bin"), action = "replace", {
    skip_if(nzchar(Sys.which("air")), "Air is on the bare PATH here")
    air <- bp_air_path()
    skip_if(!nzchar(air), "Air is not installed in a known location")
    expect_identical(file.access(air, mode = 1)[[1]], 0L)
  })
})

test_that("format_r_expression returns NULL for unparseable input", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  expect_null(format_r_expression("as.character(serial"))
  expect_identical(
    format_r_expression("as.character( serial )"),
    "as.character(serial)"
  )
})
