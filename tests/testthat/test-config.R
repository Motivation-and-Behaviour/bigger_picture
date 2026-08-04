test_that("dataset directory pattern matches the naming convention", {
  pattern <- bp_dataset_dir_pattern()

  expect_true(grepl(pattern, "BPIPD-21 - Przybylski"))
  expect_true(grepl(pattern, "BPIPD-1528 - HBSC (multi-wave)"))
  expect_false(grepl(pattern, "BPIPD-21-Przybylski"))
  expect_false(grepl(pattern, "bpipd-21 - Przybylski"))
  expect_false(grepl(pattern, "BPIPD- - Missing ID"))
})

test_that("harmonisation status values are stable", {
  expect_setequal(
    bp_harmonisation_status_values(),
    c(
      "compatible",
      "partial",
      "proximate",
      "incompatible",
      "unavailable",
      "inferred",
      "in_progress"
    )
  )
  expect_setequal(
    bp_system_schema_variables(),
    c("dataset_id", "dataset_name")
  )
})

test_that("bp_data_dir errors clearly when DATASET_PATH is unset", {
  # Run from a temp dir with an empty .env so no real configuration leaks
  # in; this documents that the test suite never needs the data mount.
  temp_dir <- withr::local_tempdir()
  writeLines(character(0), fs::path(temp_dir, ".env"))
  withr::local_dir(temp_dir)
  withr::local_envvar(DATASET_PATH = NA)

  expect_error(bp_data_dir(), "DATASET_PATH environment variable is not set")
})

test_that("bp_schema declares the required core contract", {
  schema <- bp_schema()

  expect_true(all(schema$required %in% names(schema$types)))
  expect_true(all(c("dataset_id", "participant_id") %in% schema$required))
})
