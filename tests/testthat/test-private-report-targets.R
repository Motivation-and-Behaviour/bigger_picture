test_that("no private report folder means no report targets, silently", {
  withr::local_dir(withr::local_tempdir())

  expect_no_warning(out <- private_report_targets())
  expect_identical(out, list())
})

test_that("a report folder without targets.R warns and adds nothing", {
  withr::local_dir(withr::local_tempdir())
  dir.create("DoEreport")

  expect_warning(out <- private_report_targets(), "has no targets.R")
  expect_identical(out, list())
})

test_that("a broken report link warns and adds nothing", {
  withr::local_dir(withr::local_tempdir())
  file.symlink("missing-checkout", "DoEreport")

  expect_warning(out <- private_report_targets(), "broken link")
  expect_identical(out, list())
})

test_that("the report's targets.R supplies the report targets", {
  withr::local_dir(withr::local_tempdir())
  dir.create("DoEreport")
  writeLines("list(targets::tar_target(x, 1))", "DoEreport/targets.R")

  out <- private_report_targets()
  expect_length(out, 1)
  expect_identical(out[[1]]$settings$name, "x")
})
