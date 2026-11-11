# The deviations ledger (ANALYSIS_PLAN.md, Step 0): every entry is complete,
# rated, and points at decision records that exist.

test_that("deviations.csv entries are complete and rated", {
  ledger <- read_registry_csv(
    file.path(repo_root, bp_deviations_path()),
    bp_deviations_columns()
  )

  expect_gt(nrow(ledger), 0)
  expect_false(anyDuplicated(ledger$id) > 0)
  expect_true(all(grepl("^D[0-9]{2,}$", ledger$id)))
  expect_false(anyNA(as.Date(ledger$date_logged, format = "%Y-%m-%d")))
  expect_true(all(ledger$status %in% bp_deviation_statuses()))
  expect_true(all(ledger$validity %in% bp_deviation_ratings()))
  expect_true(all(ledger$severity %in% bp_deviation_ratings()))

  required_text <- c("prereg_plan", "deviation", "reason", "assessment")
  for (column in required_text) {
    expect_false(anyNA(ledger[[column]]), info = column)
  }
})

test_that("deviations.csv cites decision records that exist", {
  ledger <- read_registry_csv(
    file.path(repo_root, bp_deviations_path()),
    bp_deviations_columns()
  )
  cited <- unlist(strsplit(
    stats::na.omit(ledger$decision_record),
    "|",
    fixed = TRUE
  ))
  on_disk <- sub(
    "^(DR-[0-9]{4}).*$",
    "\\1",
    list.files(
      file.path(repo_root, "docs", "decisions"),
      pattern = "^DR-[0-9]{4}"
    )
  )
  expect_true(all(cited %in% on_disk))
})
