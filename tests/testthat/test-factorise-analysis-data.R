make_analysis_data <- function() {
  tibble::tibble(
    dataset_id = c("2", "10", "2", "10", "2"),
    participant_id = c("a1", "a2", "a3", "a4", "a5"),
    sex = c("Male", NA, "Female", "male", "Female"),
    age_years = c(10, 11, NA, 13, 14),
    is_active = c(TRUE, FALSE, NA, TRUE, TRUE),
    st_measure_id = factor(c("primary", "diary", "primary", "primary", "diary"))
  )
}

test_that("low-cardinality character columns become factors of observed values", {
  data <- make_analysis_data()

  out <- factorise_analysis_data(data, max_levels = 3)

  expect_s3_class(out$sex, "factor")
  expect_identical(levels(out$sex), c("Female", "Male", "male"))
  expect_identical(levels(out$dataset_id), c("10", "2"))
})

test_that("factorising never changes a value or introduces NA", {
  data <- make_analysis_data()

  out <- factorise_analysis_data(data, max_levels = 3)

  for (column in c("dataset_id", "sex")) {
    expect_identical(as.character(out[[column]]), data[[column]])
    expect_identical(is.na(out[[column]]), is.na(data[[column]]))
  }
})

test_that("columns with more than max_levels distinct values stay character", {
  data <- make_analysis_data()

  out <- factorise_analysis_data(data, max_levels = 3)
  expect_identical(out$participant_id, data$participant_id)

  # NA is not counted as a distinct value.
  at_limit <- factorise_analysis_data(data["sex"], max_levels = 3)
  expect_s3_class(at_limit$sex, "factor")
  over_limit <- factorise_analysis_data(data["sex"], max_levels = 2)
  expect_identical(over_limit$sex, data$sex)
})

test_that("levels use byte order whatever the locale", {
  data <- tibble::tibble(x = c("b", "B", "a", "A", "_"))

  out <- factorise_analysis_data(data)

  expect_identical(levels(out$x), c("A", "B", "_", "a", "b"))
})

test_that("the same string in different encodings is one level, never NA", {
  utf8 <- "café"
  latin1 <- iconv(utf8, "UTF-8", "latin1")
  data <- tibble::tibble(x = c(utf8, latin1, NA))

  out <- factorise_analysis_data(data)

  expect_identical(nlevels(out$x), 1L)
  expect_identical(is.na(out$x), c(FALSE, FALSE, TRUE))
  expect_identical(as.character(out$x), c(utf8, utf8, NA))
})

test_that("non-character columns are left untouched", {
  data <- make_analysis_data()

  out <- factorise_analysis_data(data)

  expect_identical(out$age_years, data$age_years)
  expect_identical(out$is_active, data$is_active)
  expect_identical(out$st_measure_id, data$st_measure_id)
})

test_that("an all-NA character column becomes a zero-level factor", {
  data <- tibble::tibble(x = rep(NA_character_, 3))

  out <- factorise_analysis_data(data)

  expect_s3_class(out$x, "factor")
  expect_identical(levels(out$x), character())
  expect_true(all(is.na(out$x)))
})

test_that("column order, row count and class are preserved", {
  data <- make_analysis_data()

  out <- factorise_analysis_data(data)

  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), names(data))
  expect_identical(nrow(out), nrow(data))
})

test_that("zero-row and zero-column inputs work", {
  zero_rows <- make_analysis_data()[0, ]
  out <- factorise_analysis_data(zero_rows)
  expect_identical(names(out), names(zero_rows))
  expect_identical(nrow(out), 0L)
  expect_s3_class(out$sex, "factor")

  expect_identical(
    factorise_analysis_data(tibble::tibble()),
    tibble::tibble()
  )
})

test_that("factor levels and values survive a Parquet round trip", {
  skip_if_not_installed("arrow")
  data <- factorise_analysis_data(make_analysis_data())
  path <- withr::local_tempfile(fileext = ".parquet")

  arrow::write_parquet(data, path)
  out <- arrow::read_parquet(path)

  expect_identical(out, data)
})
