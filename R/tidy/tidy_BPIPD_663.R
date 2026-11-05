#' Tidier for BPIPD-663 (Șipoș)
#'
#' One Excel sheet: adolescent self-report (`C_*`, `MASC *`, `YSR *`) and the
#' caregiver's report on the same child (`P_*`, `ADHD-RS *`, `CBCL *`) share a row.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent (single occasion), keyed on `#`
tidy_BPIPD_663 <- function(raw_dataset, spec) {
  db <- tibble::as_tibble(raw_dataset$data$data)

  # `Date` holds two encodings: Excel serials (41 rows) and dd.mm.yyyy text (56).
  is_serial <- grepl("^[0-9]+$", db$Date)
  parsed <- as.Date(db$Date, format = "%d.%m.%Y")
  parsed[is_serial] <- as.Date(
    as.numeric(db$Date[is_serial]),
    origin = "1899-12-30"
  )
  attr(parsed, "label") <- "Assessment date, parsed from `Date`"

  db$assessment_date <- parsed
  dplyr::relocate(db, "assessment_date", .after = "Date")
}
