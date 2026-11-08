#' Tidier for BPIPD-1380 (Buja)
#'
#' One cross-sectional mother-reported survey of fifth-grade children
#' (Veneto, Italy, March-May 2023) delivered as a single spreadsheet.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child, keyed on `B_ID`
tidy_BPIPD_1380 <- function(raw_dataset, spec) {
  db <- tibble::as_tibble(raw_dataset$data$database)

  # The sheet ends in an unnamed, empty column that `readxl` names by position.
  db <- db[, names(db) != "...307", drop = FALSE]

  # "-" is the study's only missing marker and appears in 295 of the 306 columns.
  db <- dplyr::mutate(
    db,
    dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(.x, "-"))
  )

  # `B_Codice_Identificativo` repeats between classes, so `B_ID` is the key.
  if (anyDuplicated(db$B_ID) > 0) {
    stop("BPIPD-1380: `B_ID` does not uniquely identify rows.", call. = FALSE)
  }

  db
}
