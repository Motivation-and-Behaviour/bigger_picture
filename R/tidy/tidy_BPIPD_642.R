#' Tidier for BPIPD-642 (Rose)
#'
#' One SPSS file of parent-reported TV viewing and CBQ Very Short Form
#' temperament scores; it carries no participant identifier.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child (single timepoint)
tidy_BPIPD_642 <- function(raw_dataset, spec) {
  tbl <- tibble::as_tibble(raw_dataset$data[[1]])

  # The .sav ends with one row that is empty in every column; the study reports
  # 41 children (Rose, Lamont & Reyland, 2022, Media Psychology 25(2)).
  tbl[rowSums(!is.na(tbl)) > 0L, ]
}
