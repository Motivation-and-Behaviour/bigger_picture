#' Tidier for BPIPD-788 (KiGGS)
#'
#' KiGGS Wave 2 Scientific Use File: one cross-sectional participant-level file.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant
tidy_BPIPD_788 <- function(raw_dataset, spec) {
  # .sav and .dta hold identical values; .sav used because Stata caps variable
  # labels at 80 chars and drops some value labels.
  tibble::as_tibble(raw_dataset$data$`Main Dataset (SPSS Format)`)
}
