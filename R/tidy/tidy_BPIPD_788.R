#' Tidier for BPIPD-788 (KiGGS)
#'
#' One cross-sectional participant-level file: the KiGGS Wave 2 Scientific Use
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant
tidy_BPIPD_788 <- function(raw_dataset, spec) {
  # The .sav and .dta copies hold identical values. The .sav is used because
  # Stata caps variable labels at 80 characters and drops some value labels.
  tibble::as_tibble(raw_dataset$data$`Main Dataset (SPSS Format)`)
}
