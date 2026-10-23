#' Tidier for BPIPD-1249 (Gentile, 2009)
#'
#' One cross-sectional Harris Interactive online survey of US youth aged 8-18
#' (January 2007); a single SPSS file with no participant id, so none is built.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per youth respondent
tidy_BPIPD_1249 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data[[1]])
}
