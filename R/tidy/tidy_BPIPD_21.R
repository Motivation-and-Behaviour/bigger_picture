#' Tidier for BPIPD-21 (Przybylski)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent
tidy_BPIPD_21 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data[[1]])
}
