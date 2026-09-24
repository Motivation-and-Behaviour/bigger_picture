#' Tidier for BPIPD-97 (ESPAD)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per responding student per wave (each wave is an
#'   independent cross-section)
tidy_BPIPD_97 <- function(raw_dataset, spec) {
  espad <- tibble::as_tibble(raw_dataset$data[[1]])
  dplyr::filter(espad, ESPAD_Year >= 2015)
}
