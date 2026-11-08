#' Tidier for BPIPD-2329 (Koch)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child at the single measurement occasion
tidy_BPIPD_2329 <- function(raw_dataset, spec) {
  # The release is already the n = 89 analysis sample of Koch et al. (2024).
  tibble::as_tibble(raw_dataset$data$koch_data)
}
