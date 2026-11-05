#' Tidier for BPIPD-2395 (Tardif-Grenier)
#'
#' One SPSS file from a single April 2020 online survey, already one row per
#' adolescent with a unique `id`.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent (single cross-section)
tidy_BPIPD_2395 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$data)
}
