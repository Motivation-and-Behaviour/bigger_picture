#' Tidier for BPIPD-1714 (Alloway)
#'
#' One cross-sectional SPSS file, already one row per child, returned as is.
#' The `Fic*`, `Pic*`, `Fact*` and `Edu*` columns are book reading, not screens.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child, keyed on `Prticpnt`
tidy_BPIPD_1714 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$data)
}
