#' Tidier for BPIPD-1713 (Antoniadou)
#'
#' One cross-sectional SPSS file of Greek junior high school students; nothing
#' to join, stack, or pivot, and `AA` is already unique across the 1,097 rows.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant, keyed on `AA`
tidy_BPIPD_1713 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$data)
}
