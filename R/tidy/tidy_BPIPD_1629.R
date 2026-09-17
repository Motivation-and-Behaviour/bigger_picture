#' Tidier for BPIPD-1629 (EU Kids Online)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child (a 9-16 year-old internet user), carrying
#'   both the child (`QC*`) and the accompanying parent (`QP*`) interview
tidy_BPIPD_1629 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$`Primary Dataset`)
}
