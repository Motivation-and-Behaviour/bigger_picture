#' Tidier for BPIPD-677 (Przybylski)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent (single carer/adolescent survey file,
#'   no waves)
tidy_BPIPD_677 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$data_vvg_rsos)
}
