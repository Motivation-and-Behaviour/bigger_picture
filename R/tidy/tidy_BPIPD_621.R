#' Tidier for BPIPD-621 (Ellis)
#'
#' One online survey, April 2020 (Ellis, Dumas & Forbes 2020, Can J Behav Sci
#' 52:177-187). "Before COVID" items are retrospective recall from the same
#' occasion as "since COVID", not a second wave, so this stays one row per
#' adolescent. No participant identifier in the file, so none is built here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent
tidy_BPIPD_621 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$data)
}
