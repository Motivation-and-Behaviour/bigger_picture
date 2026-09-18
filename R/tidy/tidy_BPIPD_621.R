#' Tidier for BPIPD-621 (Ellis)
#'
#' One online survey run in April 2020 (Ellis, Dumas & Forbes, 2020, Can J
#' Behav Sci 52:177-187): the "before COVID" items are retrospective recall
#' collected on the same occasion as the "since COVID" items, not a second
#' wave, so the file stays one row per adolescent. It carries no participant
#' identifier, so none is constructed here.
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
