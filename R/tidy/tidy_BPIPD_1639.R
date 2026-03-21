#' Template tidier for pre-harmonisation dataset shaping
#'
#' Use this step for study-specific table assembly before harmonisation, for
#' example joining multiple raw files, binding waves, filtering records, or
#' reshaping raw tables into one canonical analysis tibble.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble to be used as the harmonisation input
tidy_BPIPD_1639 <- function(raw_dataset, spec) {
  df <- raw_dataset$data$demographics |>
    dplyr::full_join(raw_dataset$data$questionnaire, by = "personid") |>
    dplyr::full_join(raw_dataset$data$self_control, by = "personid")
  df
}
