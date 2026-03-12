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
tidy_BPIPD_TEMPLATE <- function(raw_dataset, spec) {
  if (length(raw_dataset$data) != 1L) {
    stop(
      "Replace the template tidier with study-specific code that returns one tibble.",
      call. = FALSE
    )
  }

  tibble::as_tibble(raw_dataset$data[[1]])
}
