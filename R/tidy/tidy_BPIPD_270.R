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
tidy_BPIPD_270 <- function(raw_dataset, spec) {
  # The rda files seem to be the most complete
  rda_names <- grep("_rda$", names(raw_dataset$data), value = TRUE)
  rda_list <- raw_dataset$data[rda_names]

  # TODO: These need to be full-joined by wave and then row bound
  combined_df <- dplyr::bind_rows(rda_list, .id = "source")
  combined_df
}
