#' Tidier for BPIPD-161 (Taylor)
#'
#' One cross-sectional parent-report survey of UK children aged 6-36 months,
#' released as a single cleaned CSV, so there is nothing to join or stack.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child, keyed on the Qualtrics response id `V1`
tidy_BPIPD_161 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data[["BMQ Clean Data"]])

  # The reader names the CSV's unnamed exported row-name column `...1`. `V1` is
  # the Qualtrics response id, already unique, so no `participant_id` is built.
  dplyr::rename(df, source_row = "...1")
}
