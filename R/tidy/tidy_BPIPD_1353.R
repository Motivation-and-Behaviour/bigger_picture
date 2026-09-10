#' Tidier for BPIPD-1353 (Korea Youth Risk Behavior Web-based Survey)
#'
#' KYRBS is a repeated cross-section, not a panel: each of the 21 annual waves
#' (2005-2025) is an independent stratified cluster sample.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent per year
tidy_BPIPD_1353 <- function(raw_dataset, spec) {
  df <- dplyr::bind_rows(raw_dataset$data)

  dplyr::mutate(
    df,
    participant_id = paste0(.wave, "_", OBS),
    cluster_id = paste0(.wave, "_", CLUSTER),
    .before = 1
  )
}
