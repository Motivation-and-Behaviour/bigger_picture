#' Tidier for BPIPD-1353 (Korea Youth Risk Behavior Web-based Survey)
#'
#' Repeated cross-section, not a panel: 21 independent annual stratified
#' cluster samples (2005-2025).
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent per year
tidy_BPIPD_1353 <- function(raw_dataset, spec) {
  df <- dplyr::bind_rows(raw_dataset$data)

  # Grain and `participant_id` use the reader's `.wave`; the mapping derives
  # `wave` and `data_year` from `YEAR`, so the two must agree.
  if (any(as.character(df$YEAR) != df$.wave)) {
    stop("BPIPD-1353: `YEAR` disagrees with `.wave`.", call. = FALSE)
  }

  # `OBS` is unique only within a wave and `CLUSTER` restarts at 1 each wave,
  # so both need a wave prefix to be unique dataset-wide.
  df <- dplyr::mutate(
    df,
    participant_id = paste0(.wave, "_", OBS),
    cluster_id = paste0(.wave, "_", CLUSTER),
    .before = 1
  )

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-1353: `participant_id` is not unique.", call. = FALSE)
  }
  attr(
    df$participant_id,
    "label"
  ) <- "Wave-prefixed respondent serial (<year>_<OBS>)"
  attr(
    df$cluster_id,
    "label"
  ) <- "Wave-prefixed sampling cluster (<year>_<CLUSTER>)"

  df
}
