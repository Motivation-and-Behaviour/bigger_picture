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

  # The grain and `participant_id` come from the reader's `.wave`, while the
  # mapping derives `wave` and `data_year` from the survey's own `YEAR`.
  if (any(as.character(df$YEAR) != df$.wave)) {
    stop("BPIPD-1353: `YEAR` disagrees with `.wave`.", call. = FALSE)
  }

  # `OBS` and `CLUSTER` are numbered within a survey year (`OBS` is unique only
  # inside a wave; `CLUSTER` restarts at 1 in every wave).
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
