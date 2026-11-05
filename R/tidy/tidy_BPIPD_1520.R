#' Tidier for BPIPD-1520 (Yilmaz)
#'
#' A two-country cross-section: primary schoolchildren in Nottingham, England
#' (2016-17) and Ankara, Turkey (2017-18), surveyed once each.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child, with the two samples stacked
tidy_BPIPD_1520 <- function(raw_dataset, spec) {
  # The `_Updated` workbooks are the originals plus `Social Prob. Solv. Score`.
  # The device items were only put to the Turkish sample.
  df <- dplyr::bind_rows(
    British = tibble::as_tibble(raw_dataset$data$data_british_sample_updated),
    Turkish = tibble::as_tibble(raw_dataset$data$data_turkish_sample_updated),
    .id = "sample"
  )

  # `Participant No` restarts at 1 in each sample (British 1-255, Turkish
  # 1-268), so the sample has to be part of the key.
  df <- dplyr::mutate(
    df,
    participant_id = paste0(sample, "_", `Participant No`),
    .before = 1
  )

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-1520: `participant_id` is not unique.", call. = FALSE)
  }
  attr(df$participant_id, "label") <- "Sample-prefixed serial (<sample>_<no>)"
  attr(df$sample, "label") <- "Country sample the child was surveyed in"

  df
}
