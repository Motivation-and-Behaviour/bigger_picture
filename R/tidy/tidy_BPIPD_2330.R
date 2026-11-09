#' Tidier for BPIPD-2330 (Holmboe)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per infant at the single 10-month assessment
tidy_BPIPD_2330 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  # Parents who declined data sharing (code "2") left only the random ID (data note).
  df <- df[haven::zap_labels(df$Data_Sharing) %in% "1", , drop = FALSE]

  if (anyDuplicated(df$Random_ID) > 0) {
    stop(
      "BPIPD-2330: `Random_ID` does not uniquely identify rows.",
      call. = FALSE
    )
  }
  df
}
