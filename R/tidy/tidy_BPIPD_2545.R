#' Tidier for BPIPD-2545 (Marques, 2014)
#'
#' One SPSS file from a single cross-sectional school survey of 802 students
#' aged 10-12; the release has no participant id.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student
tidy_BPIPD_2545 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  # `xxxxxxxxxx` is an undefined placeholder that is empty in every row.
  df <- df[, names(df) != "xxxxxxxxxx", drop = FALSE]

  # `filter_$` is an SPSS working selection (`Pratica_rec` == 1), not an
  # exclusion flag, so every student is kept.
  df
}
