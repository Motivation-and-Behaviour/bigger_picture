#' Tidier for BPIPD-160 (Topic)
#'
#' A single cross-sectional SPSS extract with no participant identifier and no
#' wave column, so neither is constructed here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child
tidy_BPIPD_160 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data[[1]])

  # `father_age` holds ages in years (30-58) but carries `father_work`'s value
  # labels (1 = employed, 2 = unemployed); only the labels are dropped.
  df$father_age <- haven::zap_labels(df$father_age)

  df
}
