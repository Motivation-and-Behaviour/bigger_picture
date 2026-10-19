#' Tidier for BPIPD-337 (Tezol)
#'
#' A single cross-sectional SPSS extract of 210 mother-child pairs, so there is
#' nothing to join, stack or pivot; `surveynumber` already identifies a row.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child (mother-reported, single survey round)
tidy_BPIPD_337 <- function(raw_dataset, spec) {
  tibble::as_tibble(raw_dataset$data$data)
}
