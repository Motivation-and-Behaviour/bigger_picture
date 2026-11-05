#' Tidier for BPIPD-845 (Anuroj)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child (a single cross-sectional guardian-reported
#'   survey file; the study has no waves)
tidy_BPIPD_845 <- function(raw_dataset, spec) {
  # Already the published analytic sample of 528 guardians: the nine
  # screen-time outliers the paper excluded are not in the file.
  tibble::as_tibble(raw_dataset$data$data)
}
