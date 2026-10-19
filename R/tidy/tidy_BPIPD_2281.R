#' Tidier for BPIPD-2281 (Grao-Cruces)
#'
#' One cross-section of Andalusian adolescents, delivered as the same rows in
#' `.sav` and `.xlsx`.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent, keyed on `ID`
tidy_BPIPD_2281 <- function(raw_dataset, spec) {
  # .sav carries the labels; the .xlsx twin writes missing values as `#NULL!`.
  tibble::as_tibble(raw_dataset$data$`Grao-Cruces SPSS Dataset`)
}
