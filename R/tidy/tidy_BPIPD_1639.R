#' Tidier for BPIPD-1639 (Prevention of Overweight in Infancy (POI) study)
#'
#' Three release files, each one row per child, keyed on `personid`.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per trial child with any age-5 questionnaire or
#'   assessment data (551 of the 802 randomised)
tidy_BPIPD_1639 <- function(raw_dataset, spec) {
  demographics <- tibble::as_tibble(raw_dataset$data$demographics)
  questionnaire <- tibble::as_tibble(raw_dataset$data$questionnaire)
  self_control <- tibble::as_tibble(raw_dataset$data$self_control)

  # 3.5-year files excluded: no usable screen-time measure at that wave.
  df <- demographics |>
    dplyr::left_join(
      questionnaire,
      by = "personid",
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      self_control,
      by = "personid",
      relationship = "one-to-one"
    )

  # Children not retained to age 5 have only a demographics row; drop them.
  age5 <- setdiff(c(names(questionnaire), names(self_control)), "personid")
  observed <- rowSums(!is.na(df[age5])) > 0L
  df[observed, , drop = FALSE]
}
