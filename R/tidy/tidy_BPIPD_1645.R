#' Tidier for BPIPD-1645 (Tandon)
#'
#' The stacked release (both cohorts, re-weighted) is the spine; columns only the
#' 6-10 form carries are joined back from its file, and `cohort` records the form.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per parent-child dyad
tidy_BPIPD_1645 <- function(raw_dataset, spec) {
  stacked <- tibble::as_tibble(raw_dataset$data$`data-6-17 years stacked`)
  younger <- tibble::as_tibble(raw_dataset$data$`data-6-10 years`)

  # The stacked file lacks the 6-10 child's age and gender, parent race and
  # consent; the 6-10 file's `weight` equals `weight_6_to_10`.
  extra <- setdiff(names(younger), c(names(stacked), "weight"))

  df <- dplyr::left_join(
    stacked,
    younger[c("caseid", extra)],
    by = "caseid",
    relationship = "one-to-one"
  )

  cohort <- ifelse(df$caseid %in% younger$caseid, "6-10", "11-17")
  attr(cohort, "label") <- "Age cohort and questionnaire form"
  df <- dplyr::mutate(df, cohort = cohort, .after = "caseid")

  if (anyDuplicated(df$caseid) > 0) {
    stop(
      "BPIPD-1645: `caseid` does not uniquely identify dyads.",
      call. = FALSE
    )
  }

  df
}
