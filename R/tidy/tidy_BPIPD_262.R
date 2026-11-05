#' Tidier for BPIPD-262 (Risks to Adolescent Wellbeing (RAW) Project)
#'
#' One SPSS file holds the Time 1 (Grade 6) child (`T1C_`) and parent (`T1P_`)
#' questionnaires side by side; the later annual waves are not in the extract.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child at Time 1
tidy_BPIPD_262 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  if (anyDuplicated(df$RAW_ID) > 0) {
    stop("BPIPD-262: `RAW_ID` does not uniquely identify rows.", call. = FALSE)
  }
  df
}
