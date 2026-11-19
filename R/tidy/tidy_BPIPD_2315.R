#' Tidier for BPIPD-2315 (Maciel)
#'
#' One curated cross-sectional file, kept as released; the variable labels the
#' Stata copy leaves blank are filled from the contributor's variable dictionary.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student
tidy_BPIPD_2315 <- function(raw_dataset, spec) {
  # The Stata copy is read; its undeclared CSV twin holds the same rows.
  df <- tibble::as_tibble(raw_dataset$data$core)

  dictionary <- readr::read_csv(
    raw_dataset$codebook$variable_codebook,
    col_types = readr::cols(.default = "c")
  )
  labels <- stats::setNames(dictionary$label, dictionary$variable)
  for (col in names(df)) {
    if (is.null(attr(df[[col]], "label"))) {
      attr(df[[col]], "label") <- labels[[col]]
    }
  }

  if (anyDuplicated(df$study_id) > 0) {
    stop(
      "BPIPD-2315: `study_id` does not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}
