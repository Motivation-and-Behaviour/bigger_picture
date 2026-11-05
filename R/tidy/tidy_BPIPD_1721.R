#' Tidier for BPIPD-1721 (Cyr)
#'
#' One cross-sectional classroom survey of US high-school students, delivered on
#' a bubble sheet and keyed into a single SPSS file that already carries the
#' study's computed scale scores.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant
tidy_BPIPD_1721 <- function(raw_dataset, spec) {
  # The scoring syntax (`Info_Tech_Scoring_Prgm[1].SPS`) skips the bubble-sheet
  # columns holding birth date and identification number, so the file has no id.
  tibble::as_tibble(raw_dataset$data$`Cyr Data`)
}
