#' Tidier for BPIPD-2679 (Donthu)
#'
#' One CSV from a single cross-sectional survey in which a parent reports on
#' one index child aged 5-15; the file has no participant id.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per index child (parent report)
tidy_BPIPD_2679 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$`Screen Time Datasheet`)

  # The commas in this header would split it in the comma-separated
  # `source_columns`, so they are dropped and the header kept as its label.
  df <- dplyr::rename(
    df,
    `fixed boundaries for movies youtube TV shows entertainment prog` = `fixed boundaries for movies , youtube, TV shows, entertainment prog`
  )
  attr(
    df$`fixed boundaries for movies youtube TV shows entertainment prog`,
    "label"
  ) <- "fixed boundaries for movies , youtube, TV shows, entertainment prog"

  df
}
