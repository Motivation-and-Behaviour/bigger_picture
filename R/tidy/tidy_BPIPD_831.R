#' Tidier for BPIPD-831 (Pastor-Ruiz)
#'
#' One Google Forms export of a single cross-sectional survey. Its headers are
#' the full question text, so each column is renamed to the questionnaire's
#' item number and keeps its header as the column label.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent
tidy_BPIPD_831 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  headers <- names(df)
  names(df) <- bp831_item_codes(headers)

  for (i in seq_along(headers)) {
    attr(df[[i]], "label") <- headers[[i]]
  }

  # `EDAD` is the volatile worksheet formula `YEAR(TODAY()) - YEAR(birth date)`,
  # so its stored values are ages at the workbook's last save.
  attr(df$EDAD, "label") <- paste(
    "EDAD (worksheet formula YEAR(TODAY()) - YEAR(birth date)):",
    "age at the file's last save, not age at data collection"
  )

  df
}

#' Questionnaire item numbers read from the Google Forms headers
bp831_item_codes <- function(headers) {
  codes <- sub(
    "^\\[?[[:space:]]*([0-9]+)[[:space:]]*\\.[[:space:]]*([0-9]+)?.*$",
    "q\\1_\\2",
    headers
  )
  codes <- sub("_$", "", codes)

  # The questionnaire numbers item 19.2 as a second 19.1 and item 21.4 as 214,
  # and the export dropped item 23.9's number; `EDAD` never had one.
  codes[[max(which(codes == "q19_1"))]] <- "q19_2"
  codes[codes == "q214"] <- "q21_4"
  codes[codes == headers & codes != "EDAD"] <- "q23_9"

  if (anyDuplicated(codes) > 0) {
    stop(
      "BPIPD-831: item codes do not uniquely name the columns.",
      call. = FALSE
    )
  }

  codes
}
