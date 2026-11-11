#' Read an analysis registry CSV as text, with its logical columns converted
#'
#' Every column is read as character, so blanks stay NA and nothing is
#' re-typed by guessing; the named logical columns accept `TRUE`, `FALSE` or
#' blank.
#'
#' @param path Path to the CSV.
#' @param columns Required columns, in output order.
#' @param logical_columns Columns holding `TRUE`/`FALSE`.
#' @return A tibble with `columns` first, then any extra columns.
read_registry_csv <- function(path, columns, logical_columns = character()) {
  tbl <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA"),
    trim_ws = TRUE
  )
  label <- basename(path)
  ensure_required_columns(tbl, columns, label)

  for (column in logical_columns) {
    values <- tbl[[column]]
    bad <- !is.na(values) & !values %in% c("TRUE", "FALSE")
    if (any(bad)) {
      stop(
        label,
        ": `",
        column,
        "` must be TRUE, FALSE or blank; found ",
        paste(unique(values[bad]), collapse = ", "),
        call. = FALSE
      )
    }
    tbl[[column]] <- as.logical(values)
  }

  dplyr::select(tbl, dplyr::all_of(columns), dplyr::everything())
}
