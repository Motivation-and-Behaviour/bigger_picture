#' Count one dataset's mapping rows by harmonisation status
#'
#' @param variables A dataset's `variables.csv` table.
#' @param dataset_id The dataset's ID.
#' @return A tibble of `dataset_id`, `status`, `rows` and `source_columns`.
count_mapping <- function(variables, dataset_id) {
  sources <- function(cells) {
    names <- unlist(strsplit(cells[!is.na(cells)], "[,;|]"))
    names <- trimws(names)
    length(unique(names[nzchar(names)]))
  }
  out <- dplyr::summarise(
    variables,
    rows = dplyr::n(),
    source_columns = sources(.data$source_columns),
    .by = "status"
  )
  out$dataset_id <- rep(as.character(dataset_id), nrow(out))
  out
}
