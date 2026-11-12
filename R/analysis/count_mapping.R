#' Count one dataset's mapping rows by harmonisation status
#'
#' @param variables A dataset's `variables.csv` table.
#' @param dataset_id The dataset's ID.
#' @return A tibble of `dataset_id`, `status`, `rows` (mapping rows, including
#'   rows that re-map a screen-time variable for an extra measure) and
#'   `tidied_columns`: on every row, the number of distinct columns of the
#'   dataset's tidied table that its evaluated mappings name.
count_mapping <- function(variables, dataset_id) {
  evaluated <- variables$status %in% bp_evaluated_statuses()
  cells <- variables$source_columns[evaluated]
  names <- trimws(unlist(strsplit(cells[!is.na(cells)], "[,;|]")))
  # Free-text entries such as an instrument name are not column names.
  names <- unique(names[grepl("^[A-Za-z.][A-Za-z0-9._]*$", names)])
  out <- dplyr::count(variables, .data$status, name = "rows")
  out$tidied_columns <- rep(length(names), nrow(out))
  out$dataset_id <- rep(as.character(dataset_id), nrow(out))
  out
}

# Statuses whose mapping expression is evaluated, so the row has data.
bp_evaluated_statuses <- function() {
  c("compatible", "partial", "proximate", "inferred")
}
