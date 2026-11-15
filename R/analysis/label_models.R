#' Say which model each pooled row belongs to
#'
#' The cross-sectional models are fitted and pooled together, and a row's
#' model follows from its cell: its exposure, and for an outcome row (L3)
#' its outcome too. The models named must not share a cell.
#'
#' @param rows Pooled rows with `exposure`, and `outcome` for L3 rows.
#' @param model_cells From `expand_model_cells()`.
#' @param models The models the rows may belong to.
#' @return `rows` with `model_name`; rows that belong to none are dropped.
label_models <- function(rows, model_cells, models) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(tibble::tibble())
  }
  cells <- model_cells[model_cells$model_name %in% models, ]
  by <- if ("outcome" %in% names(rows)) c("exposure", "outcome") else "exposure"
  key <- dplyr::distinct(cells[c("model_name", by)])
  if (anyDuplicated(key[by]) > 0) {
    stop("label_models(): the models share a cell.", call. = FALSE)
  }
  rows$model_name <- NULL
  out <- dplyr::inner_join(rows, key, by = by)
  dplyr::relocate(out, "model_name")
}
