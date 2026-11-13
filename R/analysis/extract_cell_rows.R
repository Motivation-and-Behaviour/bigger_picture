#' One dataset's rows for one exposure and outcome cell
#'
#' The one-stage check fits a single model across datasets, so it needs the
#' rows themselves. This keeps only what that model reads, for one cell, so
#' the combined table stays small.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param exposure,outcome The cell's exposure and outcome rows.
#' @return A tibble of `dataset_id`, `unit`, `x` (hours), `y` (the
#'   standardised score), `age` and `sex`, for rows with all of `x` and `y`;
#'   NULL when the dataset has no such rows.
extract_cell_rows <- function(prepared, exposure, outcome) {
  if (
    is.null(prepared) ||
      !exposure %in% names(prepared$exposures) ||
      !outcome %in% names(prepared$outcomes_z)
  ) {
    return(NULL)
  }
  base <- prepared$base
  rows <- tibble::tibble(
    dataset_id = base$dataset_id,
    unit = base$unit,
    x = prepared$exposures[[exposure]],
    y = prepared$outcomes_z[[outcome]],
    age = as.numeric(base$age_years),
    sex = if ("sex" %in% names(base)) base$sex else NA_character_
  )
  rows <- rows[!is.na(rows$x) & !is.na(rows$y), ]
  if (nrow(rows) == 0) NULL else rows
}
