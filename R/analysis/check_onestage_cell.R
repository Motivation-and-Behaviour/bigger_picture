#' Confirm the one-stage check's cell is a development cell
#'
#' Before the freeze only the development cells may be fitted on real data
#' (ANALYSIS_PLAN.md, Step 5). The one-stage check names its cell in the
#' settings, so this stops the pipeline if that cell is not on the list.
#'
#' @param cell A named vector with `exposure` and `outcome`.
#' @param cells The development cells, from `read_development_cells()`.
#' @return `cell`, when it is a development cell.
check_onestage_cell <- function(cell, cells) {
  listed <- any(
    cells$exposure == cell[["exposure"]] & cells$outcome == cell[["outcome"]]
  )
  if (!listed) {
    stop(
      "The one-stage check's cell (",
      cell[["exposure"]],
      " by ",
      cell[["outcome"]],
      ") is not a development cell; before the freeze only cells in ",
      bp_development_cells_path(),
      " may be fitted.",
      call. = FALSE
    )
  }
  cell
}
