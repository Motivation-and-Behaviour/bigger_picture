#' Stage 2 for every cell in a table of stage 1 estimates
#'
#' @param estimates Stage 1 rows for any number of cells.
#' @param cells Optional tibble of `exposure` and `outcome`: every cell
#'   listed gets a row, so a cell no unit could fit is reported and not
#'   silently absent.
#' @param settings From `analysis_settings()`.
#' @return One `pool_stage2()` row per exposure, outcome and effect measure.
pool_stage2_cells <- function(
  estimates,
  cells = NULL,
  settings = analysis_settings()
) {
  if (is.null(estimates)) {
    estimates <- tibble::tibble(
      exposure = character(),
      outcome = character(),
      score_type = character(),
      effect_measure = character(),
      status = character()
    )
  }
  fitted <- estimates[estimates$status %in% "fitted", ]
  key <- paste(
    fitted$exposure,
    fitted$outcome,
    fitted$effect_measure,
    sep = "\r"
  )
  pooled <- lapply(split(fitted, key), function(cell) {
    out <- pool_stage2(cell, settings)
    out$exposure <- cell$exposure[1]
    out$outcome <- cell$outcome[1]
    out$score_type <- cell$score_type[1]
    out$effect_measure <- cell$effect_measure[1]
    out
  })
  out <- dplyr::bind_rows(pooled)
  if (!is.null(cells)) {
    done <- paste(out$exposure, out$outcome)
    absent <- cells[!paste(cells$exposure, cells$outcome) %in% done, ]
    if (nrow(absent) > 0) {
      tried <- paste(estimates$exposure, estimates$outcome)
      empty <- pool_stage2(estimates[0, ], settings)
      none <- dplyr::bind_rows(lapply(seq_len(nrow(absent)), function(i) {
        row <- empty
        row$exposure <- absent$exposure[i]
        row$outcome <- absent$outcome[i]
        attempted <- paste(row$exposure, row$outcome) %in% tried
        row$status <- if (attempted) "no unit could be fitted" else "no data"
        row
      }))
      out <- dplyr::bind_rows(out, none)
    }
  }
  if (nrow(out) == 0) {
    return(tibble::tibble())
  }
  dplyr::relocate(
    out,
    dplyr::any_of(c("exposure", "outcome", "score_type", "effect_measure"))
  )
}
