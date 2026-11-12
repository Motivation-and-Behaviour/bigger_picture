#' Stage 2 for every cell in a table of stage 1 estimates
#'
#' @param estimates Stage 1 rows for any number of cells.
#' @param settings From `analysis_settings()`.
#' @return One `pool_stage2()` row per exposure, outcome and effect measure.
pool_stage2_cells <- function(estimates, settings = analysis_settings()) {
  if (is.null(estimates) || nrow(estimates) == 0) {
    return(tibble::tibble())
  }
  estimates <- estimates[estimates$status %in% "fitted", ]
  key <- paste(
    estimates$exposure,
    estimates$outcome,
    estimates$effect_measure,
    sep = "\r"
  )
  pooled <- lapply(split(estimates, key), function(cell) {
    out <- pool_stage2(cell, settings)
    out$exposure <- cell$exposure[1]
    out$outcome <- cell$outcome[1]
    out$score_type <- cell$score_type[1]
    out$effect_measure <- cell$effect_measure[1]
    out
  })
  out <- dplyr::bind_rows(pooled)
  dplyr::relocate(
    out,
    "exposure",
    "outcome",
    "score_type",
    "effect_measure"
  )
}
