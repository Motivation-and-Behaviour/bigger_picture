#' What a fuller adjustment changes, cell by cell
#'
#' A unit that does not record a tier's covariates is not fitted at that
#' tier, so a further tier rests on fewer units than the first. Comparing a
#' tier's pooled slope with the first tier's over all units would mix the
#' effect of adjusting with the effect of losing units, so each further
#' tier is set beside the first tier pooled over the same units.
#'
#' @param first Stage 1 rows at tier 1.
#' @param further Stage 1 rows at the further tiers.
#' @param cells Optional tibble of `exposure` and `outcome`; see
#'   `pool_stage2_cells()`.
#' @param settings From `analysis_settings()`.
#' @return One row per cell and further tier: the `pool_stage2()` columns
#'   for the tier; `tier1_k_datasets`, `tier1_estimate` and its interval,
#'   pooled over the units the tier could fit; `change`, the tier's estimate
#'   minus that one; and `k_units_lost`, the units tier 1 fitted that the
#'   tier could not.
compare_stage2_tiers <- function(
  first,
  further,
  cells = NULL,
  settings = analysis_settings()
) {
  if (is.null(further) || nrow(further) == 0) {
    return(tibble::tibble())
  }
  key <- function(rows) paste(rows$unit, rows$exposure, rows$outcome)
  first_fitted <- first[first$status %in% "fitted", ]
  out <- lapply(sort(unique(further$tier)), function(tier) {
    rows <- further[further$tier == tier, ]
    fitted <- rows[rows$status %in% "fitted", ]
    pooled <- pool_stage2_cells(rows, cells, settings)
    same <- pool_stage2_cells(
      first_fitted[key(first_fitted) %in% key(fitted), ],
      cells,
      settings
    )
    at <- match(
      paste(pooled$exposure, pooled$outcome),
      paste(same$exposure, same$outcome)
    )
    pooled$tier <- as.integer(tier)
    pooled$tier1_k_datasets <- same$k_datasets[at]
    pooled$tier1_estimate <- same$estimate[at]
    pooled$tier1_ci_low <- same$ci_low[at]
    pooled$tier1_ci_high <- same$ci_high[at]
    pooled$change <- pooled$estimate - pooled$tier1_estimate
    lost <- first_fitted[!key(first_fitted) %in% key(fitted), ]
    pooled$k_units_lost <- vapply(
      seq_len(nrow(pooled)),
      function(i) {
        sum(
          lost$exposure %in%
            pooled$exposure[i] &
            lost$outcome %in% pooled$outcome[i]
        )
      },
      integer(1)
    )
    pooled
  })
  dplyr::relocate(dplyr::bind_rows(out), "tier", .after = "outcome")
}
