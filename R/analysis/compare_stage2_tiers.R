#' What a fuller adjustment changes, cell by cell
#'
#' A further covariate tier is fitted on fewer units than the first, and
#' within a unit on fewer rows, because rows with an unknown covariate are
#' left out. Comparing a tier's pooled slope with tier 1's over all units
#' would mix the effect of adjusting with the effect of losing units and
#' rows. So each further tier is set beside the tier 1 model fitted on the
#' tier's own rows (`tier1_same_rows_estimate` from `fit_stage1()`), pooled
#' the same way.
#'
#' The change has no interval: the two slopes come from the same children,
#' so their intervals cannot be compared, and the covariance between them is
#' not estimated. The spread of the unit-by-unit changes is given instead.
#'
#' @param first Stage 1 rows at tier 1.
#' @param further Stage 1 rows at the further tiers.
#' @param cells Optional tibble of `exposure` and `outcome`; see
#'   `pool_stage2_cells()`.
#' @param settings From `analysis_settings()`.
#' @return One row per cell and further tier: the `pool_stage2()` columns
#'   for the tier; `tier1_estimate` and its interval, from the tier 1 model
#'   on the tier's rows; `change`, the tier's estimate minus that one; the
#'   median and quartiles of the units' own changes; `rows_lost_share`, the
#'   share of the fitted units' rows left out for an unknown covariate; and
#'   `k_units_lost`, the units tier 1 fitted that the tier could not.
compare_stage2_tiers <- function(
  first,
  further,
  cells = NULL,
  settings = default_analysis_settings()
) {
  if (is.null(further) || nrow(further) == 0) {
    return(tibble::tibble())
  }
  key <- function(rows) paste(rows$unit, rows$exposure, rows$outcome)
  cell <- function(rows) paste(rows$exposure, rows$outcome)
  first_fitted <- first[first$status %in% "fitted", ]
  out <- lapply(sort(unique(further$tier)), function(tier) {
    rows <- further[further$tier == tier, ]
    fitted <- rows[rows$status %in% "fitted", ]
    pooled <- pool_stage2_cells(rows, cells, settings)
    same <- fitted
    same$estimate <- same$tier1_same_rows_estimate
    same$se <- same$tier1_same_rows_se
    same <- pool_stage2_cells(same, cells, settings)
    at <- match(cell(pooled), cell(same))
    pooled$tier <- as.integer(tier)
    pooled$tier1_estimate <- same$estimate[at]
    pooled$tier1_ci_low <- same$ci_low[at]
    pooled$tier1_ci_high <- same$ci_high[at]
    pooled$change <- pooled$estimate - pooled$tier1_estimate
    lost <- first_fitted[!key(first_fitted) %in% key(fitted), ]
    by_cell <- function(i, rows, summarise) {
      summarise(rows[cell(rows) %in% cell(pooled[i, ]), ])
    }
    index <- seq_len(nrow(pooled))
    quartile <- function(probability) {
      vapply(
        index,
        by_cell,
        numeric(1),
        rows = fitted,
        summarise = function(units) {
          stats::quantile(
            units$estimate - units$tier1_same_rows_estimate,
            probability,
            na.rm = TRUE,
            names = FALSE
          )
        }
      )
    }
    pooled$change_unit_q25 <- quartile(0.25)
    pooled$change_unit_median <- quartile(0.5)
    pooled$change_unit_q75 <- quartile(0.75)
    pooled$rows_lost_share <- vapply(
      index,
      by_cell,
      numeric(1),
      rows = fitted,
      summarise = function(units) {
        sum(units$n_no_covariate) / sum(units$n + units$n_no_covariate)
      }
    )
    pooled$k_units_lost <- vapply(
      index,
      by_cell,
      integer(1),
      rows = lost,
      summarise = nrow
    )
    pooled
  })
  dplyr::relocate(dplyr::bind_rows(out), "tier", .after = "outcome")
}
