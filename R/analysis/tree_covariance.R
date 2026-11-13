#' Sampling covariance of slopes that share a unit's participants
#'
#' Two outcome rows fitted in one unit are fitted on the same children, so
#' their slopes' errors are correlated. Slopes in different units are taken
#' as independent, as stage 2 does.
#'
#' @param effects Rows from `tree_effects()`: `unit`, `outcome`, `se` and,
#'   where the alignment changed a sign, `flipped`.
#' @param correlations A tibble of `unit`, `outcome_a`, `outcome_b` and `r`,
#'   the correlation between the two rows' slopes as estimated; either
#'   order of the pair. NULL when none is known.
#' @param settings From `analysis_settings()`.
#' @return A list: `V`, the covariance matrix in the order of `effects`,
#'   and `assumed`, the number of pairs given the default correlation.
tree_covariance <- function(
  effects,
  correlations = NULL,
  settings = analysis_settings()
) {
  n <- nrow(effects)
  covariance <- diag(effects$se^2, nrow = n)
  flipped <- if ("flipped" %in% names(effects)) {
    effects$flipped
  } else {
    rep(FALSE, n)
  }
  known <- if (is.null(correlations) || nrow(correlations) == 0) {
    character()
  } else {
    c(
      paste(correlations$unit, correlations$outcome_a, correlations$outcome_b),
      paste(correlations$unit, correlations$outcome_b, correlations$outcome_a)
    )
  }
  known_r <- if (length(known) > 0) rep(correlations$r, 2) else numeric()
  assumed <- 0L
  for (unit_rows in split(seq_len(n), effects$unit)) {
    if (length(unit_rows) < 2) {
      next
    }
    pairs <- utils::combn(unit_rows, 2)
    for (p in seq_len(ncol(pairs))) {
      i <- pairs[1, p]
      j <- pairs[2, p]
      r <- if (effects$outcome[i] == effects$outcome[j]) {
        1
      } else {
        key <- paste(effects$unit[i], effects$outcome[i], effects$outcome[j])
        known_r[match(key, known)]
      }
      if (is.na(r)) {
        r <- settings$tree_default_correlation
        assumed <- assumed + 1L
      } else if (flipped[i] != flipped[j]) {
        # The correlation was estimated before one slope changed sign.
        r <- -r
      }
      covariance[i, j] <- covariance[j, i] <- r * effects$se[i] * effects$se[j]
    }
  }
  list(V = covariance, assumed = assumed)
}
