#' Sampling covariance of slopes that share a unit's participants
#'
#' Two outcome rows fitted in one unit are fitted on the same children, so
#' their slopes' errors are correlated. Slopes in different units are taken
#' as independent, as stage 2 does.
#'
#' Correlations are given for the slopes as stage 1 estimated them, so a
#' pair changes sign when one of the two slopes was flipped to align
#' directions. A pair with no estimate takes the default correlation, which
#' is for aligned slopes and is scaled by the pair's `overlap` where that is
#' known.
#'
#' Correlations estimated pair by pair need not form a possible correlation
#' matrix. Where a unit's do not, they are shrunk towards zero by the
#' smallest factor that makes the matrix usable, and the unit is counted.
#'
#' @param effects Rows from `tree_effects()`: `unit`, `outcome`, `se` and,
#'   where the alignment changed a sign, `flipped`.
#' @param correlations A tibble of `unit`, `outcome_a`, `outcome_b` and `r`,
#'   as `stage1_correlations()` gives; either order of the pair. NULL when
#'   none is known.
#' @param settings From `analysis_settings()`.
#' @return A list: `V`, the covariance matrix in the order of `effects`;
#'   `assumed`, the number of pairs given the default correlation; and
#'   `repaired`, the number of units whose correlations were shrunk.
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
  has <- !is.null(correlations) && nrow(correlations) > 0
  known <- if (has) {
    c(
      paste(correlations$unit, correlations$outcome_a, correlations$outcome_b),
      paste(correlations$unit, correlations$outcome_b, correlations$outcome_a)
    )
  } else {
    character()
  }
  known_r <- if (has) rep(correlations$r, 2) else numeric()
  known_overlap <- if (has && "overlap" %in% names(correlations)) {
    rep(correlations$overlap, 2)
  } else {
    rep(1, length(known))
  }
  assumed <- 0L
  repaired <- 0L
  for (unit_rows in split(seq_len(n), effects$unit)) {
    m <- length(unit_rows)
    if (m < 2) {
      next
    }
    r_block <- diag(m)
    pairs <- utils::combn(m, 2)
    for (p in seq_len(ncol(pairs))) {
      i <- unit_rows[pairs[1, p]]
      j <- unit_rows[pairs[2, p]]
      if (effects$outcome[i] == effects$outcome[j]) {
        r <- if (flipped[i] == flipped[j]) 1 else -1
      } else {
        key <- paste(effects$unit[i], effects$outcome[i], effects$outcome[j])
        at <- match(key, known)
        r <- known_r[at]
        if (is.na(r)) {
          overlap <- if (is.na(at)) 1 else known_overlap[at]
          r <- settings$tree_default_correlation * overlap
          assumed <- assumed + 1L
        } else if (flipped[i] != flipped[j]) {
          r <- -r
        }
      }
      r_block[pairs[1, p], pairs[2, p]] <- r
      r_block[pairs[2, p], pairs[1, p]] <- r
    }
    # Shrink towards zero until the block is safely positive definite.
    floor <- 1e-6
    smallest <- min(eigen(r_block, symmetric = TRUE, only.values = TRUE)$values)
    if (smallest < floor) {
      keep <- (1 - floor) / (1 - smallest)
      r_block <- keep * r_block + (1 - keep) * diag(m)
      repaired <- repaired + 1L
    }
    se <- effects$se[unit_rows]
    covariance[unit_rows, unit_rows] <- r_block * outer(se, se)
  }
  list(V = covariance, assumed = assumed, repaired = repaired)
}
