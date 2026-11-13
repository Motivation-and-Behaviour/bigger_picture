#' How strongly two outcome rows' slopes in one unit share their error
#'
#' Two rows of one specific outcome fitted in one unit are fitted on many of
#' the same children, so their slopes on screen use err together. The
#' correlation between the two slopes is estimated from the unit's rows as
#' the correlation between the two outcomes, after taking screen use, age
#' and sex out of each, on the rows that have both, scaled by the share of
#' rows the two fits have in common: `r_residual * n_shared / sqrt(n_a *
#' n_b)`. Two rows with no child in common get zero.
#'
#' This is an approximation: it treats every score as numeric and ignores
#' the survey design. It sets the weights of the working model in
#' `pool_tree_group()`, whose inference is robust to a wrong covariance.
#'
#' Only pairs within one specific outcome are estimated, because pooling
#' uses no other.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param cells A tibble of `exposure` and `outcome` rows.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per unit, exposure and pair of outcome rows
#'   observed in the unit: `r`, the correlation between the slopes;
#'   `r_residual` and `n_shared` behind it; and `basis` (`estimated`, `no
#'   shared rows` or `default` where too few rows have both). NULL when the
#'   dataset is not included or holds no pair.
stage1_correlations <- function(
  prepared,
  cells,
  outcome_registry,
  settings = analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  cells <- cells[
    cells$exposure %in%
      names(prepared$exposures) &
      cells$outcome %in% names(prepared$outcomes),
  ]
  registry <- outcome_registry$rows
  cells$node <- registry$node[match(cells$outcome, registry$variable_name)]
  cells <- cells[!is.na(cells$node), ]
  if (nrow(cells) < 2) {
    return(NULL)
  }
  weight <- as.numeric(base$survey_weight_norm)
  age <- as.numeric(base$age_years)
  sex <- if ("sex" %in% names(base)) as.character(base$sex) else NA
  female <- ifelse(sex %in% "Female", 1, ifelse(sex %in% "Male", 0, NA))
  value <- function(outcome) {
    if (outcome %in% names(prepared$outcomes_z)) {
      prepared$outcomes_z[[outcome]]
    } else {
      as.numeric(prepared$outcomes[[outcome]])
    }
  }
  # What is left of a score once the model's other terms are taken out.
  residual <- function(y, design, w) {
    stats::lm.wfit(design, y, w)$residuals
  }
  unit_rows <- split(seq_len(nrow(base)), base$unit)

  results <- list()
  for (group in split(cells, paste(cells$exposure, cells$node))) {
    if (nrow(group) < 2) {
      next
    }
    x <- prepared$exposures[[group$exposure[1]]]
    values <- lapply(group$outcome, value)
    pairs <- utils::combn(nrow(group), 2)
    for (unit in names(unit_rows)) {
      index <- unit_rows[[unit]]
      index <- index[!is.na(x[index])]
      weighted <- any(!is.na(weight[index]) & weight[index] > 0)
      if (weighted) {
        index <- index[!is.na(weight[index]) & weight[index] > 0]
      }
      for (p in seq_len(ncol(pairs))) {
        a <- values[[pairs[1, p]]]
        b <- values[[pairs[2, p]]]
        n_a <- sum(!is.na(a[index]))
        n_b <- sum(!is.na(b[index]))
        if (n_a == 0 || n_b == 0) {
          next
        }
        shared <- index[!is.na(a[index]) & !is.na(b[index])]
        n_shared <- length(shared)
        overlap <- n_shared / sqrt(as.numeric(n_a) * n_b)
        r_residual <- NA_real_
        basis <- "no shared rows"
        if (n_shared >= settings$min_unit_rows) {
          design <- cbind(1, x[shared], age[shared], female[shared])
          # A term the unit does not record, or that does not vary, is left
          # out.
          usable <- apply(design, 2, function(column) {
            !anyNA(column) && (all(column == 1) || stats::sd(column) > 0)
          })
          design <- design[, usable, drop = FALSE]
          w <- if (weighted) weight[shared] else rep(1, n_shared)
          r_residual <- stats::cov.wt(
            cbind(
              residual(a[shared], design, w),
              residual(b[shared], design, w)
            ),
            wt = w / sum(w),
            cor = TRUE
          )$cor[1, 2]
          basis <- "estimated"
        }
        if (n_shared > 0 && is.na(r_residual)) {
          r_residual <- settings$tree_default_correlation
          basis <- "default"
        }
        results[[length(results) + 1]] <- tibble::tibble(
          unit = unit,
          exposure = group$exposure[1],
          outcome_a = group$outcome[pairs[1, p]],
          outcome_b = group$outcome[pairs[2, p]],
          r = if (n_shared == 0) 0 else r_residual * overlap,
          r_residual = r_residual,
          n_shared = n_shared,
          basis = basis
        )
      }
    }
  }
  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- rep(as.character(base$dataset_id[1]), nrow(out))
  dplyr::relocate(out, "dataset_id")
}
