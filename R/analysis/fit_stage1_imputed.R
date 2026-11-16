#' Stage 1 on imputed data: fit each imputation, then combine
#'
#' For each imputation, the dataset's covariates are filled in
#' (`apply_imputation()`), each model's exposure is imputed for that model
#' (`impute_cell_exposure()`, through `fit_stage1()`), and the models are
#' fitted. Each unit's slope is then combined over the imputations by
#' Rubin's rules (`pool_rubin()`), so that stage 2 pools one slope per unit
#' as before.
#'
#' Outcomes are never imputed, so a row still needs its outcome to be
#' analysed.
#'
#' A slope is `fitted` only where every imputation fitted it; otherwise its
#' status says how many did, and it does not go on to stage 2. The columns
#' that describe a fit (covariates, measures, clusters) are those of the
#' first imputation; the counts of rows are means over imputations, the
#' tier 1 slope on the same rows is combined as the slope is, and
#' `design_varied` says whether the covariates, the measures chosen or the
#' rows differed between imputations.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param imputed Every imputation of the dataset's covariates, from
#'   `bind_imputations()`.
#' @param indexes The imputation numbers to fit.
#' @param cells,outcome_registry,dataschema,tiers,adjust_other As for
#'   `fit_stage1()`.
#' @param settings From `analysis_settings()`, with the model and the
#'   imputation settings.
#' @return The tibble of `fit_stage1()` with `m`, `riv`, `fmi` and
#'   `design_varied`.
fit_stage1_imputed <- function(
  prepared,
  imputed,
  indexes,
  cells,
  outcome_registry,
  dataschema,
  settings = default_analysis_settings(),
  tiers = 1L,
  adjust_other = FALSE
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  if (is.null(settings$seed)) {
    stop("fit_stage1_imputed(): `settings` needs a `seed`.", call. = FALSE)
  }
  fits <- lapply(indexes, function(index) {
    out <- fit_stage1(
      apply_imputation(prepared, imputed, index),
      cells,
      outcome_registry,
      dataschema,
      settings,
      tiers = tiers,
      adjust_other = adjust_other,
      imputation = list(index = index, seed = settings$seed)
    )
    if (!is.null(out)) {
      out$imputation <- rep(index, nrow(out))
    }
    out
  })
  fits <- dplyr::bind_rows(fits)
  if (nrow(fits) == 0) {
    return(NULL)
  }
  key <- paste(fits$unit, fits$exposure, fits$outcome, fits$tier, sep = "\r")
  m <- length(indexes)
  counts <- c(
    "n",
    "n_available",
    "n_no_weight",
    "n_no_sex",
    "n_no_covariate",
    "n_no_other",
    "n_exposure_imputed"
  )
  design <- c("covariates", "ses_measure", "ethnicity_measure", "n")
  combined <- lapply(split(fits, key), function(rows) {
    rows <- rows[order(rows$imputation), ]
    out <- rows[1, setdiff(names(rows), "imputation")]
    fitted <- rows$status %in% "fitted"
    out$design_varied <- nrow(dplyr::distinct(rows[design])) > 1
    out$m <- m
    out$riv <- NA_real_
    out$fmi <- NA_real_
    for (count in counts) {
      out[[count]] <- mean(rows[[count]])
    }
    if (all(fitted) && nrow(rows) == m) {
      pooled <- pool_rubin(rows$estimate, rows$se, rows$df)
      out$estimate <- pooled$estimate
      out$se <- pooled$se
      out$df <- pooled$df
      out$riv <- pooled$riv
      out$fmi <- pooled$fmi
      out$status <- "fitted"
      if (!anyNA(rows$tier1_same_rows_estimate)) {
        same <- pool_rubin(
          rows$tier1_same_rows_estimate,
          rows$tier1_same_rows_se,
          rows$df
        )
        out$tier1_same_rows_estimate <- same$estimate
        out$tier1_same_rows_se <- same$se
      }
    } else if (any(fitted)) {
      out$estimate <- NA_real_
      out$se <- NA_real_
      out$df <- NA_real_
      out$status <- sprintf("fitted in %d of %d imputations", sum(fitted), m)
    }
    out
  })
  dplyr::bind_rows(combined)
}
