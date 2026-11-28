#' Stage 1 on imputed data: fit each imputation
#'
#' For each imputation, the dataset's covariates are filled in
#' (`apply_imputation()`), each model's exposure is imputed for that model
#' (`impute_cell_exposure()`, through `fit_stage1()`), and the models are
#' fitted. `combine_stage1_imputations()` then combines each unit's slope
#' over the imputations by Rubin's rules (`pool_rubin()`), so that stage 2
#' pools one slope per unit as before.
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
#' @param review The dataset's `review_imputation()`, whose checks stop
#'   the build when an imputation is wrong. It is asked for so that no
#'   model is fitted on imputations that have not passed them.
#' @param indexes The imputation numbers to fit.
#' @param cells,outcome_registry,dataschema,tiers,adjust_other,moderator As
#'   for `fit_stage1()`.
#' @param settings From `analysis_settings()`, with the model and the
#'   imputation settings.
#' @return The rows of `fit_stage1()` for each imputation fitted, with
#'   `imputation`; NULL where nothing was fitted. Combined, they gain `m`,
#'   `riv`, `fmi` and `design_varied`.
fit_stage1_imputations <- function(
  prepared,
  imputed,
  review,
  indexes,
  cells,
  outcome_registry,
  dataschema,
  settings = default_analysis_settings(),
  tiers = 1L,
  adjust_other = FALSE,
  moderator = NULL
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  if (is.null(settings$seed)) {
    stop("fit_stage1_imputations(): `settings` needs a `seed`.", call. = FALSE)
  }
  # The imputations are fitted only once they have passed their checks:
  # taking the review as an argument makes the fit wait for it.
  if (
    !is.list(review) ||
      !"units" %in% names(review) ||
      !identical(
        review$dataset_id,
        as.character(prepared$base$dataset_id[1])
      )
  ) {
    stop(
      "fit_stage1_imputations(): needs the dataset's own `review_imputation()`.",
      call. = FALSE
    )
  }
  # An imputation that was never made must not be fitted as if it had
  # nothing to fill.
  if (!all(indexes %in% imputed$made)) {
    stop(
      "fit_stage1_imputations(): imputation ",
      paste(setdiff(indexes, imputed$made), collapse = ", "),
      " is missing for dataset ",
      prepared$base$dataset_id[1],
      ".",
      call. = FALSE
    )
  }
  # One store of each exposure's mean over a child's other waves serves
  # every imputation (see `fit_stage1()`).
  other_waves <- new.env(parent = emptyenv())
  fits <- lapply(indexes, function(index) {
    out <- fit_stage1(
      apply_imputation(prepared, imputed, index),
      cells,
      outcome_registry,
      dataschema,
      settings,
      tiers = tiers,
      adjust_other = adjust_other,
      moderator = moderator,
      imputation = list(
        index = index,
        seed = settings$seed,
        other_waves = other_waves
      )
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
  fits
}

#' Combine the fits of a dataset's imputations by Rubin's rules
#'
#' @param fits Rows of `fit_stage1_imputations()` for every imputation, as
#'   one table or a list of tables (one per group of imputations, NULL
#'   where a group fitted nothing).
#' @param indexes The imputation numbers there should be.
#' @return As `fit_stage1_imputed()`, or NULL where nothing was fitted.
combine_stage1_imputations <- function(fits, indexes) {
  if (!is.data.frame(fits)) {
    fits <- dplyr::bind_rows(fits)
  }
  if (is.null(fits) || nrow(fits) == 0) {
    return(NULL)
  }
  # Every imputation must be there: a group of them that failed to arrive
  # would otherwise pass for slopes fitted in too few imputations.
  if (!setequal(fits$imputation, indexes)) {
    stop(
      "combine_stage1_imputations(): the fits hold ",
      dplyr::n_distinct(fits$imputation),
      " imputations, not the ",
      length(indexes),
      " asked for.",
      call. = FALSE
    )
  }
  # Nor may one arrive twice.
  if (
    anyDuplicated(fits[c(
      "unit",
      "exposure",
      "outcome",
      "tier",
      "imputation"
    )]) >
      0
  ) {
    stop(
      "combine_stage1_imputations(): an imputation's fits are there twice.",
      call. = FALSE
    )
  }
  m <- length(indexes)
  key <- paste(fits$unit, fits$exposure, fits$outcome, fits$tier, sep = "\r")
  counts <- c(
    "covariate_imputed_share",
    "n",
    "n_available",
    "n_no_weight",
    "n_no_sex",
    "n_no_covariate",
    "n_no_other",
    "n_other_inconsistent",
    "n_exposure_imputed"
  )
  design <- c("covariates", "ses_measure", "ethnicity_measure", "n")
  # A model of moderation also has the rows left out for want of a group,
  # the measure the groups came from, and the two groups' slopes, which
  # are averaged over the imputations as the difference between them is.
  moderated <- "moderator" %in% names(fits)
  slopes <- c("slope_first", "slope_second")
  if (moderated) {
    counts <- c(counts, "n_no_group")
    design <- c(design, "moderator_measure")
  }
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
      if (moderated) {
        for (slope in slopes) {
          out[[slope]] <- mean(rows[[slope]])
        }
      }
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
      if (moderated) {
        out[slopes] <- NA_real_
      }
      out$status <- sprintf("fitted in %d of %d imputations", sum(fitted), m)
    }
    out
  })
  dplyr::bind_rows(combined)
}

#' Fit a dataset's models on every imputation and combine them
#'
#' `fit_stage1_imputations()` then `combine_stage1_imputations()`. The
#' pipeline calls the two apart, so that a dataset's imputations are fitted
#' in groups on several workers; this is the whole of it in one call.
#'
#' @inheritParams fit_stage1_imputations
#' @return The combined rows; see the documentation above.
fit_stage1_imputed <- function(
  prepared,
  imputed,
  review,
  indexes,
  cells,
  outcome_registry,
  dataschema,
  settings = default_analysis_settings(),
  tiers = 1L,
  adjust_other = FALSE,
  moderator = NULL
) {
  fits <- fit_stage1_imputations(
    prepared,
    imputed,
    review,
    indexes,
    cells,
    outcome_registry,
    dataschema,
    settings,
    tiers = tiers,
    adjust_other = adjust_other,
    moderator = moderator
  )
  combine_stage1_imputations(fits, indexes)
}
