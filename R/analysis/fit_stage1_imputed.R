#' Stage 1 on imputed data: fit each imputation, then combine
#'
#' Fits `fit_stage1()` on the dataset filled with each imputation in turn
#' and combines each unit's slope over the imputations by Rubin's rules
#' (`pool_rubin()`), so that stage 2 pools one slope per unit as before.
#'
#' Only screen use and covariates are imputed, so a row still needs its
#' outcome to be analysed.
#'
#' A slope is `fitted` only where every imputation fitted it; otherwise its
#' status says how many did, and it does not go on to stage 2. The columns
#' that describe a fit (rows, covariates, measures) are those of the first
#' imputation, with `n` the mean over imputations, and `design_varied` says
#' whether the covariates differed between imputations.
#'
#' A dataset with nothing imputed is fitted once, as it is.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param imputed Every imputation of the dataset from `impute_dataset()`,
#'   bound together.
#' @param cells,outcome_registry,dataschema,settings,tiers,adjust_other As
#'   for `fit_stage1()`.
#' @return The tibble of `fit_stage1()` with `m`, `riv`, `fmi` and
#'   `design_varied`.
fit_stage1_imputed <- function(
  prepared,
  imputed,
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
  fit <- function(data) {
    fit_stage1(
      data,
      cells,
      outcome_registry,
      dataschema,
      settings,
      tiers = tiers,
      adjust_other = adjust_other
    )
  }
  indexes <- sort(unique(c(
    imputed$numeric$imputation,
    imputed$categorical$imputation
  )))
  if (length(indexes) == 0) {
    out <- fit(prepared)
    if (is.null(out)) {
      return(NULL)
    }
    out$m <- 0L
    out$riv <- NA_real_
    out$fmi <- NA_real_
    out$design_varied <- FALSE
    return(out)
  }
  fits <- lapply(indexes, function(index) {
    out <- fit(apply_imputation(
      prepared,
      imputed,
      index,
      max_hours = settings$exposure_max_hours
    ))
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
  combined <- lapply(split(fits, key), function(rows) {
    rows <- rows[order(rows$imputation), ]
    out <- rows[1, setdiff(names(rows), "imputation")]
    fitted <- rows$status %in% "fitted"
    out$design_varied <- dplyr::n_distinct(rows$covariates) > 1
    out$m <- m
    out$riv <- NA_real_
    out$fmi <- NA_real_
    if (all(fitted) && nrow(rows) == m) {
      pooled <- pool_rubin(rows$estimate, rows$se, rows$df)
      out$estimate <- pooled$estimate
      out$se <- pooled$se
      out$df <- pooled$df
      out$riv <- pooled$riv
      out$fmi <- pooled$fmi
      out$n <- mean(rows$n)
      out$status <- "fitted"
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
