#' Combine one slope's estimates over imputations by Rubin's rules
#'
#' The estimate is the mean over imputations. Its variance is the mean of
#' the imputations' squared standard errors plus the variance between
#' their estimates, the latter inflated by 1 + 1 / m. The degrees of
#' freedom are Barnard and Rubin's, which never exceed those of one
#' complete-data fit.
#'
#' @param estimate,se,df One imputation's estimate, standard error and
#'   degrees of freedom each; vectors of one length, with no missing value.
#' @return A one-row tibble: `estimate`, `se`, `df`, `m`, `riv` (the
#'   relative increase in variance from missing values) and `fmi` (the
#'   fraction of information missing).
pool_rubin <- function(estimate, se, df) {
  m <- length(estimate)
  if (m == 0 || anyNA(estimate) || anyNA(se)) {
    stop("pool_rubin(): needs an estimate and a standard error per imputation.")
  }
  within <- mean(se^2)
  if (m == 1) {
    return(tibble::tibble(
      estimate = estimate,
      se = se,
      df = df,
      m = 1L,
      riv = NA_real_,
      fmi = NA_real_
    ))
  }
  between <- stats::var(estimate)
  total <- within + (1 + 1 / m) * between
  riv <- (1 + 1 / m) * between / within
  lambda <- (1 + 1 / m) * between / total
  complete <- mean(df)
  if (between == 0) {
    return(tibble::tibble(
      estimate = mean(estimate),
      se = sqrt(total),
      df = complete,
      m = m,
      riv = 0,
      fmi = 0
    ))
  }
  old <- (m - 1) / lambda^2
  observed <- if (is.finite(complete)) {
    (complete + 1) / (complete + 3) * complete * (1 - lambda)
  } else {
    Inf
  }
  pooled_df <- 1 / (1 / old + 1 / observed)
  tibble::tibble(
    estimate = mean(estimate),
    se = sqrt(total),
    df = pooled_df,
    m = m,
    riv = riv,
    fmi = (riv + 2 / (pooled_df + 3)) / (1 + riv)
  )
}
