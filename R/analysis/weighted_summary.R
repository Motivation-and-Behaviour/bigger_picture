#' Weighted summary of a screen-time variable
#'
#' Medians and shares lead the descriptives, because top-coded response bands
#' make means understate high use. Quantiles are weighted: the smallest value
#' at which the cumulative share of weight reaches the probability.
#'
#' @param x Hours a day.
#' @param w Weights, one per value.
#' @param thresholds Durations for "share at or above".
#' @return A one-row tibble: `n`, `mean`, the 10th, 25th, 50th, 75th and 90th
#'   percentiles, `share_zero` and one `share_<h>plus` per threshold.
weighted_summary <- function(x, w = rep(1, length(x)), thresholds = c(2, 4)) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  probabilities <- c(p10 = 0.1, p25 = 0.25, median = 0.5, p75 = 0.75, p90 = 0.9)
  if (length(x) == 0) {
    quantiles <- stats::setNames(rep(NA_real_, 5), names(probabilities))
    shares <- rep(NA_real_, length(thresholds))
    centre <- NA_real_
    zero <- NA_real_
  } else {
    order <- order(x)
    cumulative <- cumsum(w[order]) / sum(w)
    quantiles <- vapply(
      probabilities,
      function(p) x[order][which(cumulative >= p - 1e-12)[1]],
      numeric(1)
    )
    shares <- vapply(
      thresholds,
      function(h) sum(w[x >= h]) / sum(w),
      numeric(1)
    )
    centre <- sum(w * x) / sum(w)
    zero <- sum(w[x == 0]) / sum(w)
  }
  out <- c(
    list(n = length(x), mean = centre),
    as.list(quantiles),
    list(share_zero = zero),
    stats::setNames(as.list(shares), paste0("share_", thresholds, "plus"))
  )
  tibble::as_tibble(out)
}
