#' Is an estimate within the smallest effect of interest?
#'
#' Two one-sided tests against the bounds `-margin` and `margin`. The
#' p-value is the larger of the two, so a small one says the estimate is
#' reliably inside the bounds: evidence of no association large enough to
#' matter, which a non-significant difference from zero is not.
#'
#' The tests use the estimate's own standard error and degrees of freedom,
#' so with a robust standard error they are robust in the same way.
#'
#' @param estimate,se,df The estimate, its standard error and its degrees of
#'   freedom; vectors of one length.
#' @param margin The smallest effect of interest, on the estimate's scale.
#' @return A tibble, one row per estimate: `p_equivalence`, and
#'   `equivalent`, TRUE where the 90% interval lies inside the bounds
#'   (p below 0.05).
test_equivalence <- function(estimate, se, df, margin) {
  if (length(margin) != 1 || is.na(margin) || margin <= 0) {
    stop(
      "test_equivalence(): `margin` must be one positive number.",
      call. = FALSE
    )
  }
  usable <- !is.na(estimate) & !is.na(se) & !is.na(df) & se > 0 & df > 0
  p <- rep(NA_real_, length(estimate))
  # Against the lower bound: is the estimate above it?
  above <- stats::pt(
    (estimate[usable] + margin) / se[usable],
    df = df[usable],
    lower.tail = FALSE
  )
  # Against the upper bound: is the estimate below it?
  below <- stats::pt(
    (estimate[usable] - margin) / se[usable],
    df = df[usable],
    lower.tail = TRUE
  )
  p[usable] <- pmax(above, below)
  tibble::tibble(p_equivalence = p, equivalent = p < 0.05)
}
