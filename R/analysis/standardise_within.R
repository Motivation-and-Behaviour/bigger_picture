#' Standardise a score within groups
#'
#' A z-score within each group: the score minus the group's mean, over the
#' group's standard deviation. A group with fewer than two values, or no
#' spread, gets NA, because it has no scale to standardise against.
#'
#' @param values A numeric vector.
#' @param groups A vector or list of vectors defining the groups.
#' @return A numeric vector the length of `values`.
standardise_within <- function(values, groups) {
  if (length(values) == 0) {
    return(numeric())
  }
  group <- if (is.list(groups)) {
    do.call(paste, c(lapply(groups, as.character), sep = "\r"))
  } else {
    as.character(groups)
  }
  group[is.na(group)] <- "\r(missing)"
  centre <- stats::ave(values, group, FUN = function(x) mean(x, na.rm = TRUE))
  spread <- stats::ave(values, group, FUN = function(x) {
    if (sum(!is.na(x)) > 1) stats::sd(x, na.rm = TRUE) else NA_real_
  })
  z <- (values - centre) / spread
  z[!is.finite(z)] <- NA_real_
  z
}
