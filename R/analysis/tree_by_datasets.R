#' A family's estimate weighted by each specific outcome's datasets
#'
#' The sensitivity analysis beside the plain average of a family's specific
#' outcomes (DR-0014): `pool_tree_family()` computes both, and this puts the
#' weighted one in the estimate's columns so that it can be assembled and
#' tested as a model of its own.
#'
#' @param tree Rows of `pool_tree()`.
#' @return The family (L0) rows of `tree`, with `estimate`, `se`, `df` and
#'   the interval taken from the `_by_datasets` columns. The prediction
#'   interval is not carried over: it belongs to the plain average.
tree_by_datasets <- function(tree) {
  if (is.null(tree) || nrow(tree) == 0) {
    return(tibble::tibble())
  }
  out <- tree[tree$level %in% "L0", ]
  for (name in c("estimate", "se", "df", "ci_low", "ci_high")) {
    out[[name]] <- out[[paste0(name, "_by_datasets")]]
  }
  out$pi_low <- NA_real_
  out$pi_high <- NA_real_
  out
}
