#' A family's estimate weighted by each specific outcome's datasets
#'
#' The sensitivity analysis beside the plain average of a family's specific
#' outcomes (DR-0014): `pool_tree_family()` computes both, and this puts the
#' weighted one in the estimate's columns so that it can be assembled and
#' tested as a model of its own.
#'
#' The weighted average has degrees of freedom of its own, often far more
#' or fewer than the plain average's, so whether it has too few studies to
#' say is judged again on those (`df_floor_by_datasets`, the same form of
#' them as the plain average's status uses).
#'
#' @param tree Rows of `pool_tree()`.
#' @param settings From `analysis_settings()`.
#' @return The family (L0) rows of `tree`, with `estimate`, `se`, `df`, the
#'   interval and `status` taken from the `_by_datasets` columns. The
#'   prediction interval is not carried over: it belongs to the plain
#'   average.
tree_by_datasets <- function(tree, settings = default_analysis_settings()) {
  if (is.null(tree) || nrow(tree) == 0) {
    return(tibble::tibble())
  }
  out <- tree[tree$level %in% "L0", ]
  for (name in c("estimate", "se", "df", "ci_low", "ci_high")) {
    out[[name]] <- out[[paste0(name, "_by_datasets")]]
  }
  out$pi_low <- NA_real_
  out$pi_high <- NA_real_
  estimated <- out$status %in% c("pooled", "too few studies to say")
  # Judged on the same degrees of freedom as the plain average's status.
  floor_df <- if ("df_floor_by_datasets" %in% names(out)) {
    dplyr::coalesce(out$df_floor_by_datasets, out$df)
  } else {
    out$df
  }
  out$df_floor <- floor_df
  out$status[estimated] <- ifelse(
    is.na(out$df[estimated]),
    "robust inference failed",
    ifelse(
      floor_df[estimated] < settings$min_robust_df,
      "too few studies to say",
      "pooled"
    )
  )
  out
}
