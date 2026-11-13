#' Average a family's specific outcomes into its L0 estimate
#'
#' The family's estimate is the plain average of its pooled specific
#' outcomes, so each specific outcome counts once however many rows,
#' datasets or participants it has (ANALYSIS_PLAN.md, Step 7 task 1). Each
#' specific outcome keeps exactly the estimate `pool_tree_group()` gave it:
#' the slopes are refitted with one mean per specific outcome and each
#' group's own working covariance.
#'
#' The standard error is robust to any dependence among a dataset's slopes,
#' within and across specific outcomes. A score that sits in two specific
#' outcomes therefore adds nothing to the family's precision: it counts
#' once.
#'
#' The family is always estimated when at least two of its specific outcomes
#' pooled, and carries a warning where they disagree in sign or their spread
#' exceeds the smallest effect of interest; it is then not for the headline.
#'
#' @param effects Rows from `tree_effects()` for one family.
#' @param groups A named list of `pool_tree_group()` results, one per node,
#'   each fitted on that node's rows of `effects` in their order.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble: the number of specific outcomes averaged, the
#'   `estimate` with its robust `se`, `df` and 95% interval, the range of the
#'   specific outcomes' estimates, `warning` and `status`.
pool_tree_family <- function(effects, groups, settings = analysis_settings()) {
  pooled <- names(groups)[vapply(
    groups,
    function(group) !is.null(group$M) && !is.na(group$row$estimate),
    logical(1)
  )]
  effects <- effects[effects$node %in% pooled, ]
  result <- function(status, ...) {
    values <- list(
      k_nodes = length(pooled),
      k_datasets = dplyr::n_distinct(effects$dataset_id),
      k_slopes = nrow(effects),
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      node_low = NA_real_,
      node_high = NA_real_,
      warning = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (length(pooled) < 2) {
    return(result("fewer than two specific outcomes pooled"))
  }

  # One block per specific outcome, in the order its group was fitted.
  blocks <- lapply(pooled, function(node) effects[effects$node == node, ])
  sizes <- vapply(blocks, nrow, integer(1))
  working <- matrix(0, sum(sizes), sum(sizes))
  end <- cumsum(sizes)
  for (b in seq_along(pooled)) {
    at <- seq(end[b] - sizes[b] + 1, end[b])
    working[at, at] <- groups[[pooled[b]]]$M
  }
  data <- dplyr::bind_rows(blocks)
  data$node <- factor(data$node, levels = pooled)
  fit <- tryCatch(
    metafor::rma.mv(
      estimate,
      working,
      mods = ~ 0 + node,
      data = data,
      method = "FE",
      test = "t"
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) {
    return(result(paste("model failed:", fit)))
  }
  weights <- matrix(1 / length(pooled), nrow = 1, ncol = length(pooled))
  robust <- tryCatch(
    clubSandwich::linear_contrast(
      fit,
      vcov = "CR2",
      cluster = data$dataset_id,
      contrasts = weights
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(result(paste("robust inference failed:", robust)))
  }

  nodes <- as.numeric(fit$b)
  disagree <- any(nodes > 0) && any(nodes < 0)
  spread <- max(nodes) - min(nodes)
  wide <- spread > settings$smallest_effect_per_hour
  warning <- c(
    if (disagree) "specific outcomes disagree in sign",
    if (wide) "specific outcomes differ by more than the smallest effect"
  )
  result(
    if (robust$df[1] < 4) "too few studies to say" else "pooled",
    estimate = robust$Est[1],
    se = robust$SE[1],
    df = robust$df[1],
    ci_low = robust$CI_L[1],
    ci_high = robust$CI_U[1],
    node_low = min(nodes),
    node_high = max(nodes),
    warning = if (length(warning) > 0) {
      paste(warning, collapse = "; ")
    } else {
      NA_character_
    }
  )
}
