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
#' The family is estimated whenever one of its specific outcomes pooled, and
#' with one it is that specific outcome. A specific outcome that could not
#' be pooled is left out and named in `nodes_left_out`. Three things keep a
#' family from the headline, each recorded in `warning`:
#'
#' - it rests on one specific outcome;
#' - two specific outcomes have intervals on opposite sides of zero;
#' - the specific outcomes' estimates span more than the smallest effect of
#'   interest and a robust test rejects that they are equal (or the test
#'   could not be run). The span alone is not enough: estimates of equal
#'   effects differ by chance, the more so the more of them there are.
#'
#' @param effects Rows from `tree_effects()` for one family.
#' @param groups A named list of `pool_tree_group()` results, one per node,
#'   each fitted on that node's rows of `effects` in their order.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble: the number of specific outcomes averaged and
#'   how many of those had too few studies to say, the `estimate` with its
#'   robust `se`, `df` and 95% interval, the range of the specific outcomes'
#'   estimates, the p-value of the test that they are equal, `warning`,
#'   `headline` and `status`.
pool_tree_family <- function(effects, groups, settings = analysis_settings()) {
  is_pooled <- vapply(
    groups,
    function(group) !is.null(group$M) && !is.na(group$row$estimate),
    logical(1)
  )
  pooled <- names(groups)[is_pooled]
  left_out <- if (all(is_pooled)) {
    NA_character_
  } else {
    paste(
      sprintf(
        "%s (%s)",
        names(groups)[!is_pooled],
        vapply(groups[!is_pooled], function(g) g$row$status, character(1))
      ),
      collapse = "; "
    )
  }
  rows <- dplyr::bind_rows(lapply(groups[pooled], function(g) g$row))
  effects <- effects[effects$node %in% pooled, ]
  result <- function(status, ...) {
    values <- list(
      k_nodes = length(pooled),
      k_nodes_thin = sum(rows[["status"]] %in% "too few studies to say"),
      k_datasets = dplyr::n_distinct(effects$dataset_id),
      k_slopes = nrow(effects),
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      node_low = NA_real_,
      node_high = NA_real_,
      nodes_differ_p = NA_real_,
      nodes_left_out = left_out,
      warning = NA_character_,
      headline = FALSE,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (length(pooled) == 0) {
    return(result("no specific outcome pooled"))
  }
  if (length(pooled) == 1) {
    return(result(
      rows$status,
      estimate = rows$estimate,
      se = rows$se,
      df = rows$df,
      ci_low = rows$ci_low,
      ci_high = rows$ci_high,
      node_low = rows$estimate,
      node_high = rows$estimate,
      warning = "one specific outcome only"
    ))
  }

  # One block per specific outcome, in the order its group was fitted.
  blocks <- lapply(pooled, function(node) effects[effects$node == node, ])
  for (b in seq_along(pooled)) {
    keys <- paste(blocks[[b]]$unit, blocks[[b]]$outcome, sep = "\r")
    if (!identical(keys, groups[[pooled[b]]]$keys)) {
      stop(
        "pool_tree_family(): the rows of '",
        pooled[b],
        "' are not the rows, in order, its group was fitted on.",
        call. = FALSE
      )
    }
  }
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
  equal <- tryCatch(
    clubSandwich::Wald_test(
      fit,
      constraints = clubSandwich::constrain_equal(seq_along(pooled)),
      vcov = "CR2",
      cluster = data$dataset_id,
      test = "HTZ"
    )$p_val[1],
    error = function(e) NA_real_
  )

  nodes <- as.numeric(fit$b)
  opposed <- any(rows$ci_low > 0, na.rm = TRUE) &&
    any(rows$ci_high < 0, na.rm = TRUE)
  wide <- max(nodes) - min(nodes) > settings$smallest_effect_per_hour
  warning <- c(
    if (opposed) "specific outcomes on opposite sides of zero",
    if (wide && is.na(equal)) {
      "specific outcomes span more than the smallest effect; not testable"
    },
    if (wide && !is.na(equal) && equal < 0.05) {
      "specific outcomes differ by more than the smallest effect"
    }
  )
  status <- if (robust$df[1] < 4) "too few studies to say" else "pooled"
  result(
    status,
    estimate = robust$Est[1],
    se = robust$SE[1],
    df = robust$df[1],
    ci_low = robust$CI_L[1],
    ci_high = robust$CI_U[1],
    node_low = min(nodes),
    node_high = max(nodes),
    nodes_differ_p = equal,
    warning = if (length(warning) > 0) {
      paste(warning, collapse = "; ")
    } else {
      NA_character_
    },
    headline = status == "pooled" && length(warning) == 0
  )
}
