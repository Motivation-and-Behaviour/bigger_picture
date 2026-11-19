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
#' within and across specific outcomes, so a score that sits in two
#' specific outcomes adds nothing to the family's precision: it counts
#' once. For a family of several specific outcomes it is the jackknife
#' form of the robust variance (CR3), which is a little larger than the
#' form used for a single specific outcome (CR2, which the test that the
#' specific outcomes are equal also keeps). On the real design, with CR2, a
#' family's 95% interval covered the truth about 93% of the time, and with
#' CR3 95 to 97%; CR3 is conservative where datasets differ little
#' (DR-0014).
#'
#' The family is estimated whenever one of its specific outcomes pooled, and
#' with one it is that specific outcome. A specific outcome that could not
#' be pooled is left out and named in `nodes_left_out`.
#'
#' Only evidence that the specific outcomes disagree keeps a family from the
#' headline (DR-0014), recorded in `warning`:
#'
#' - two specific outcomes have intervals on opposite sides of zero;
#' - the specific outcomes' estimates span more than the smallest effect of
#'   interest and a robust test rejects that they are equal. The span alone
#'   is not enough: estimates of equal effects differ by chance, the more so
#'   the more of them there are.
#'
#' Uncertainty does not: a family on one specific outcome, one whose test
#' could not be run and one with too few studies to say are reported as
#' such, in `note` and `status`, and stay in the headline.
#'
#' Beside the plain average the row gives the average weighted by each
#' specific outcome's number of datasets, as a sensitivity analysis. Those
#' weights come from what was measured, not from the estimates.
#'
#' @param effects Rows from `tree_effects()` for one family.
#' @param groups A named list of `pool_tree_group()` results, one per node,
#'   each fitted on that node's rows of `effects` in their order.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble: the number of specific outcomes averaged and
#'   how many of those had too few studies to say, the `estimate` with its
#'   robust `se`, `df` and 95% interval, the range of the specific outcomes'
#'   estimates, the p-value of the test that they are equal, `warning`,
#'   `note`, `headline`, `status`, and the same estimate weighted by
#'   datasets in the `_by_datasets` columns.
pool_tree_family <- function(
  effects,
  groups,
  settings = default_analysis_settings()
) {
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
      note = NA_character_,
      headline = FALSE,
      status = status,
      estimate_by_datasets = NA_real_,
      se_by_datasets = NA_real_,
      df_by_datasets = NA_real_,
      df_floor = NA_real_,
      df_floor_by_datasets = NA_real_,
      ci_low_by_datasets = NA_real_,
      ci_high_by_datasets = NA_real_
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
      note = "one specific outcome only",
      headline = TRUE,
      estimate_by_datasets = rows$estimate,
      se_by_datasets = rows$se,
      df_by_datasets = rows$df,
      df_floor = rows$df,
      df_floor_by_datasets = rows$df,
      ci_low_by_datasets = rows$ci_low,
      ci_high_by_datasets = rows$ci_high
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
  average <- function(weights, form = "CR3") {
    tryCatch(
      clubSandwich::linear_contrast(
        fit,
        vcov = form,
        cluster = data$dataset_id,
        contrasts = matrix(weights / sum(weights), nrow = 1)
      ),
      error = function(e) conditionMessage(e)
    )
  }
  robust <- average(rep(1, length(pooled)))
  if (is.character(robust)) {
    return(result(paste("robust inference failed:", robust)))
  }
  if (is.na(robust$df[1]) || is.na(robust$SE[1])) {
    return(result("robust inference failed: no degrees of freedom"))
  }
  by_datasets <- average(rows$k_datasets)
  if (is.character(by_datasets)) {
    by_datasets <- list(
      Est = NA_real_,
      SE = NA_real_,
      df = NA_real_,
      CI_L = NA_real_,
      CI_U = NA_real_
    )
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
    if (wide && !is.na(equal) && equal < 0.05) {
      "specific outcomes differ by more than the smallest effect"
    }
  )
  note <- if (wide && is.na(equal)) {
    "whether the specific outcomes differ could not be tested"
  } else {
    NA_character_
  }
  # Whether there are too few studies to say is judged on the degrees of
  # freedom of the form used for a single specific outcome (CR2), which the
  # floor was set on: the jackknife form's are systematically lower.
  floor_df <- function(weights, fallback) {
    plain <- average(weights, "CR2")
    if (is.character(plain) || is.na(plain$df[1])) fallback else plain$df[1]
  }
  df_floor <- floor_df(rep(1, length(pooled)), robust$df[1])
  status <- if (df_floor < settings$min_robust_df) {
    "too few studies to say"
  } else {
    "pooled"
  }
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
    note = note,
    headline = length(warning) == 0,
    estimate_by_datasets = by_datasets$Est[1],
    se_by_datasets = by_datasets$SE[1],
    df_by_datasets = by_datasets$df[1],
    df_floor = df_floor,
    df_floor_by_datasets = floor_df(rows$k_datasets, by_datasets$df[1]),
    ci_low_by_datasets = by_datasets$CI_L[1],
    ci_high_by_datasets = by_datasets$CI_U[1]
  )
}
