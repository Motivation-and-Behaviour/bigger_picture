#' Whether a specific outcome's slopes differ between the levels of a label
#'
#' Fits the working model of `pool_tree_group()` with one mean per level of
#' a categorical moderator, and tests whether the means are equal with a
#' robust small-sample test clustered on dataset. A level enters only with
#' the minimum number of datasets, and the comparison needs two such levels.
#'
#' Most datasets have one level of a moderator, so the comparison is mostly
#' between datasets, which also differ in country, age and instrument: a
#' difference says the slopes differ between kinds of study, not that the
#' label is the cause. `k_datasets_several` counts the datasets with more
#' than one of the compared levels, which is what a within-study comparison
#' would rest on, and `largest_dataset_share` is the share of the compared
#' slopes that one dataset supplies.
#'
#' The comparison has little power: for a true difference of one smallest
#' effect of interest it found the difference about a third of the time in
#' designs like the development cells'. A test that does not reject is not
#' evidence of no difference. P-values here are not adjusted for the number
#' of comparisons.
#'
#' @param effects Rows from `tree_moderators()` for one group.
#' @param moderator The name of a moderator column.
#' @param correlations See `tree_covariance()`.
#' @param settings From `analysis_settings()`.
#' @return One row per level (one row with an NA level where no slope has
#'   the label): its datasets and slopes, `estimate` with a robust `se`,
#'   `df` and 95% interval, and `level_status`; and on every row the number
#'   of levels compared, `equal_p` (the test that the levels' means are
#'   equal) with its degrees of freedom and `test_status` (`tested`, or `too
#'   few studies to say` under four degrees of freedom), for two levels the
#'   `difference` with its interval and `difference_of` naming which level
#'   is taken from which, and `status`.
moderate_tree_group <- function(
  effects,
  moderator,
  correlations = NULL,
  settings = analysis_settings()
) {
  effects$level <- as.character(effects[[moderator]])
  effects <- effects[!is.na(effects$level), ]
  counts <- dplyr::summarise(
    dplyr::group_by(effects, .data$level),
    k_datasets = dplyr::n_distinct(.data$dataset_id),
    k_slopes = dplyr::n(),
    .groups = "drop"
  )
  counts <- counts[order(counts$level), ]
  enough <- counts$level[counts$k_datasets >= settings$min_datasets]
  compared <- effects[effects$level %in% enough, ]
  per_dataset <- tapply(compared$level, compared$dataset_id, dplyr::n_distinct)
  out <- counts
  out$estimate <- NA_real_
  out$se <- NA_real_
  out$df <- NA_real_
  out$ci_low <- NA_real_
  out$ci_high <- NA_real_
  out$level_status <- ifelse(
    out$level %in% enough,
    NA_character_,
    "insufficient data"
  )
  out$k_levels <- length(enough)
  out$k_datasets_several <- sum(per_dataset > 1)
  out$largest_dataset_share <- if (nrow(compared) == 0) {
    NA_real_
  } else {
    max(table(compared$dataset_id)) / nrow(compared)
  }
  out$equal_p <- NA_real_
  out$equal_df <- NA_real_
  out$test_status <- NA_character_
  out$difference_of <- NA_character_
  out$difference <- NA_real_
  out$difference_ci_low <- NA_real_
  out$difference_ci_high <- NA_real_
  out$difference_df <- NA_real_
  finish <- function(status) {
    out$status <- rep(status, nrow(out))
    out
  }
  if (nrow(out) == 0) {
    out <- out[NA_integer_, ]
    out$k_datasets <- 0L
    out$k_slopes <- 0L
    out$k_levels <- 0L
    out$k_datasets_several <- 0L
    return(finish("no slope has this label"))
  }
  if (length(enough) < 2) {
    return(finish("fewer than two levels with enough datasets"))
  }

  used <- compared
  used$level <- factor(used$level, levels = enough)
  working <- fit_tree_model(used, correlations, settings, mods = ~ 0 + level)
  if (is.character(working)) {
    return(finish(paste("model failed:", working)))
  }
  cluster <- working$data$dataset_id
  robust <- tryCatch(
    clubSandwich::conf_int(working$fit, vcov = "CR2", cluster = cluster),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(finish(paste("robust inference failed:", robust)))
  }
  at <- match(enough, out$level)
  out$estimate[at] <- robust$beta
  out$se[at] <- robust$SE
  out$df[at] <- robust$df
  out$ci_low[at] <- robust$CI_L
  out$ci_high[at] <- robust$CI_U
  out$level_status[at] <- ifelse(
    robust$df < 4,
    "too few studies to say",
    "estimated"
  )
  equal <- tryCatch(
    clubSandwich::Wald_test(
      working$fit,
      constraints = clubSandwich::constrain_equal(seq_along(enough)),
      vcov = "CR2",
      cluster = cluster,
      test = "HTZ"
    ),
    error = function(e) NULL
  )
  if (!is.null(equal)) {
    out$equal_p <- equal$p_val[1]
    out$equal_df <- equal$df_denom[1]
  }
  if (length(enough) == 2) {
    contrast <- tryCatch(
      clubSandwich::linear_contrast(
        working$fit,
        vcov = "CR2",
        cluster = cluster,
        contrasts = matrix(c(-1, 1), nrow = 1)
      ),
      error = function(e) NULL
    )
    if (!is.null(contrast)) {
      out$difference_of <- paste(enough[2], "minus", enough[1])
      out$difference <- contrast$Est[1]
      out$difference_ci_low <- contrast$CI_L[1]
      out$difference_ci_high <- contrast$CI_U[1]
      out$difference_df <- contrast$df[1]
    }
  }
  out$test_status <- if (all(is.na(out$equal_p))) {
    "could not be tested"
  } else if (out$equal_df[1] < 4) {
    "too few studies to say"
  } else {
    "tested"
  }
  finish("compared")
}
