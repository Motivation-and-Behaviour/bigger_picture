#' Moderator tests as rows for the results table
#'
#' Puts the tests of `moderate_tree()` and `trend_tree()` in one shape, so
#' that `assemble_results()` can count them in the tiered false discovery
#' rate and give each its q-value. A categorical moderator is one test per
#' exposure and specific outcome or family: that its levels' means are
#' equal. The year trend is two: the between-dataset and the within-series
#' slope. A moderator that sorts the slopes exactly as an earlier one does
#' (`same_as`) is the same test, and is left out.
#'
#' The comparison of same-reporter with different-reporter slopes is its
#' own row of the model list (`sens_reporter`); every other categorical
#' moderator is `moderation_study_level`.
#'
#' @param moderation Rows of `moderate_tree()`, or NULL.
#' @param trend Rows of `trend_tree()`, or NULL.
#' @return A tibble, one row per test: `model_name`, `exposure`, `level`
#'   (L0 or L1), `family`, `node`, `moderator`, `term`, `effect_measure`,
#'   `estimate` with its interval and `df` where the test has one number
#'   (the difference between two levels, or the trend per decade),
#'   `p_test` and `status` (`pooled` for a test that was run, `too few
#'   studies to say`, or why it was not run).
moderator_tests <- function(moderation = NULL, trend = NULL) {
  status_of <- function(run, few, otherwise) {
    dplyr::case_when(
      run ~ "pooled",
      few ~ "too few studies to say",
      .default = otherwise
    )
  }
  compared <- tibble::tibble()
  if (!is.null(moderation) && nrow(moderation) > 0) {
    own <- moderation[is.na(moderation$same_as), ]
    key <- c("exposure", "tree_level", "family", "node", "moderator")
    own <- own[!duplicated(own[key]), ]
    compared <- tibble::tibble(
      model_name = ifelse(
        own$moderator %in% "reporter",
        "sens_reporter",
        "moderation_study_level"
      ),
      exposure = own$exposure,
      level = own$tree_level,
      family = own$family,
      node = own$node,
      moderator = own$moderator,
      term = "levels equal",
      effect_measure = "sd_per_hour_difference",
      estimate = own$difference,
      ci_low = own$difference_ci_low,
      ci_high = own$difference_ci_high,
      df = own$equal_df,
      p_test = own$equal_p,
      status = status_of(
        own$test_status %in% "tested",
        own$test_status %in% "too few studies to say",
        dplyr::coalesce(own$test_status, own$status)
      )
    )
    compared$p_test[
      !compared$status %in% c("pooled", "too few studies to say")
    ] <-
      NA_real_
  }
  trends <- tibble::tibble()
  if (!is.null(trend) && nrow(trend) > 0) {
    part <- function(name) {
      estimate <- trend[[paste0(name, "_per_decade")]]
      low <- trend[[paste0(name, "_ci_low")]]
      high <- trend[[paste0(name, "_ci_high")]]
      df <- trend[[paste0(name, "_df")]]
      state <- trend[[paste0(name, "_status")]]
      se <- (high - low) / (2 * stats::qt(0.975, df))
      tibble::tibble(
        model_name = "year_trend",
        exposure = trend$exposure,
        level = trend$tree_level,
        family = trend$family,
        node = trend$node,
        moderator = "data_year",
        term = paste(name, if (name == "between") "datasets" else "series"),
        effect_measure = "sd_per_hour_per_decade",
        estimate = estimate,
        ci_low = low,
        ci_high = high,
        df = df,
        p_test = 2 * stats::pt(-abs(estimate / se), df),
        status = status_of(
          state %in% "estimated",
          state %in% "too few studies to say",
          ifelse(
            trend$status %in% "fitted",
            "no series spans more than one year",
            trend$status
          )
        )
      )
    }
    trends <- dplyr::bind_rows(part("between"), part("within"))
  }
  dplyr::bind_rows(compared, trends)
}
