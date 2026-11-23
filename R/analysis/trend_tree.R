#' The trend in a pooled association over collection years, up the tree
#'
#' For each exposure, the slopes of a specific outcome (L1), and of a
#' family across its specific outcomes (L0), are regressed on the year
#' their data were collected, as `regress_units_on_year()` does for one
#' outcome row: year is split into a between-dataset part (a dataset's
#' mean year) and a within-series part (a unit's year less the mean of its
#' series, see `assign_series()`), with the units' median age in the model
#' and, where they vary, the response format and who reported. The working
#' model is the tree's (`fit_tree_model()`), so a unit that gives several
#' slopes is allowed for, and at L0 it has a term for each specific
#' outcome. Slopes are aligned and in SD per hour a day, so the trend is in
#' SD per hour per decade. It runs only with enough datasets over enough
#' years.
#'
#' The between-dataset part compares studies run in different periods,
#' which differ in much else. The within-series part compares like with
#' like, but in practice rests on the one or two datasets with long
#' series: it had about one robust degree of freedom in every group of the
#' development cells, so it is reported as a description and is not
#' tested. A series can be a panel's waves within a school stage as well
#' as a repeated cross-section, and there year and age move together.
#'
#' @param estimates Stage 1 rows for any number of exposures.
#' @param units Per-unit descriptions with `unit`, `wave`, `country`,
#'   `year`, `age_median`, `measure_type` and `responder`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param breaks The table from `read_series_breaks()`.
#' @param correlations See `tree_covariance()`.
#' @param settings From `analysis_settings()`.
#' @return One row per exposure and specific outcome or family
#'   (`tree_level` L1 or L0; `node` is NA for a family), with the columns
#'   of `regress_units_on_year()`.
trend_tree <- function(
  estimates,
  units,
  outcome_registry,
  exposure_registry,
  breaks,
  correlations = NULL,
  settings = default_analysis_settings()
) {
  effects <- tree_effects(estimates, outcome_registry, settings)
  effects <- effects[effects$pools, ]
  if (nrow(effects) == 0) {
    return(tibble::tibble())
  }
  at <- match(effects$unit, units$unit)
  for (name in c(
    "wave",
    "country",
    "year",
    "age_median",
    "measure_type",
    "responder"
  )) {
    effects[[name]] <- units[[name]][at]
  }
  registered <- match(effects$exposure, exposure_registry$variable_name)
  exposure_of <- exposure_registry$exposure[registered]
  day_of <- exposure_registry$day[registered]
  effects$series <- NA_character_
  for (exposure in unique(exposure_of)) {
    rows <- exposure_of %in% exposure
    effects$series[rows] <- assign_series(
      effects[rows, ],
      exposure,
      breaks,
      settings,
      day_of[rows]
    )
  }
  effects <- effects[
    order(effects$exposure, effects$family, effects$node, effects$unit),
  ]
  groups <- c(
    lapply(
      split(effects, paste(effects$exposure, effects$node)),
      function(rows) list(rows = rows, level = "L1")
    ),
    lapply(
      split(effects, paste(effects$exposure, effects$family, "L0")),
      function(rows) list(rows = rows, level = "L0")
    )
  )
  # A family whose slopes all sit in one specific outcome has the same
  # trend as that specific outcome, and is not fitted twice.
  groups <- Filter(
    function(group) {
      group$level == "L1" || dplyr::n_distinct(group$rows$node) > 1
    },
    groups
  )
  out <- lapply(groups, function(group) {
    rows <- group$rows
    if ("exposure" %in% names(correlations)) {
      correlations <- correlations[
        correlations$exposure %in% rows$exposure[1],
      ]
    }
    family_level <- group$level == "L0"
    result <- trend_tree_group(
      rows,
      correlations,
      settings,
      adjust = if (family_level) "node"
    )
    result$exposure <- rows$exposure[1]
    result$tree_level <- group$level
    result$family <- rows$family[1]
    result$node <- if (family_level) NA_character_ else rows$node[1]
    result
  })
  dplyr::relocate(
    dplyr::bind_rows(out),
    "exposure",
    "tree_level",
    "family",
    "node"
  )
}

#' The trend over collection years in one group of aligned slopes
#'
#' @param effects Rows from `tree_effects()` for one group, with `year`,
#'   `series`, `age_median`, `measure_type` and `responder`.
#' @param correlations See `tree_covariance()`.
#' @param settings From `analysis_settings()`.
#' @param adjust A column whose values each get a term of their own (the
#'   specific outcome, for a family), or NULL.
#' @return A one-row tibble with the columns of `regress_units_on_year()`.
trend_tree_group <- function(
  effects,
  correlations = NULL,
  settings = default_analysis_settings(),
  adjust = NULL
) {
  effects <- effects[!is.na(effects$year), ]
  k_datasets <- dplyr::n_distinct(effects$dataset_id)
  span <- if (nrow(effects) == 0) NA_real_ else diff(range(effects$year))
  result <- function(status, ...) {
    values <- list(
      k_datasets = k_datasets,
      k_units = dplyr::n_distinct(effects$unit),
      k_slopes = nrow(effects),
      first_year = if (nrow(effects) == 0) NA_real_ else min(effects$year),
      last_year = if (nrow(effects) == 0) NA_real_ else max(effects$year),
      between_per_decade = NA_real_,
      between_ci_low = NA_real_,
      between_ci_high = NA_real_,
      between_df = NA_real_,
      within_per_decade = NA_real_,
      within_ci_low = NA_real_,
      within_ci_high = NA_real_,
      within_df = NA_real_,
      k_within = 0L,
      between_status = NA_character_,
      within_status = NA_character_,
      adjusted_for = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (
    k_datasets < settings$year_min_datasets ||
      is.na(span) ||
      span < settings$year_min_span
  ) {
    return(result("too few datasets or years"))
  }
  built <- year_terms(effects)
  effects <- built$units
  terms <- built$terms
  adjusted <- built$adjusted
  if (!is.null(adjust) && dplyr::n_distinct(effects[[adjust]]) > 1) {
    effects$adjusted_for <- factor(effects[[adjust]])
    terms <- c(terms, "adjusted_for")
    adjusted <- c(adjusted, "specific outcome")
  }
  mods <- stats::as.formula(paste("~", paste(terms, collapse = " + ")))
  design <- stats::model.matrix(mods, effects)
  if (qr(design)$rank < ncol(design)) {
    return(result("year not separable from the model's other terms"))
  }
  working <- fit_tree_model(effects, correlations, settings, mods = mods)
  if (is.character(working)) {
    return(result(paste("model failed:", working)))
  }
  robust <- tryCatch(
    as.data.frame(clubSandwich::conf_int(
      working$fit,
      vcov = "CR2",
      cluster = working$data$dataset_id
    )),
    error = function(e) conditionMessage(e)
  )
  if (is.character(robust)) {
    return(result(paste("robust inference failed:", robust)))
  }
  rownames(robust) <- rownames(working$fit$b)
  pick <- function(term, column) {
    if (term %in% rownames(robust)) robust[term, column] else NA_real_
  }
  result(
    "fitted",
    between_per_decade = pick("decade_between", "beta"),
    between_ci_low = pick("decade_between", "CI_L"),
    between_ci_high = pick("decade_between", "CI_U"),
    between_df = pick("decade_between", "df"),
    within_per_decade = pick("decade_within", "beta"),
    within_ci_low = pick("decade_within", "CI_L"),
    within_ci_high = pick("decade_within", "CI_U"),
    within_df = pick("decade_within", "df"),
    k_within = as.integer(built$k_within),
    between_status = few_to_say(
      pick("decade_between", "df"),
      settings$min_robust_df
    ),
    within_status = few_to_say(
      pick("decade_within", "df"),
      settings$min_robust_df
    ),
    adjusted_for = paste(adjusted, collapse = ", ")
  )
}
