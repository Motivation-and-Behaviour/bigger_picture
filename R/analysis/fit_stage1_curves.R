#' Stage 1 for curves: a spline of screen use in each unit
#'
#' Within each stage 1 unit, regresses an outcome on a natural cubic spline
#' of screen use with knots fixed in hours (`curve_basis()`), so every unit
#' estimates the same curve terms and stage 2 can pool them
#' (ANALYSIS_PLAN.md, Step 11 task 2). Rows, covariates, weights and
#' clusters are those of the linear stage 1 model at tier 1
#' (`stage1_design()`).
#'
#' Curves are fitted for rows that take a linear model: continuous rows and
#' ordinal rows with five or more levels. A unit needs more distinct values
#' of screen use than the curve has terms, which a banded item with few
#' bands does not have; a minimum number of rows in every stretch between
#' knots (`curve_interval_rows`), so that no part of its curve rests on a
#' handful of children; and, where its errors are clustered, more clusters
#' than curve terms.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param cells A tibble of `exposure` and `outcome` rows to fit.
#' @param outcome_registry The outcome registry.
#' @param dataschema The dataschema, for the level order of ordinal rows.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per unit and cell that has data: `coef` and
#'   `vcov` (list columns: the curve terms and their covariance), the rows
#'   behind them, `hours_distinct`, `rows_least_stretch` (the rows in its
#'   thinnest stretch between knots), `rows_near` (a list column: the rows
#'   within `curve_window` hours of each hour of `curve_grid`), the
#'   clusters, and `status`. NULL when the dataset is not included or holds no cell.
fit_stage1_curves <- function(
  prepared,
  cells,
  outcome_registry,
  dataschema,
  settings = default_analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  cells <- cells[
    cells$exposure %in%
      names(prepared$exposures) &
      cells$outcome %in% names(prepared$outcomes),
  ]
  if (nrow(cells) == 0) {
    return(NULL)
  }
  registry <- outcome_registry$rows
  column <- function(name) {
    if (name %in% names(base)) base[[name]] else rep(NA, nrow(base))
  }
  frame <- data.frame(
    unit = base$unit,
    age = as.numeric(base$age_years),
    sex = as.character(column("sex")),
    weight = as.numeric(base$survey_weight_norm),
    cluster = as.character(column("survey_cluster")),
    family = as.character(column("family_id")),
    stringsAsFactors = FALSE
  )
  unit_rows <- split(seq_len(nrow(frame)), frame$unit)

  results <- list()
  for (i in seq_len(nrow(cells))) {
    exposure <- cells$exposure[i]
    outcome <- cells$outcome[i]
    score_type <- registry$score_type[match(outcome, registry$variable_name)]
    model <- stage1_model_type(
      score_type,
      schema_level_count(outcome, dataschema)
    )
    if (model != "linear") {
      next
    }
    x <- prepared$exposures[[exposure]]
    y <- if (outcome %in% names(prepared$outcomes_z)) {
      prepared$outcomes_z[[outcome]]
    } else {
      standardise_within(
        prepared$outcomes[[outcome]],
        base$unit,
        settings$min_unit_rows
      )
    }
    for (unit in names(unit_rows)) {
      index <- unit_rows[[unit]]
      index <- index[!is.na(x[index]) & !is.na(y[index])]
      if (length(index) == 0) {
        next
      }
      data <- frame[index, ]
      data$x <- x[index]
      data$y <- y[index]
      fit <- fit_curve_unit(data, settings)
      fit$unit <- unit
      fit$exposure <- exposure
      fit$outcome <- outcome
      results[[length(results) + 1]] <- fit
    }
  }
  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    return(NULL)
  }
  units <- dplyr::distinct(base[c("unit", "dataset_id", "wave", "country")])
  out <- dplyr::left_join(out, units, by = "unit")
  dplyr::relocate(
    out,
    "dataset_id",
    "unit",
    "wave",
    "country",
    "exposure",
    "outcome"
  )
}

#' The curve terms at given hours of screen use
#'
#' A natural cubic spline with interior and boundary knots fixed in hours,
#' so the terms mean the same in every unit. Beyond the boundary knots the
#' curve is a straight line.
#'
#' @param hours Hours of screen use a day.
#' @param settings From `analysis_settings()`.
#' @return A matrix with one row per element of `hours` and one column per
#'   curve term, named `s1`, `s2`, and so on.
curve_basis <- function(hours, settings = default_analysis_settings()) {
  basis <- splines::ns(
    hours,
    knots = settings$curve_knots,
    Boundary.knots = settings$curve_boundary
  )
  basis <- matrix(basis, nrow = length(hours))
  colnames(basis) <- paste0("s", seq_len(ncol(basis)))
  basis
}

# Fit one unit's curve and return a one-row tibble.
fit_curve_unit <- function(data, settings = default_analysis_settings()) {
  grid <- settings$curve_grid
  available <- nrow(data)
  design <- stage1_design(data, settings, tier = 1L)
  data <- design$data
  n <- nrow(data)
  # The stretches between knots, the last one ending at the upper boundary
  # knot: rows beyond it do not count towards it.
  breaks <- c(-Inf, settings$curve_knots, settings$curve_boundary[2])
  stretch <- if (n == 0) {
    0L
  } else {
    min(table(cut(data$x, breaks, right = TRUE)))
  }
  result <- function(status, coef = NULL, vcov = NULL, ...) {
    values <- list(
      n = n,
      n_available = available,
      hours_distinct = dplyr::n_distinct(data$x),
      hours_max = if (n == 0) NA_real_ else max(data$x),
      rows_least_stretch = as.integer(stretch),
      weighted = design$weighted,
      covariates = paste(design$covariates, collapse = " + "),
      n_clusters = NA_integer_,
      few_clusters = NA,
      se_type = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    out <- tibble::as_tibble(values)
    out$coef <- list(coef)
    out$vcov <- list(vcov)
    out$rows_near <- list(vapply(
      grid,
      function(hour) sum(abs(data$x - hour) <= settings$curve_window),
      numeric(1)
    ))
    out
  }
  if (n < settings$min_unit_rows) {
    return(result("fewer rows than the minimum unit size"))
  }
  basis <- curve_basis(data$x, settings)
  terms <- colnames(basis)
  if (dplyr::n_distinct(data$x) <= length(terms)) {
    return(result("too few distinct values of screen use for a curve"))
  }
  if (stretch < settings$curve_interval_rows) {
    return(result("too few rows in a stretch between knots for a curve"))
  }
  if (stats::sd(data$y) == 0) {
    return(result("no variation in the outcome"))
  }
  data[terms] <- basis
  clustering <- stage1_cluster(data)
  clustered <- !is.null(clustering$cluster)
  n_clusters <- dplyr::n_distinct(clustering$cluster_id)
  # A curve's covariance from a handful of clusters is close to singular.
  if (clustered && n_clusters < settings$min_clusters) {
    return(result(
      "too few clusters for a curve",
      n_clusters = n_clusters,
      few_clusters = TRUE
    ))
  }
  data$cluster_id <- clustering$cluster_id
  data$w <- if (design$weighted) data$weight else rep(1, n)
  formula <- stats::as.formula(paste(
    "y ~",
    paste(c(terms, setdiff(design$terms, "x")), collapse = " + ")
  ))
  vcov <- if (is.null(clustering$cluster)) {
    "hetero"
  } else {
    stats::as.formula("~cluster_id")
  }
  fit <- tryCatch(
    fixest::feols(
      formula,
      data = data,
      weights = ~w,
      vcov = vcov,
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) {
    return(result(paste("model failed:", fit)))
  }
  estimated <- stats::coef(fit)
  if (!all(terms %in% names(estimated))) {
    return(result("curve not estimable in the unit"))
  }
  covariance <- stats::vcov(fit)[terms, terms, drop = FALSE]
  if (anyNA(covariance) || any(diag(covariance) <= 0)) {
    return(result("curve not estimable in the unit"))
  }
  result(
    "fitted",
    coef = estimated[terms],
    vcov = covariance,
    n_clusters = if (clustered) n_clusters else NA_integer_,
    few_clusters = clustered && n_clusters < settings$min_clusters,
    se_type = clustering$se_type
  )
}
