#' Stage 1: one model per unit, for each exposure and outcome cell
#'
#' Within each stage 1 unit (dataset by wave by country), regresses an
#' outcome on screen use in hours per day plus the covariate tier, and keeps
#' the slope on screen use (DR-0009, DR-0010). Each score type has its own
#' model and effect measure:
#'
#' - continuous rows: a linear model of the standardised score, giving SD
#'   per hour;
#' - ordinal rows with five or more levels: the same, on the level's position
#'   standardised within the unit;
#' - ordinal rows with three or four levels: a proportional-odds model,
#'   giving log odds per hour of a higher level;
#' - binary rows: a logistic model, giving log odds per hour.
#'
#' Design rules:
#'
#' - weights are used where the unit has them, normalised within unit, and a
#'   row with no weight or a zero weight in a weighted unit is left out;
#' - standard errors are clustered on the sampling unit where one is recorded
#'   on every row, otherwise on the family where families repeat, otherwise
#'   they are heteroskedasticity-robust; strata are not used, which errs
#'   towards wider intervals;
#' - age enters as a natural spline with two degrees of freedom where the
#'   unit spans two or more years of age, as categories where it takes five
#'   or fewer distinct values, linearly otherwise, and not at all where it
#'   does not vary; sex enters where both sexes have at least five rows;
#' - a unit is fitted on complete cases, and only with at least the minimum
#'   number of rows, variation in the exposure and, for a binary outcome, at
#'   least ten rows in each class.
#'
#' A unit that cannot be fitted returns a row saying why, so nothing drops
#' out silently.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param cells A tibble of `exposure` and `outcome` rows to fit.
#' @param outcome_registry The outcome registry.
#' @param dataschema The dataschema, for the level order of ordinal rows.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per unit and cell that has data: the slope
#'   (`estimate`, `se`), its `effect_measure`, the rows, clusters and degrees
#'   of freedom behind it, the covariates and engine used, and `status`
#'   (`fitted` or the reason it was not). NULL when the dataset is not
#'   included or holds none of the cells.
fit_stage1 <- function(
  prepared,
  cells,
  outcome_registry,
  dataschema,
  settings = analysis_settings()
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
  levels_of <- function(variable) {
    allowed <- dataschema$allowed_values[match(
      variable,
      dataschema$variable_name
    )]
    length(setdiff(strsplit(allowed, "|", fixed = TRUE)[[1]], "Unknown"))
  }
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
    model <- stage1_model_type(score_type, levels_of(outcome))
    x <- prepared$exposures[[exposure]]
    y <- if (model == "linear" && outcome %in% names(prepared$outcomes_z)) {
      prepared$outcomes_z[[outcome]]
    } else {
      prepared$outcomes[[outcome]]
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
      fit <- fit_stage1_unit(data, model, settings)
      fit$unit <- unit
      fit$exposure <- exposure
      fit$outcome <- outcome
      fit$score_type <- score_type
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
    "outcome",
    "score_type"
  )
}

# The model a score type takes. Ordinal rows with five or more levels are
# treated as numeric within unit. Diagnosis and nominal rows are not fitted
# here: diagnoses are analysed on their own and nominal rows only described.
stage1_model_type <- function(score_type, n_levels) {
  if (score_type %in% "continuous") {
    return("linear")
  }
  if (score_type %in% "binary") {
    return("logistic")
  }
  if (score_type %in% "ordinal") {
    return(if (n_levels >= 5) "linear" else "ordinal")
  }
  "none"
}

# Fit one unit's model and return a one-row tibble.
fit_stage1_unit <- function(data, model, settings = analysis_settings()) {
  result <- function(status, ...) {
    values <- list(
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      n = nrow(data),
      n_clusters = NA_integer_,
      weighted = FALSE,
      se_type = NA_character_,
      covariates = NA_character_,
      effect_measure = NA_character_,
      engine = NA_character_,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (model == "none") {
    return(result("score type not modelled here"))
  }

  # Weights: used where the unit has them, on the rows that have one.
  weighted <- any(!is.na(data$weight) & data$weight > 0)
  if (weighted) {
    data <- data[!is.na(data$weight) & data$weight > 0, ]
  }

  # Covariates the unit can support.
  terms <- "x"
  covariates <- character()
  age_span <- if (all(is.na(data$age))) {
    0
  } else {
    diff(stats::quantile(data$age, c(0.05, 0.95), na.rm = TRUE, names = FALSE))
  }
  age_values <- dplyr::n_distinct(data$age, na.rm = TRUE)
  spline_works <- age_span >= 2 &&
    !inherits(try(splines::ns(data$age, df = 2), silent = TRUE), "try-error")
  if (age_values > 1 && age_values <= 5) {
    # A few distinct ages, as where age comes from the school grade: a
    # category each is the most flexible form, and a spline has no room for
    # its knot.
    terms <- c(terms, "factor(age)")
    covariates <- c(covariates, "age (categories)")
  } else if (spline_works) {
    terms <- c(terms, "splines::ns(age, df = 2)")
    covariates <- c(covariates, "age (spline)")
  } else if (age_values > 1) {
    terms <- c(terms, "age")
    covariates <- c(covariates, "age")
  }
  if (age_values > 1) {
    data <- data[!is.na(data$age), ]
  }
  sex_counts <- table(data$sex[data$sex %in% c("Female", "Male")])
  if (length(sex_counts) == 2 && min(sex_counts) >= 5) {
    data <- data[data$sex %in% c("Female", "Male"), ]
    terms <- c(terms, "sex")
    covariates <- c(covariates, "sex")
  }

  n <- nrow(data)
  if (n < settings$min_unit_rows) {
    return(result("fewer rows than the minimum unit size", n = n))
  }
  if (stats::sd(data$x) == 0) {
    return(result("no variation in the exposure", n = n))
  }
  if (model == "linear") {
    # A numeric ordinal score is put on the unit's own SD scale.
    spread <- stats::sd(data$y)
    if (is.na(spread) || spread == 0) {
      return(result("no variation in the outcome", n = n))
    }
    data$y <- (data$y - mean(data$y)) / spread
  }
  if (
    model == "logistic" && min(table(factor(data$y, levels = c(0, 1)))) < 10
  ) {
    return(result("fewer than 10 rows in one class", n = n))
  }
  if (model == "ordinal") {
    data$y <- factor(data$y, ordered = TRUE)
    if (nlevels(data$y) < 3 || min(table(data$y)) < 5) {
      return(result("too few rows in one level", n = n))
    }
  }

  # Clustering: the sampling unit, else the family, else none.
  cluster <- NULL
  se_type <- "heteroskedasticity-robust"
  if (!anyNA(data$cluster) && dplyr::n_distinct(data$cluster) > 1) {
    cluster <- data$cluster
    se_type <- "clustered on sampling unit"
  } else if (
    !anyNA(data$family) && dplyr::n_distinct(data$family) < nrow(data)
  ) {
    cluster <- data$family
    se_type <- "clustered on family"
  }
  data$cluster_id <- if (is.null(cluster)) {
    as.character(seq_len(n))
  } else {
    cluster
  }
  n_clusters <- dplyr::n_distinct(data$cluster_id)
  data$w <- if (weighted) data$weight else rep(1, n)
  formula <- stats::as.formula(paste("y ~", paste(terms, collapse = " + ")))

  common <- list(
    n = n,
    n_clusters = if (is.null(cluster)) NA_integer_ else n_clusters,
    weighted = weighted,
    se_type = se_type,
    covariates = paste(covariates, collapse = " + ")
  )
  fitted <- tryCatch(
    switch(
      model,
      linear = stage1_fixest(data, formula, cluster, "linear"),
      logistic = stage1_fixest(data, formula, cluster, "logistic"),
      ordinal = stage1_ordinal(data, formula)
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fitted)) {
    return(do.call(
      result,
      c(list(status = paste("model failed:", fitted)), common)
    ))
  }
  if (is.na(fitted$estimate) || is.na(fitted$se) || fitted$se <= 0) {
    return(do.call(result, c(list(status = "slope not estimable"), common)))
  }
  measure <- if (model == "linear") "sd_per_hour" else "log_odds_per_hour"
  parameters <- length(terms) + 2 + max(age_values - 2, 0) * (age_values <= 5)
  do.call(
    result,
    c(
      list(
        status = "fitted",
        estimate = fitted$estimate,
        se = fitted$se,
        df = if (is.null(cluster)) n - parameters else n_clusters - 1,
        effect_measure = measure,
        engine = fitted$engine
      ),
      common
    )
  )
}

# Linear and logistic models, with weights and clustered or robust errors.
stage1_fixest <- function(data, formula, cluster, model) {
  vcov <- if (is.null(cluster)) "hetero" else stats::as.formula("~cluster_id")
  fit <- if (model == "linear") {
    fixest::feols(
      formula,
      data = data,
      weights = ~w,
      vcov = vcov,
      notes = FALSE,
      warn = FALSE
    )
  } else {
    fixest::feglm(
      formula,
      data = data,
      weights = ~w,
      family = stats::quasibinomial(),
      vcov = vcov,
      notes = FALSE,
      warn = FALSE
    )
  }
  table <- fixest::coeftable(fit)
  if (!"x" %in% rownames(table)) {
    return(list(estimate = NA_real_, se = NA_real_, engine = "fixest"))
  }
  list(
    estimate = unname(table["x", "Estimate"]),
    se = unname(table["x", "Std. Error"]),
    engine = "fixest"
  )
}

# A proportional-odds model through the survey package, which gives design
# based errors with weights and clusters. The outcome must be an ordered
# factor: `svyolr()` accepts an unordered one without complaint.
stage1_ordinal <- function(data, formula) {
  stopifnot(is.ordered(data$y))
  design <- survey::svydesign(
    ids = ~cluster_id,
    weights = ~w,
    data = data
  )
  fit <- survey::svyolr(formula, design = design)
  table <- stats::coef(summary(fit))
  list(
    estimate = unname(table["x", "Value"]),
    se = unname(table["x", "Std. Error"]),
    engine = "survey"
  )
}
