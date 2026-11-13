#' Stage 1: one model per unit, for each exposure and outcome cell
#'
#' Within each stage 1 unit (dataset by wave by country), regresses an
#' outcome on screen use in hours per day plus the covariate tier, and keeps
#' the slope on screen use (DR-0009, DR-0010). Each score type has its own
#' model and effect measure:
#'
#' - continuous rows: a linear model of the score standardised by the
#'   preparation step, giving SD per hour;
#' - ordinal rows with five or more levels in the dataschema: the same, on
#'   the level's position standardised within the unit over every row that
#'   has the outcome, so the scale does not depend on the exposure;
#' - ordinal rows with three or four levels: a proportional-odds model,
#'   giving log odds per hour of a higher level;
#' - binary rows: a logistic model, giving log odds per hour.
#'
#' Design rules:
#'
#' - weights are used where the unit has them, normalised within unit, and a
#'   row with no weight or a zero weight in a weighted unit is left out;
#' - standard errors are clustered on the sampling unit where the unit
#'   records one, a row without one forming a cluster of its own; otherwise
#'   on the family where families repeat; otherwise they are
#'   heteroskedasticity-robust. Strata are not used: against stratified fits
#'   of the same models the standard error was between about 13% smaller and
#'   7% larger, so this is not uniformly conservative;
#' - age enters as categories where it takes five or fewer distinct values,
#'   as a natural spline with two degrees of freedom where the middle 90% of
#'   ages span two or more years and a knot can be placed, linearly
#'   otherwise, and not at all where it does not vary;
#' - sex enters where it is known for at least 90% of the unit's rows and
#'   both sexes have at least five; rows of unknown or other sex are then
#'   left out. Where sex is known for fewer rows it is left out of the model
#'   and the rows stay;
#' - a unit is fitted on complete cases, and only with at least the minimum
#'   number of rows, variation in the exposure and, for a binary outcome, at
#'   least ten rows in each class.
#'
#' Every row says how many rows had the exposure and the outcome and how many
#' the weight and sex rules left out, and a unit that cannot be fitted says
#' why, so nothing drops out silently.
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
    } else if (model == "linear") {
      # A numeric ordinal score, on each unit's own SD scale.
      standardise_within(
        prepared$outcomes[[outcome]],
        base$unit,
        settings$min_unit_rows
      )
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
  available <- nrow(data)
  counts <- list(
    n = available,
    n_available = available,
    n_no_weight = 0L,
    n_no_sex = 0L
  )
  result <- function(status, ...) {
    values <- c(
      list(estimate = NA_real_, se = NA_real_, df = NA_real_),
      counts,
      list(
        n_clusters = NA_integer_,
        few_clusters = NA,
        weighted = FALSE,
        se_type = NA_character_,
        covariates = NA_character_,
        effect_measure = NA_character_,
        engine = NA_character_,
        status = status
      )
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
    has_weight <- !is.na(data$weight) & data$weight > 0
    counts$n_no_weight <- sum(!has_weight)
    data <- data[has_weight, ]
  }

  # Covariates the unit can support.
  terms <- "x"
  covariates <- character()
  age_values <- dplyr::n_distinct(data$age, na.rm = TRUE)
  age_span <- if (age_values == 0) {
    0
  } else {
    diff(stats::quantile(data$age, c(0.05, 0.95), na.rm = TRUE, names = FALSE))
  }
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
  sex_known <- data$sex %in% c("Female", "Male")
  sex_counts <- table(data$sex[sex_known])
  if (
    length(sex_counts) == 2 &&
      min(sex_counts) >= 5 &&
      mean(sex_known) >= settings$sex_known_min
  ) {
    counts$n_no_sex <- sum(!sex_known)
    data <- data[sex_known, ]
    terms <- c(terms, "sex")
    covariates <- c(covariates, "sex")
  }

  n <- nrow(data)
  counts$n <- n
  common <- list(
    weighted = weighted,
    covariates = paste(covariates, collapse = " + ")
  )
  stop_with <- function(status) {
    do.call(result, c(list(status = status), common))
  }
  if (n < settings$min_unit_rows) {
    return(stop_with("fewer rows than the minimum unit size"))
  }
  if (stats::sd(data$x) == 0) {
    return(stop_with("no variation in the exposure"))
  }
  if (model == "linear" && stats::sd(data$y) == 0) {
    return(stop_with("no variation in the outcome"))
  }
  if (
    model == "logistic" && min(table(factor(data$y, levels = c(0, 1)))) < 10
  ) {
    return(stop_with("fewer than 10 rows in one class"))
  }
  if (model == "ordinal") {
    data$y <- factor(data$y, ordered = TRUE)
    if (nlevels(data$y) < 3) {
      return(stop_with("fewer than three levels observed"))
    }
    if (min(table(data$y)) < 5) {
      return(stop_with("fewer than 5 rows in one level"))
    }
  }

  # Clustering: the sampling unit, else the family, else none. A row with no
  # sampling unit recorded is a cluster of its own.
  cluster <- NULL
  se_type <- "heteroskedasticity-robust"
  if (dplyr::n_distinct(data$cluster, na.rm = TRUE) > 1) {
    cluster <- ifelse(
      is.na(data$cluster),
      paste0("\r", seq_len(n)),
      data$cluster
    )
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
  common <- c(
    common,
    list(
      n_clusters = if (is.null(cluster)) NA_integer_ else n_clusters,
      few_clusters = !is.null(cluster) && n_clusters < settings$min_clusters,
      se_type = se_type
    )
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
    return(stop_with(paste("model failed:", fitted)))
  }
  if (!fitted$converged) {
    return(stop_with("model did not converge"))
  }
  if (is.na(fitted$estimate) || is.na(fitted$se) || fitted$se <= 0) {
    return(stop_with("slope not estimable"))
  }
  do.call(
    result,
    c(
      list(
        status = "fitted",
        estimate = fitted$estimate,
        se = fitted$se,
        df = fitted$df,
        effect_measure = if (model == "linear") {
          "sd_per_hour"
        } else {
          "log_odds_per_hour"
        },
        engine = fitted$engine
      ),
      common
    )
  )
}

# Linear and logistic models, with weights and clustered or robust errors.
# A logistic fit that stops before converging returns a normal-looking
# table, so its convergence flag is read.
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
  converged <- model == "linear" || isTRUE(fit$convStatus)
  if (!"x" %in% rownames(table)) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      converged = converged,
      engine = "fixest"
    ))
  }
  list(
    estimate = unname(table["x", "Estimate"]),
    se = unname(table["x", "Std. Error"]),
    df = fixest::degrees_freedom(fit, type = "t"),
    converged = converged,
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
    df = survey::degf(design),
    converged = TRUE,
    engine = "survey"
  )
}
