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
#' - a further covariate tier adds to age and sex (DR-0015): tier 2 a
#'   socioeconomic measure, the first the unit records of household SES,
#'   parental education and area SES; tier 3 also ethnicity, or else
#'   minority status, and urbanicity. A unit that does not record a tier's
#'   covariates is not fitted at that tier, and says so. A covariate counts
#'   as recorded where it is known for at least `covariate_known_min` of the
#'   unit's rows and two of its groups have five rows or more, and the rows
#'   with all of the tier's covariates must be that share too; rows where
#'   one is unknown are left out, and groups of fewer than five rows are
#'   merged. A further tier also gives the tier 1 slope on its own rows
#'   (`tier1_same_rows_estimate`);
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
#' @param tiers The covariate tiers to fit each cell at, among 1, 2 and 3.
#' @param adjust_other Also adjust for the rest of the child's screen use
#'   (`other_<exposure>` from `derive_total_screen()`): the secondary family
#'   in which one content class is adjusted for the others. Rows without it
#'   are left out (`n_no_other`), a unit that records none is not fitted,
#'   and each estimate says what the rest was built from (`other_from`). The
#'   slope without the adjustment on the same rows is in the
#'   `tier1_same_rows_` columns where the tier is 1.
#' @param imputation NULL to fit on the rows that have the exposure, or a
#'   list of `index` (the imputation number) and `seed` to first impute the
#'   exposure missing from a unit's rows for each outcome
#'   (`impute_cell_exposure()`); `settings` then needs the imputation
#'   settings. `n_exposure_imputed` counts the rows filled, and
#'   `exposure_imputation_failed` gives the reason where a draw failed and
#'   the rows were left as they were.
#' @return A tibble with one row per unit, cell and tier that has data: the slope
#'   (`estimate`, `se`), its `effect_measure`, the rows, clusters and degrees
#'   of freedom behind it, the covariates and engine used, and `status`
#'   (`fitted` or the reason it was not). NULL when the dataset is not
#'   included or holds none of the cells.
fit_stage1 <- function(
  prepared,
  cells,
  outcome_registry,
  dataschema,
  settings = default_analysis_settings(),
  tiers = 1L,
  adjust_other = FALSE,
  imputation = NULL
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
  levels_of <- function(variable) schema_level_count(variable, dataschema)
  frame <- stage1_frame(base)
  unit_rows <- split(seq_len(nrow(frame)), frame$unit)

  results <- list()
  for (i in seq_len(nrow(cells))) {
    exposure <- cells$exposure[i]
    outcome <- cells$outcome[i]
    score_type <- registry$score_type[match(outcome, registry$variable_name)]
    model <- stage1_model_type(score_type, levels_of(outcome))
    x <- prepared$exposures[[exposure]]
    other <- if (adjust_other) stage1_other_screen(prepared, exposure)
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
    x_other_waves <- if (!is.null(imputation)) {
      stage1_other_waves(x, base$participant_id)
    }
    for (unit in names(unit_rows)) {
      index <- unit_rows[[unit]]
      index <- index[!is.na(y[index])]
      if (is.null(imputation)) {
        index <- index[!is.na(x[index])]
      }
      if (length(index) == 0 || all(is.na(x[index]))) {
        next
      }
      data <- frame[index, ]
      data$x <- x[index]
      data$y <- y[index]
      if (adjust_other) {
        data$other <- other$value[index]
      }
      n_exposure_imputed <- NA_integer_
      exposure_imputation <- NA_character_
      if (!is.null(imputation)) {
        data$x_other_waves <- x_other_waves[index]
        data <- impute_cell_exposure(
          data,
          imputation_seed(
            imputation$seed,
            base$dataset_id[1],
            unit,
            exposure,
            outcome,
            adjust_other,
            imputation$index
          ),
          settings
        )
        n_exposure_imputed <- attr(data, "imputed")
        exposure_imputation <- attr(data, "failed") %||% NA_character_
        data <- data[!is.na(data$x), ]
      }
      for (tier in tiers) {
        fit <- fit_stage1_unit(
          data,
          model,
          settings,
          tier,
          extra_terms = if (adjust_other) "other" else character()
        )
        fit$n_no_other <- if (adjust_other) {
          sum(is.na(data$other))
        } else {
          NA_integer_
        }
        fit$n_exposure_imputed <- n_exposure_imputed
        fit$exposure_imputation_failed <- exposure_imputation
        fit$other_from <- if (adjust_other) {
          other$from[index[1]]
        } else {
          NA_character_
        }
        fit$unit <- unit
        fit$exposure <- exposure
        fit$outcome <- outcome
        fit$score_type <- score_type
        results[[length(results) + 1]] <- fit
      }
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

# A child's mean on a variable over their other rows (their other waves),
# NA where they have none with a value.
stage1_other_waves <- function(values, participant) {
  seen <- !is.na(values)
  total <- stats::ave(ifelse(seen, values, 0), participant, FUN = sum)
  count <- stats::ave(as.numeric(seen), participant, FUN = sum)
  others <- count - seen
  ifelse(others > 0, (total - ifelse(seen, values, 0)) / others, NA_real_)
}

# The columns of the prepared base table a unit's model reads.
stage1_frame <- function(base) {
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
  for (covariate in bp_stage1_covariates()) {
    frame[[covariate]] <- as.character(column(covariate))
  }
  frame
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
#
# `extra_terms` names further columns of `data` to adjust for (the rest of
# the child's screen use, or the outcome at an earlier wave). Rows where one
# is unknown are left out before anything else is judged, and a unit where
# one is not recorded or does not vary is not fitted.
fit_stage1_unit <- function(
  data,
  model,
  settings = default_analysis_settings(),
  tier = 1L,
  extra_terms = character()
) {
  if (!tier %in% 1:3) {
    stop("fit_stage1_unit(): `tier` must be 1, 2 or 3.", call. = FALSE)
  }
  available <- nrow(data)
  if (length(extra_terms) > 0) {
    data <- data[stats::complete.cases(data[extra_terms]), ]
  }
  counts <- list(
    n = available,
    n_available = available,
    n_no_weight = 0L,
    n_no_sex = 0L,
    n_no_covariate = 0L
  )
  result <- function(status, ...) {
    values <- c(
      list(estimate = NA_real_, se = NA_real_, df = NA_real_),
      counts,
      list(
        n_clusters = NA_integer_,
        few_clusters = NA,
        weighted = FALSE,
        tier = as.integer(tier),
        ses_measure = NA_character_,
        ethnicity_measure = NA_character_,
        tier1_same_rows_estimate = NA_real_,
        tier1_same_rows_se = NA_real_,
        terms_dropped = NA_character_,
        covariate_separation = NA,
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
  if (nrow(data) == 0) {
    return(result(
      if (length(extra_terms) == 0) {
        "no rows"
      } else {
        paste("no row has", paste(extra_terms, collapse = " and "))
      }
    ))
  }

  design <- stage1_design(data, settings, tier)
  data <- design$data
  terms <- c(design$terms, extra_terms)
  covariates <- c(design$covariates, extra_terms)
  weighted <- design$weighted
  ses_measure <- design$ses_measure
  missing_tier <- design$not_fitted
  counts[names(design$counts)] <- design$counts

  n <- nrow(data)
  counts$n <- n
  common <- list(
    weighted = weighted,
    ses_measure = ses_measure,
    ethnicity_measure = design$ethnicity_measure,
    covariates = paste(covariates, collapse = " + ")
  )
  stop_with <- function(status) {
    do.call(result, c(list(status = status), common))
  }
  if (!is.na(missing_tier)) {
    return(stop_with(missing_tier))
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
  for (term in extra_terms) {
    if (stats::sd(data[[term]]) == 0) {
      return(stop_with(paste("no variation in", term)))
    }
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

  clustering <- stage1_cluster(data)
  cluster <- clustering$cluster
  se_type <- clustering$se_type
  data$cluster_id <- clustering$cluster_id
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

  run <- function(formula) {
    tryCatch(
      switch(
        model,
        linear = stage1_fixest(data, formula, cluster, "linear"),
        logistic = stage1_fixest(data, formula, cluster, "logistic"),
        ordinal = stage1_ordinal(data, formula)
      ),
      error = function(e) conditionMessage(e)
    )
  }
  fitted <- run(formula)
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
        engine = fitted$engine,
        terms_dropped = fitted$dropped
      ),
      stage1_tier_extras(
        data,
        model,
        c(design$added, extra_terms),
        design$added,
        terms,
        run
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
  # A term that repeats another is left out of the fit, which is recorded.
  dropped <- if (length(fit$collin.var) > 0) {
    paste(fit$collin.var, collapse = ", ")
  } else {
    NA_character_
  }
  if (!"x" %in% rownames(table)) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      converged = converged,
      engine = "fixest",
      dropped = dropped
    ))
  }
  list(
    estimate = unname(table["x", "Estimate"]),
    se = unname(table["x", "Std. Error"]),
    df = fixest::degrees_freedom(fit, type = "t"),
    converged = converged,
    engine = "fixest",
    dropped = dropped
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
    engine = "survey",
    dropped = NA_character_
  )
}

# The rest of a child's screen use beside one exposure, and the rows it was
# built from: `other_<exposure>` where the preparation step derived it. The
# derived total has no rest, and nor has a row outside the content classes.
stage1_other_screen <- function(prepared, exposure) {
  n <- nrow(prepared$base)
  column <- paste0("other_", sub("^st_", "", exposure))
  if (!column %in% names(prepared$exposures)) {
    return(list(value = rep(NA_real_, n), from = rep(NA_character_, n)))
  }
  list(
    value = prepared$exposures[[column]],
    from = ifelse(
      is.na(prepared$exposures$total_source),
      NA_character_,
      paste0(
        prepared$exposures$total_source,
        ": ",
        prepared$exposures$total_rows
      )
    )
  )
}

# The covariates of the further tiers, as columns of the prepared base table.
bp_stage1_covariates <- function() {
  c(
    "ses_household",
    "parental_education",
    "ses_area",
    "ethnicity",
    "minority",
    "urbanicity"
  )
}

# The rows and terms of one unit's model at a covariate tier: which rows the
# weight, covariate, age and sex rules leave out, and the terms beside screen
# use. `stage1_correlations()` uses the same function, so the two cannot
# drift apart. The tier's covariates are settled first, so that age and sex
# are judged on the rows that are left.
#
# `not_fitted` says why the unit cannot be fitted at the tier, or is NA.
stage1_design <- function(
  data,
  settings = default_analysis_settings(),
  tier = 1L
) {
  counts <- list(n_no_weight = 0L, n_no_sex = 0L, n_no_covariate = 0L)
  not_fitted <- NA_character_

  # Weights: used where the unit has them, on the rows that have one.
  weighted <- any(!is.na(data$weight) & data$weight > 0)
  if (weighted) {
    has_weight <- !is.na(data$weight) & data$weight > 0
    counts$n_no_weight <- sum(!has_weight)
    data <- data[has_weight, ]
  }

  # Further tiers: each needs its covariates recorded in the unit.
  wanted <- list()
  if (tier >= 2) {
    wanted[["socioeconomic measure"]] <- c(
      "ses_household",
      "parental_education",
      "ses_area"
    )
  }
  if (tier >= 3) {
    wanted[["ethnicity or minority measure"]] <- c("ethnicity", "minority")
    wanted[["urbanicity measure"]] <- "urbanicity"
  }
  added <- character()
  for (what in names(wanted)) {
    # The first of the candidates the unit records well enough.
    found <- NA_character_
    for (candidate in wanted[[what]]) {
      groups <- stage1_covariate_groups(data[[candidate]], settings)
      if (!is.null(groups)) {
        data[[candidate]] <- groups
        found <- candidate
        break
      }
    }
    if (is.na(found) && is.na(not_fitted)) {
      not_fitted <- paste("no", what, "that varies in the unit")
    }
    added <- c(added, found)
  }
  ses_measure <- if (tier >= 2) added[1] else NA_character_
  ethnicity_measure <- if (tier >= 3) added[2] else NA_character_
  added <- added[!is.na(added)]
  if (length(added) > 0 && is.na(not_fitted)) {
    known <- stats::complete.cases(data[added])
    counts$n_no_covariate <- sum(!known)
    # Each covariate is known for the minimum share of rows; together they
    # must be too.
    if (mean(known) < settings$covariate_known_min) {
      not_fitted <- "too few rows have all of the tier's covariates"
    }
    data <- data[known, ]
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
  if (is.na(not_fitted)) {
    # The age and sex rules may have removed a covariate's second group.
    varies <- vapply(
      added,
      function(name) dplyr::n_distinct(data[[name]]) > 1,
      logical(1)
    )
    if (!all(varies)) {
      not_fitted <- "a tier covariate does not vary in the rows left"
    }
    terms <- c(terms, added)
    covariates <- c(covariates, added)
  }
  list(
    data = data,
    terms = terms,
    covariates = covariates,
    added = if (is.na(not_fitted)) added else character(),
    weighted = weighted,
    ses_measure = ses_measure,
    ethnicity_measure = ethnicity_measure,
    counts = counts,
    not_fitted = not_fitted
  )
}

# What a further tier records beside its slope: the slope of the tier 1
# model on the same rows, so the change from adjusting is not mixed with the
# change from losing rows, and whether a group of a covariate holds only one
# class of a binary outcome, which a logistic model cannot estimate.
#
# `added` is every term beyond age and sex, and `grouped` those of them
# that are groups, which are the ones that can separate a binary outcome.
stage1_tier_extras <- function(data, model, added, grouped, terms, run) {
  if (length(added) == 0) {
    return(list())
  }
  first <- run(stats::as.formula(paste(
    "y ~",
    paste(setdiff(terms, added), collapse = " + ")
  )))
  refit <- !is.character(first) && isTRUE(first$converged)
  separation <- if (model == "logistic") {
    any(vapply(
      grouped,
      function(name) any(tapply(data$y, data[[name]], stats::sd) %in% 0),
      logical(1)
    ))
  } else {
    NA
  }
  list(
    tier1_same_rows_estimate = if (refit) first$estimate else NA_real_,
    tier1_same_rows_se = if (refit) first$se else NA_real_,
    covariate_separation = separation
  )
}

# A categorical covariate ready for a unit's model, or NULL where the unit
# does not record it well enough: known for the minimum share of rows, with
# two groups of five rows or more. Smaller groups are merged into one, which
# then stands for unlike groups and only adjusts; that one is left out (NA)
# if it is still under five rows.
stage1_covariate_groups <- function(
  values,
  settings = default_analysis_settings()
) {
  if (is.null(values) || mean(!is.na(values)) < settings$covariate_known_min) {
    return(NULL)
  }
  sizes <- table(values)
  small <- names(sizes)[sizes < 5]
  if (length(small) > 0) {
    merged <- if (sum(sizes[small]) >= 5) "(small groups)" else NA_character_
    values[values %in% small] <- merged
  }
  if (dplyr::n_distinct(values, na.rm = TRUE) < 2) {
    return(NULL)
  }
  values
}

# The number of levels the dataschema allows a categorical row, without
# `Unknown`.
schema_level_count <- function(variable, dataschema) {
  allowed <- dataschema$allowed_values[match(
    variable,
    dataschema$variable_name
  )]
  length(setdiff(strsplit(allowed, "|", fixed = TRUE)[[1]], "Unknown"))
}

# The clusters a unit's standard errors use: the sampling unit, else the
# family, else none. A row with no sampling unit recorded is a cluster of
# its own. `cluster` is NULL where errors are not clustered.
stage1_cluster <- function(data) {
  n <- nrow(data)
  cluster <- NULL
  se_type <- "heteroskedasticity-robust"
  if (dplyr::n_distinct(data$cluster, na.rm = TRUE) > 1) {
    cluster <- ifelse(
      is.na(data$cluster),
      paste0("\r", seq_len(n)),
      data$cluster
    )
    se_type <- "clustered on sampling unit"
  } else if (!anyNA(data$family) && dplyr::n_distinct(data$family) < n) {
    cluster <- data$family
    se_type <- "clustered on family"
  }
  list(
    cluster = cluster,
    se_type = se_type,
    cluster_id = if (is.null(cluster)) as.character(seq_len(n)) else cluster
  )
}
