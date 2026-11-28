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
#'   heteroskedasticity-robust. Strata are not used: against stratified
#'   fits of 112 of the same models in six stratified datasets the standard
#'   error was between about 4% smaller and 8% larger (median equal), so
#'   this is close but not uniformly conservative;
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
#' - a unit is fitted on the rows that have everything its model reads
#'   (after imputation, where that is asked for), and only with at least
#'   the minimum
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
#'   are left out (`n_no_other`), as are rows whose class is larger than
#'   the total it is part of (`n_other_inconsistent`), a unit that records none is not fitted,
#'   and each estimate says what the rest was built from (`other_from`). The
#'   slope without the adjustment on the same rows is in the
#'   `tier1_same_rows_` columns where the tier is 1.
#' @param imputation NULL to fit on the rows that have the exposure, or a
#'   list of `index` (the imputation number) and `seed` to first impute the
#'   exposure missing from a unit's rows for each outcome
#'   (`impute_cell_exposure()`); `settings` then needs the imputation
#'   settings. It may also hold `other_waves`, an environment in which each
#'   exposure's mean over a child's other waves is kept, so that fits of
#'   several imputations of one dataset work it out once. `n_exposure_imputed` counts the rows filled, and
#'   `exposure_imputation_failed` gives the reason where a draw failed and
#'   the rows were left as they were.
#' @param moderator NULL, or `"sex"` or `"ses"` for a model of moderation
#'   (DR-0021): the model then also holds the product of screen use with the
#'   unit's groups, `estimate` is the first group's slope less the second's
#'   (girls less boys; the lowest socioeconomic group less the highest),
#'   and the rows gain `moderator`, `moderator_measure` and the two groups'
#'   slopes, `slope_first` and `slope_second`. Such a model takes the first
#'   tier's covariates only.
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
  imputation = NULL,
  moderator = NULL
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  if (
    !is.null(moderator) && (!identical(as.integer(tiers), 1L) || adjust_other)
  ) {
    stop(
      "fit_stage1(): a model of moderation takes the first tier's ",
      "covariates and no other screen use.",
      call. = FALSE
    )
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
  # Each exposure's mean over a child's other waves, kept between cells
  # and, where the caller hands one store to every imputation, between
  # imputations: imputing covariates does not change an exposure.
  other_waves <- imputation$other_waves %||% new.env(parent = emptyenv())

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
    # Worked out once for an exposure, not once for each of its cells: it
    # does not depend on the outcome, and takes seconds in a large dataset.
    x_other_waves <- if (!is.null(imputation)) {
      if (is.null(other_waves[[exposure]])) {
        other_waves[[exposure]] <- stage1_other_waves(x, base$participant_id)
      }
      other_waves[[exposure]]
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
      # A child whose class is larger than the total it is part of gave an
      # inconsistent pair of answers: the row is left out of the adjusted
      # model, with or without imputation, and counted.
      n_inconsistent <- NA_integer_
      if (adjust_other) {
        inconsistent <- other$inconsistent[index] &
          any(!is.na(other$value[index]))
        n_inconsistent <- sum(inconsistent)
        index <- index[!inconsistent]
        if (length(index) == 0 || all(is.na(x[index]))) {
          next
        }
      }
      data <- frame[index, ]
      data$x <- x[index]
      data$y <- y[index]
      # The rule on rows where the exposure varies is judged on recorded
      # values, so an imputed value never makes a slope poolable and the
      # verdict is the same in every imputation.
      data$x_recorded <- !is.na(data$x)
      if (!is.null(imputation)) {
        data$x_other_waves <- x_other_waves[index]
      }
      if (adjust_other) {
        data$other <- other$value[index]
      }
      # A model of moderation is fitted to the rows whose group is known,
      # and its exposure is imputed among them, with the group in the
      # exposure's model.
      # The groups are settled here, once, so that they cannot change with
      # the rows an imputation leaves; a unit without groups keeps its
      # rows and the reason, and nothing is imputed for it.
      n_available <- nrow(data)
      no_groups <- FALSE
      if (!is.null(moderator)) {
        groups <- stage1_moderator(data, moderator, settings)
        data$moderator <- groups$score
        data$moderator_measure <- rep(groups$measure, nrow(data))
        data$moderator_status <- rep(groups$status, nrow(data))
        no_groups <- !is.na(groups$status)
        if (!no_groups) {
          data <- data[!is.na(data$moderator), ]
        }
      }
      n_no_group <- n_available - nrow(data)
      moderated <- !is.null(moderator)
      n_exposure_imputed <- NA_integer_
      exposure_imputation <- NA_character_
      if (no_groups) {
        data <- data[!is.na(data$x), ]
      } else if (
        !is.null(imputation) && !stage1_may_impute(data, model, settings)
      ) {
        n_exposure_imputed <- 0L
        data <- data[!is.na(data$x), ]
      } else if (!is.null(imputation)) {
        data <- impute_cell_exposure(
          data,
          # A model of moderation has draws of its own; the other models'
          # seeds are as they were.
          do.call(
            imputation_seed,
            c(
              list(
                imputation$seed,
                base$dataset_id[1],
                unit,
                exposure,
                outcome,
                adjust_other,
                imputation$index
              ),
              moderator
            )
          ),
          settings,
          moderated = moderated
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
          extra_terms = if (adjust_other) "other" else character(),
          moderator = moderator
        )
        if (moderated) {
          # Counted before the rows without a group were left out.
          fit$n_available <- n_available
          fit$n_no_group <- n_no_group
        }
        fit$n_no_other <- if (adjust_other) {
          sum(is.na(data$other))
        } else {
          NA_integer_
        }
        fit$n_other_inconsistent <- n_inconsistent
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

# Whether a unit's exposure may be imputed for a model. The model's own
# minimum sizes must be met by the rows that have the exposure, so that an
# imputed exposure never makes a model fittable; and the exposure is
# imputed only for the kinds of model the settings name.
stage1_may_impute <- function(data, model, settings) {
  if (!model %in% settings$impute_exposure_models) {
    return(FALSE)
  }
  # The rows a complete-case model would have: the exposure, and the rest
  # of screen use where the model adjusts for it, which is imputed too.
  seen <- data[!is.na(data$x), ]
  if ("other" %in% names(data)) {
    seen <- seen[!is.na(seen$other), ]
  }
  if (model == "logistic") {
    return(min(table(factor(seen$y, levels = c(0, 1)))) >= 10)
  }
  if (model == "ordinal") {
    counts <- table(seen$y)
    return(length(counts) >= 3 && min(counts) >= 5)
  }
  # A linear model needs the minimum unit size among the rows it would
  # keep of those that have the exposure.
  nrow(stage1_design(seen, settings, 1L)$data) >= settings$min_unit_rows
}

# The first of a model's minimums that a set of rows misses, or NA: the
# rows, variation in the exposure and the outcome, the classes of a binary
# outcome and the levels of an ordinal one, and the rows where the exposure
# varies (DR-0022). Used for each group a model of moderation compares;
# `fit_stage1_unit()` applies the same minimums to the unit as a whole.
stage1_minimums <- function(x, y, recorded, model, settings) {
  if (length(x) < settings$min_unit_rows) {
    return("fewer rows than the minimum unit size")
  }
  if (stats::sd(x) == 0) {
    return("no variation in the exposure")
  }
  varying <- recorded
  varying[recorded] <- exposure_varies(x[recorded])
  if (model == "linear") {
    if (stats::sd(y) == 0) {
      return("no variation in the outcome")
    }
  } else if (model == "logistic") {
    if (min(table(factor(y, levels = c(0, 1)))) < 10) {
      return("fewer than 10 rows in one class")
    }
  } else if (model == "ordinal") {
    # Every level the unit's model has must be in the group too.
    if (min(table(y)) < 5) {
      return("fewer than 5 rows in one level")
    }
  }
  if (sum(varying) < settings$support_min_rows) {
    return("too few rows where the exposure varies")
  }
  if (
    model == "logistic" &&
      min(table(factor(y[varying], levels = c(0, 1)))) <
        settings$support_min_class
  ) {
    return("too few of one class where the exposure varies")
  }
  NA_character_
}

# Which values differ from the commonest one. Ties go to the lowest value,
# so the count does not depend on the order of the rows. Values are
# compared as numbers: a value read back from a table's labels is rounded
# to 15 digits and equals none of its own rows.
exposure_varies <- function(x) {
  values <- sort(unique(x))
  commonest <- values[which.max(tabulate(match(x, values)))]
  x != commonest
}

# The groups a model of moderation compares in one unit (DR-0021).
#
# For sex, girls score 1 and boys 0, and a child recorded as another sex or
# none has no score. For socioeconomic position the measure is the one the
# second covariate tier uses in the unit, and its lowest, middle and
# highest categories score -1, 0 and 1; every measure the schema holds has
# three ordered categories. `status` says why the unit has no groups, or is
# NA.
stage1_moderator <- function(
  data,
  moderator,
  settings = default_analysis_settings()
) {
  none <- function(status, measure = NA_character_) {
    list(score = rep(NA_real_, nrow(data)), measure = measure, status = status)
  }
  if (moderator == "sex") {
    score <- unname(c(Female = 1, Male = 0)[data$sex])
    if (dplyr::n_distinct(score, na.rm = TRUE) < 2) {
      return(none("girls and boys are not both recorded in the unit", "sex"))
    }
    return(list(score = score, measure = "sex", status = NA_character_))
  }
  if (moderator != "ses") {
    stop("stage1_moderator(): unknown moderator ", moderator, call. = FALSE)
  }
  measure <- stage1_design(data, settings, 2L)$ses_measure
  if (is.na(measure)) {
    return(none("no socioeconomic measure that varies in the unit"))
  }
  score <- unname(bp_ses_scores()[data[[measure]]])
  if (!all(c(-1, 1) %in% score)) {
    return(none(
      "the lowest and highest socioeconomic groups are not both recorded",
      measure
    ))
  }
  list(score = score, measure = measure, status = NA_character_)
}

# The score of each category of the socioeconomic measures, lowest to
# highest. A category not listed, Unknown among them, has no score.
bp_ses_scores <- function() {
  c(
    "Low" = -1,
    "Medium" = 0,
    "High" = 1,
    "Secondary school or less" = -1,
    "Post-secondary" = 0,
    "Bachelor degree or higher" = 1
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
  # Which covariate values were imputed, where `apply_imputation()` says.
  for (marker in grep("\\.imputed$", names(base), value = TRUE)) {
    frame[[marker]] <- base[[marker]]
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
  extra_terms = character(),
  moderator = NULL
) {
  if (!tier %in% 1:3) {
    stop("fit_stage1_unit(): `tier` must be 1, 2 or 3.", call. = FALSE)
  }
  # The groups of a model of moderation, where the caller has not already
  # settled them (it does so before imputing an exposure).
  moderation <- NULL
  available <- nrow(data)
  if (!is.null(moderator)) {
    settled <- function(name) {
      if (name %in% names(data) && nrow(data) > 0) {
        data[[name]][1]
      } else {
        NA_character_
      }
    }
    if ("moderator" %in% names(data)) {
      moderation <- list(
        measure = settled("moderator_measure"),
        status = settled("moderator_status")
      )
    } else {
      moderation <- stage1_moderator(data, moderator, settings)
      data$moderator <- moderation$score
    }
    if (is.na(moderation$status)) {
      data <- data[!is.na(data$moderator), ]
    }
  }
  n_no_group <- available - nrow(data)
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
        covariate_imputed_share = NA_real_,
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
    if (!is.null(moderator)) {
      values <- c(
        values,
        list(
          moderator = moderator,
          moderator_measure = moderation$measure %||% NA_character_,
          n_no_group = n_no_group,
          slope_first = NA_real_,
          slope_second = NA_real_
        )
      )
    }
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (!is.null(moderator) && !is.na(moderation$status)) {
    return(result(moderation$status))
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
  # The compared groups, and the terms the moderation adds (DR-0021): for
  # sex, girls against boys, with sex already among the terms; for
  # socioeconomic position, the three groups as categories and the product
  # of screen use with their score of -1, 0 and 1. The groups enter as
  # categories because the outcome need not step evenly across them: with
  # the score alone, an uneven step is taken for a difference in slopes
  # wherever screen use differs between the groups.
  compared <- NULL
  if (!is.null(moderator)) {
    compared <- if (moderator == "sex") c(1, 0) else c(-1, 1)
    terms <- c(
      terms,
      if (moderator == "sex") {
        "x:moderator"
      } else {
        c("factor(moderator)", "x:moderator")
      }
    )
    covariates <- c(covariates, paste("screen use by", moderator))
  }
  weighted <- design$weighted
  ses_measure <- design$ses_measure
  missing_tier <- design$not_fitted
  counts[names(design$counts)] <- design$counts

  n <- nrow(data)
  counts$n <- n
  # The share of the model's rows with any of the tier's covariates
  # imputed.
  markers <- intersect(paste0(design$added, ".imputed"), names(data))
  imputed_share <- if (length(markers) > 0 && nrow(data) > 0) {
    mean(rowSums(as.matrix(data[markers])) > 0)
  } else if (length(design$added) > 0 && nrow(data) > 0) {
    0
  } else {
    NA_real_
  }
  common <- list(
    covariate_imputed_share = imputed_share,
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
  if (!is.null(moderator) && moderator == "sex" && !"sex" %in% design$terms) {
    return(stop_with("sex is not recorded well enough in the unit"))
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
  # A slope is learned from the rows where the exposure differs: enough of
  # the model's rows must have a recorded value away from the commonest
  # one, and for a binary outcome enough of each class among them
  # (DR-0022).
  recorded <- if ("x_recorded" %in% names(data)) {
    data$x_recorded
  } else {
    rep(TRUE, n)
  }
  varying <- recorded
  varying[recorded] <- exposure_varies(data$x[recorded])
  if (sum(varying) < settings$support_min_rows) {
    return(stop_with("too few rows where the exposure varies"))
  }
  # A difference between two slopes is estimated only where each of the
  # two would be fitted on its own (DR-0021): each compared group meets
  # the same minimums, on its own rows.
  if (!is.null(moderator)) {
    for (group in compared) {
      rows <- data$moderator == group
      short <- stage1_minimums(
        data$x[rows],
        data$y[rows],
        recorded[rows],
        model,
        settings
      )
      if (!is.na(short)) {
        return(stop_with(paste("in a compared group:", short)))
      }
    }
  }
  if (
    model == "logistic" &&
      min(table(factor(data$y[varying], levels = c(0, 1)))) <
        settings$support_min_class
  ) {
    return(stop_with("too few of one class where the exposure varies"))
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

  # The coefficient reported: the slope on screen use, or in a model of
  # moderation its product with the group.
  term <- if (is.null(moderator)) "x" else "x:moderator"
  run <- function(formula) {
    tryCatch(
      switch(
        model,
        linear = stage1_fixest(data, formula, cluster, "linear", term),
        logistic = stage1_fixest(data, formula, cluster, "logistic", term),
        ordinal = stage1_ordinal(data, formula, term)
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
  # The estimand of a model of moderation is the first group's slope less
  # the second's (DR-0021): girls less boys, which is the product's
  # coefficient; the lowest socioeconomic group less the highest, which is
  # minus twice it. The two slopes are kept beside it.
  if (!is.null(moderator)) {
    product <- fitted$estimate
    scale <- compared[1] - compared[2]
    fitted$estimate <- scale * product
    fitted$se <- abs(scale) * fitted$se
    common$slope_first <- fitted$slope + compared[1] * product
    common$slope_second <- fitted$slope + compared[2] * product
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
stage1_fixest <- function(data, formula, cluster, model, term = "x") {
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
  if (!all(c("x", term) %in% rownames(table))) {
    return(list(
      estimate = NA_real_,
      se = NA_real_,
      df = NA_real_,
      slope = NA_real_,
      converged = converged,
      engine = "fixest",
      dropped = dropped
    ))
  }
  list(
    estimate = unname(table[term, "Estimate"]),
    se = unname(table[term, "Std. Error"]),
    df = fixest::degrees_freedom(fit, type = "t"),
    slope = unname(table["x", "Estimate"]),
    converged = converged,
    engine = "fixest",
    dropped = dropped
  )
}

# A proportional-odds model through the survey package, which gives design
# based errors with weights and clusters. The outcome must be an ordered
# factor: `svyolr()` accepts an unordered one without complaint.
stage1_ordinal <- function(data, formula, term = "x") {
  stopifnot(is.ordered(data$y))
  design <- survey::svydesign(
    ids = ~cluster_id,
    weights = ~w,
    data = data
  )
  fit <- survey::svyolr(formula, design = design)
  table <- stats::coef(summary(fit))
  list(
    estimate = unname(table[term, "Value"]),
    se = unname(table[term, "Std. Error"]),
    df = survey::degf(design),
    slope = unname(table["x", "Value"]),
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
    return(list(
      value = rep(NA_real_, n),
      from = rep(NA_character_, n),
      inconsistent = rep(FALSE, n)
    ))
  }
  # Where the rest comes from a dedicated total, a class above that total
  # has no rest (`derive_total_screen()`).
  own <- prepared$exposures[[exposure]]
  total <- prepared$exposures$total_screen
  list(
    value = prepared$exposures[[column]],
    inconsistent = startsWith(
      dplyr::coalesce(prepared$exposures$total_source, ""),
      "dedicated"
    ) &
      is.na(prepared$exposures[[column]]) &
      !is.na(own) &
      !is.na(total) &
      own > total + 1e-8,
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
