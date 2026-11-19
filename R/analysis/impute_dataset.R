#' One imputation of a dataset's sporadically missing covariates
#'
#' Multiple imputation by chained equations (ANALYSIS_PLAN.md, Step 8;
#' DR-0019), one stage 1 unit (dataset by wave by country) at a time, so
#' that every relation it preserves is the unit's own. Each call makes one
#' imputation; the pipeline calls it once per imputation number.
#'
#' Sex and the covariates of the further tiers are imputed, by predictive
#' mean matching, so an imputed value is one the unit observed. Screen use
#' is not imputed here: it is imputed for each outcome's own model
#' (`impute_cell_exposure()`), because its imputation must contain the
#' relation that model estimates.
#'
#' A covariate is imputed only where its missingness is sporadic:
#'
#' - it is known for at least `impute_min_observed` of the unit's rows, on
#'   at least the minimum number of rows, and takes two values or more;
#' - and, row by row, only for children of an age (in whole years) at which
#'   the unit observed it for at least `impute_min_observed_age` of the
#'   rows, so a question never asked of one age group is not imputed for
#'   that group.
#'
#' Elsewhere it stays missing, and the covariate tiers deal with it.
#' Minority status is not imputed in a unit that records ethnicity, which
#' the tiers use first.
#'
#' What the imputation model reads beside the covariates themselves:
#'
#' - screen use: the included usual-day rows and the dedicated total;
#' - the outcomes, as one score per outcome family: the mean of a row's
#'   scores in that family, each standardised (binary and ordinal rows
#'   within the unit) and turned so that higher is better;
#' - age, and the survey weight where the unit has one;
#' - in a panel, the child's mean on each exposure and outcome score over
#'   their other waves, with the unit's mean and a marker where they have
#'   no other wave.
#'
#' Screen use and the scores have gaps of their own, which are filled
#' within the model so that it can run and are never kept. A predictor
#' enters a covariate's model where the two correlate
#' (`mice::quickpred()`), always with age, sex, the weight, screen use and
#' the outcome scores, where the unit has enough of each. A unit that
#' knows too little of an exposure imputes its covariates without it.
#'
#' Sex is imputed as female or male: a child recorded as another sex keeps
#' that record, and is not a donor. Sex, ethnicity and minority status do
#' not change within a child, so a child's imputed value is the one
#' observed at another of their waves, or else the one imputed at their
#' earliest wave.
#'
#' The seed of a unit's imputation comes from the project seed, the unit
#' and the imputation number, so an imputation is the same whichever
#' others are made. In a panel a unit's imputation reads the children's
#' other waves, so it does depend on the dataset's other units.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param index The imputation number.
#' @param settings From `analysis_settings()`, with `seed`.
#' @return NULL where the dataset is not included; otherwise a list of
#'   `made`, the imputation number (so an imputation with nothing to fill
#'   is still known to have been made), and two tibbles, each with
#'   `imputation`: `cells`, the imputed cells as `row`
#'   (the row of the prepared tables), `variable` and `value`; and `units`,
#'   one row per unit and covariate with missing values, saying how many
#'   were imputed, how many were left because the child's age group
#'   observed too little, or why none were.
impute_dataset <- function(
  prepared,
  outcome_registry,
  exposure_registry,
  index,
  settings = default_analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  if (is.null(settings$seed)) {
    stop("impute_dataset(): `settings` needs a `seed`.", call. = FALSE)
  }
  base <- prepared$base
  frame <- imputation_frame(prepared, outcome_registry, exposure_registry)
  data <- frame$data
  unit_rows <- split(seq_len(nrow(data)), base$unit)

  cells <- list(tibble::tibble(
    row = integer(),
    variable = character(),
    value = character()
  ))
  notes <- list()
  for (unit in names(unit_rows)) {
    rows <- unit_rows[[unit]]
    seed <- imputation_seed(settings$seed, base$dataset_id[1], unit, index)
    done <- impute_unit(data[rows, , drop = FALSE], frame, seed, settings)
    done$units$unit <- rep(unit, nrow(done$units))
    notes[[unit]] <- done$units
    done$cells$row <- rows[done$cells$row]
    cells[[unit]] <- done$cells
  }
  cells <- dplyr::bind_rows(cells)
  cells <- imputation_hold_constant(cells, data, base, frame$constant)

  labelled <- function(table) {
    table$imputation <- rep(as.integer(index), nrow(table))
    dplyr::relocate(table, "imputation")
  }
  list(
    cells = labelled(cells),
    units = labelled(dplyr::relocate(dplyr::bind_rows(notes), "unit")),
    made = as.integer(index)
  )
}

# The table an imputation model reads, one row per row of the prepared
# tables, and which of its columns play which part.
imputation_frame <- function(prepared, outcome_registry, exposure_registry) {
  base <- prepared$base
  usual <- exposure_registry[
    exposure_registry$include %in%
      TRUE &
      exposure_registry$day %in% "usual" &
      exposure_registry$unit %in% "hours" &
      exposure_registry$context %in% "any",
  ]
  # The dedicated total's own row is left out: the derived total holds it.
  exposures <- intersect(
    c(
      bp_total_exposure(),
      usual$variable_name[!usual$exposure %in% "total"]
    ),
    names(prepared$exposures)
  )
  covariates <- intersect(c("sex", bp_stage1_covariates()), names(base))
  data <- as.data.frame(prepared$exposures[exposures])
  # A summed total repeats its parts, so only a dedicated one is read.
  summed <- total_is_summed(prepared$exposures$total_source)
  data[[bp_total_exposure()]][summed] <- NA_real_
  for (covariate in covariates) {
    data[[covariate]] <- as.character(base[[covariate]])
  }
  data$aux_age <- as.numeric(base$age_years)
  weight <- as.numeric(base$survey_weight_norm)
  data$aux_weight <- ifelse(!is.na(weight) & weight > 0, log(weight), NA_real_)

  # One score per outcome family: the mean of the row's standardised
  # scores, each turned so that higher is better.
  rows <- outcome_registry$rows
  directed <- rows$variable_name[
    rows$role %in%
      "outcome" &
      rows$direction %in% c("higher_better", "higher_worse")
  ]
  scored <- intersect(directed, names(prepared$outcomes))
  at <- match(scored, rows$variable_name)
  families <- rows$prereg_family[at]
  turn <- ifelse(rows$direction[at] %in% "higher_worse", -1, 1)
  # A row's standardised score: the preparation step's where it made one,
  # and otherwise (binary and ordinal rows) its value standardised within
  # the unit.
  score_of <- function(name) {
    if (name %in% names(prepared$outcomes_z)) {
      return(prepared$outcomes_z[[name]])
    }
    values <- prepared$outcomes[[name]]
    if (!is.numeric(values) && !is.logical(values)) {
      return(NULL)
    }
    standardise_within(as.numeric(values), base$unit, 2L)
  }
  outcomes <- character()
  # The mean is built up one outcome at a time, so that a dataset with many
  # outcome rows is never copied whole.
  for (family in unique(stats::na.omit(families))) {
    total <- numeric(nrow(base))
    count <- numeric(nrow(base))
    for (i in which(families %in% family)) {
      values <- score_of(scored[i])
      if (is.null(values)) {
        next
      }
      seen <- !is.na(values)
      total[seen] <- total[seen] + turn[i] * values[seen]
      count <- count + seen
    }
    if (!any(count > 0)) {
      next
    }
    name <- paste0("aux_", family)
    data[[name]] <- ifelse(count > 0, total / count, NA_real_)
    outcomes <- c(outcomes, name)
  }

  # In a panel, the child's mean over their other waves.
  if (anyDuplicated(base$participant_id) > 0) {
    for (name in c(exposures, outcomes)) {
      others <- stage1_other_waves(data[[name]], base$participant_id)
      has <- !is.na(others)
      if (!any(has)) {
        next
      }
      unit_mean <- stats::ave(
        others,
        base$unit,
        FUN = function(x) mean(x, na.rm = TRUE)
      )
      filled <- ifelse(has, others, unit_mean)
      filled[!is.finite(filled)] <- 0
      data[[paste0("aux_other_", name)]] <- filled
      data[[paste0("aux_none_", name)]] <- as.numeric(!has)
    }
  }
  list(
    data = data,
    exposures = exposures,
    covariates = covariates,
    outcomes = outcomes,
    always = c("aux_age", "aux_weight", "sex", outcomes, exposures),
    constant = intersect(covariates, c("sex", "ethnicity", "minority"))
  )
}

# Impute one unit once. Returns the imputed cells of the covariates (`row`
# within the unit) and a note per covariate with missing values.
impute_unit <- function(data, frame, seed, settings) {
  kept <- frame$covariates
  n <- nrow(data)
  was_missing <- is.na(data)
  known_before <- colSums(!was_missing)
  # Sex is imputed as female or male, so another record is not a donor.
  if ("sex" %in% names(data)) {
    data$sex[!data$sex %in% c("Female", "Male")] <- NA_character_
  }
  known <- vapply(data, function(x) sum(!is.na(x)), numeric(1))
  values <- vapply(data, function(x) dplyr::n_distinct(x, na.rm = TRUE), 1)
  sporadic <- known / n >= settings$impute_min_observed &
    known >= settings$min_unit_rows &
    values >= 2
  targets <- names(data)[known < n & sporadic]
  # Minority status follows from ethnicity, and the covariate tiers take
  # ethnicity where a unit has it, so minority status is then left alone:
  # a model cannot hold both.
  has_ethnicity <- "ethnicity" %in% names(data)[sporadic]
  usable <- setdiff(names(data), if (has_ethnicity) "minority")
  targets <- intersect(targets, usable)

  reported <- intersect(kept, names(data)[known_before < n & known_before > 0])
  unneeded <- setdiff(reported, usable)
  note <- function(variables, status, imputed = 0L, left = 0L) {
    tibble::tibble(
      variable = variables,
      rows = rep(n, length(variables)),
      n_missing = as.integer(n - known_before[variables]),
      n_imputed = rep_len(as.integer(imputed), length(variables)),
      n_age_group = rep_len(as.integer(left), length(variables)),
      status = rep_len(status, length(variables))
    )
  }
  none <- tibble::tibble(
    row = integer(),
    variable = character(),
    value = character()
  )
  if (n < settings$min_unit_rows) {
    return(list(
      cells = none,
      units = note(reported, "fewer rows than the minimum unit size")
    ))
  }
  wanted <- intersect(targets, reported)
  left <- setdiff(reported, c(wanted, unneeded))
  one_value <- left[values[left] < 2]
  not_imputed <- dplyr::bind_rows(
    note(
      setdiff(left, one_value),
      "too little observed in the unit to impute"
    ),
    note(one_value, "takes one value in the unit"),
    note(unneeded, "not needed: the unit records ethnicity")
  )
  if (length(wanted) == 0) {
    return(list(cells = none, units = not_imputed))
  }

  # The model's columns: what has gaps to fill, and what is complete and
  # varies.
  complete <- intersect(names(data)[known == n & values >= 2], usable)
  model <- data[c(targets, setdiff(complete, targets))]
  for (name in names(model)) {
    if (is.character(model[[name]])) {
      model[[name]] <- factor(model[[name]])
    }
  }
  method <- stats::setNames(rep("", ncol(model)), names(model))
  method[targets] <- "pmm"
  predictors <- mice::quickpred(
    model,
    mincor = settings$impute_min_correlation,
    include = intersect(frame$always, names(model))
  )
  # `mice` leaves out a predictor that is constant or repeats another and
  # warns that it logged the event; that is expected here and not passed on.
  fit <- tryCatch(
    withCallingHandlers(
      mice::mice(
        model,
        m = 1,
        maxit = settings$imputation_iterations,
        method = method,
        predictorMatrix = predictors,
        seed = seed,
        printFlag = FALSE
      ),
      warning = function(w) {
        if (grepl("logged events", conditionMessage(w), fixed = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) conditionMessage(e)
  )
  if (is.character(fit)) {
    return(list(
      cells = none,
      units = dplyr::bind_rows(
        note(wanted, paste("imputation failed:", fit)),
        not_imputed
      )
    ))
  }
  # Kept: cells that were missing to begin with, of children at an age at
  # which the unit observed enough of the covariate.
  age_year <- floor(data$aux_age)
  cells <- lapply(wanted, function(name) {
    drawn <- fit$imp[[name]]
    row <- match(rownames(drawn), rownames(model))
    seen <- !was_missing[, name]
    by_age <- stats::ave(as.numeric(seen), age_year, FUN = mean)
    asked <- !is.na(age_year) & by_age >= settings$impute_min_observed_age
    keep <- was_missing[row, name] & asked[row] & !is.na(drawn[[1]])
    tibble::tibble(
      row = row[keep],
      variable = rep(name, sum(keep)),
      value = as.character(drawn[[1]][keep]),
      left = rep(sum(was_missing[, name] & !asked), sum(keep))
    )
  })
  left <- vapply(
    wanted,
    function(name) {
      seen <- !was_missing[, name]
      by_age <- stats::ave(as.numeric(seen), age_year, FUN = mean)
      asked <- !is.na(age_year) & by_age >= settings$impute_min_observed_age
      sum(was_missing[, name] & !asked)
    },
    numeric(1)
  )
  cells <- dplyr::bind_rows(c(list(none), cells))
  cells$left <- NULL
  filled <- as.integer(table(factor(cells$variable, levels = wanted)))
  status <- ifelse(
    filled + left == n - known_before[wanted],
    "imputed",
    "some values could not be imputed"
  )
  list(
    cells = cells,
    units = dplyr::bind_rows(
      note(wanted, unname(status), filled, left),
      not_imputed
    )
  )
}

# A child's imputed value of a row that cannot change within a child is the
# value observed at another of their waves, or else the one imputed at
# their earliest wave.
imputation_hold_constant <- function(cells, data, base, constant) {
  for (name in constant) {
    own <- which(cells$variable == name)
    if (length(own) == 0) {
      next
    }
    person <- base$participant_id[cells$row[own]]
    if (anyNA(person)) {
      stop(
        "impute_dataset(): a row with no participant id cannot be held ",
        "constant within its child.",
        call. = FALSE
      )
    }
    observed <- data[[name]]
    seen <- !is.na(observed)
    # Sex is imputed as female or male, whatever else the child's record
    # says at another wave.
    if (name == "sex") {
      seen <- seen & observed %in% c("Female", "Male")
    }
    first_seen <- observed[seen][match(person, base$participant_id[seen])]
    order_in <- order(base$wave_order[cells$row[own]], cells$row[own])
    first_drawn <- cells$value[own][order_in][match(person, person[order_in])]
    cells$value[own] <- dplyr::coalesce(first_seen, first_drawn)
  }
  cells
}
