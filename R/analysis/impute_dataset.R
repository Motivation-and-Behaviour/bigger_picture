#' One imputation of a dataset's sporadically missing screen use and covariates
#'
#' Multiple imputation by chained equations (ANALYSIS_PLAN.md, Step 8;
#' DR-0019), one stage 1 unit (dataset by wave by country) at a time, so
#' that every relation it preserves is the unit's own. Each call makes one
#' imputation; the pipeline calls it once per imputation number.
#'
#' What is imputed, by predictive mean matching, so an imputed value is
#' always one the unit observed:
#'
#' - the exposures the models use: the included usual-day rows, and the
#'   derived total where it is a dedicated item. Where the total is a sum
#'   it is worked out again from its imputed parts;
#' - sex and the covariates of the further tiers.
#'
#' A variable is imputed in a unit only where its missingness is sporadic:
#' it is known for at least `impute_min_observed` of the unit's rows, on at
#' least the minimum number of rows, and takes two values or more. A
#' variable the unit never measured, or measured on too few rows, stays
#' missing, and the covariate tiers deal with it. Minority status is not
#' imputed in a unit that records ethnicity, which the tiers use first.
#'
#' What the imputation model also reads, so that the relations the analysis
#' estimates are not weakened in the imputed rows:
#'
#' - the outcomes, as one score per outcome family: the mean of a row's
#'   standardised scores in that family. Missing scores are imputed for
#'   this purpose and never kept or analysed;
#' - age, and the survey weight where the unit has one;
#' - in a panel, the child's mean on each exposure and outcome score over
#'   their other waves, with the unit's mean and a marker where they have
#'   no other wave.
#'
#' Predictors are chosen per variable by their correlation with it
#' (`mice::quickpred()`), always keeping age, sex and the outcome scores.
#'
#' Sex, ethnicity and minority status do not change within a child, so a
#' child imputed at several waves takes the value of their earliest wave.
#'
#' The seed of a unit's imputation comes from the project seed, the unit
#' and the imputation number, so an imputation is the same whichever
#' others are made, and whichever other units the dataset has.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param index The imputation number.
#' @param settings From `analysis_settings()`, with `seed`.
#' @return NULL where the dataset is not included; otherwise a list of
#'   three tibbles, each with `imputation`: `numeric` and `categorical`,
#'   the imputed cells as `row` (the row of the prepared tables),
#'   `variable` and `value`; and `units`, one row per unit and variable
#'   with missing values, saying how many were imputed or why none were.
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

  cells <- list()
  numbers <- list()
  notes <- list()
  for (unit in names(unit_rows)) {
    rows <- unit_rows[[unit]]
    seed <- digest::digest2int(paste(
      settings$seed,
      base$dataset_id[1],
      unit,
      index,
      sep = "|"
    ))
    done <- impute_unit(data[rows, , drop = FALSE], frame, seed, settings)
    done$units$unit <- rep(unit, nrow(done$units))
    notes[[unit]] <- done$units
    done$cells$row <- rows[done$cells$row]
    done$numbers$row <- rows[done$numbers$row]
    cells[[unit]] <- done$cells
    numbers[[unit]] <- done$numbers
  }
  empty <- function(value) {
    tibble::tibble(row = integer(), variable = character(), value = value)
  }
  cells <- dplyr::bind_rows(c(list(empty(character())), cells))
  numbers <- dplyr::bind_rows(c(list(empty(numeric())), numbers))
  cells <- imputation_hold_constant(cells, data, base, frame$constant)

  labelled <- function(table) {
    table$imputation <- rep(as.integer(index), nrow(table))
    dplyr::relocate(table, "imputation")
  }
  list(
    numeric = labelled(numbers),
    categorical = labelled(cells),
    units = labelled(dplyr::relocate(dplyr::bind_rows(notes), "unit"))
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
  # A total that is a sum is worked out again from its imputed parts
  # (`apply_imputation()`), so it is not imputed in its own right.
  summed <- prepared$exposures$total_source %in% "summed"
  data[[bp_total_exposure()]][summed] <- NA_real_
  for (covariate in covariates) {
    data[[covariate]] <- as.character(base[[covariate]])
  }
  data$aux_age <- as.numeric(base$age_years)
  weight <- as.numeric(base$survey_weight_norm)
  data$aux_weight <- ifelse(!is.na(weight) & weight > 0, log(weight), NA_real_)

  # One score per outcome family: the mean of the row's standardised scores.
  rows <- outcome_registry$rows
  scored <- intersect(
    rows$variable_name[rows$role %in% "outcome"],
    names(prepared$outcomes_z)
  )
  families <- rows$prereg_family[match(scored, rows$variable_name)]
  outcomes <- character()
  for (family in unique(stats::na.omit(families))) {
    scores <- as.matrix(prepared$outcomes_z[scored[families %in% family]])
    score <- rowMeans(scores, na.rm = TRUE)
    score[!is.finite(score)] <- NA_real_
    name <- paste0("aux_", family)
    data[[name]] <- score
    outcomes <- c(outcomes, name)
  }

  # In a panel, the child's mean over their other waves.
  other_waves <- character()
  if (anyDuplicated(base$participant_id) > 0) {
    for (name in c(exposures, outcomes)) {
      value <- data[[name]]
      seen <- !is.na(value)
      total <- stats::ave(
        ifelse(seen, value, 0),
        base$participant_id,
        FUN = sum
      )
      count <- stats::ave(as.numeric(seen), base$participant_id, FUN = sum)
      others <- count - seen
      mean_others <- (total - ifelse(seen, value, 0)) / others
      has <- others > 0
      if (!any(has)) {
        next
      }
      unit_mean <- stats::ave(
        ifelse(has, mean_others, NA_real_),
        base$unit,
        FUN = function(x) mean(x, na.rm = TRUE)
      )
      filled <- ifelse(has, mean_others, unit_mean)
      filled[!is.finite(filled)] <- 0
      data[[paste0("aux_other_", name)]] <- filled
      data[[paste0("aux_none_", name)]] <- as.numeric(!has)
      other_waves <- c(
        other_waves,
        paste0("aux_other_", name),
        paste0("aux_none_", name)
      )
    }
  }
  list(
    data = data,
    exposures = exposures,
    covariates = covariates,
    outcomes = outcomes,
    always = c("aux_age", "sex", outcomes),
    constant = intersect(covariates, c("sex", "ethnicity", "minority"))
  )
}

# Impute one unit once. Returns the imputed cells of the variables that are
# kept (`row` within the unit), numbers in `numbers` and categories in
# `cells`, and a note per variable with missing values.
impute_unit <- function(data, frame, seed, settings) {
  kept <- c(frame$exposures, frame$covariates)
  n <- nrow(data)
  known <- vapply(data, function(x) sum(!is.na(x)), numeric(1))
  values <- vapply(data, function(x) dplyr::n_distinct(x, na.rm = TRUE), 1)
  missing <- names(data)[known < n & known > 0]
  sporadic <- known / n >= settings$impute_min_observed &
    known >= settings$min_unit_rows &
    values >= 2
  targets <- intersect(missing, names(data)[sporadic])
  # Minority status follows from ethnicity, and the covariate tiers take
  # ethnicity where a unit has it, so minority status is then left alone:
  # a model cannot hold both.
  has_ethnicity <- "ethnicity" %in% names(data)[sporadic]
  unneeded <- if (has_ethnicity) intersect("minority", missing) else character()
  targets <- setdiff(targets, unneeded)
  usable <- setdiff(names(data), if (has_ethnicity) "minority")
  note <- function(variables, status, imputed = 0L) {
    tibble::tibble(
      variable = variables,
      rows = rep(n, length(variables)),
      n_missing = as.integer(n - known[variables]),
      n_imputed = rep_len(as.integer(imputed), length(variables)),
      status = rep_len(status, length(variables))
    )
  }
  none <- list(
    cells = tibble::tibble(
      row = integer(),
      variable = character(),
      value = character()
    ),
    numbers = tibble::tibble(
      row = integer(),
      variable = character(),
      value = numeric()
    )
  )
  reported <- intersect(kept, names(data)[known < n & known > 0])
  if (n < settings$min_unit_rows) {
    none$units <- note(reported, "fewer rows than the minimum unit size")
    return(none)
  }
  left <- setdiff(reported, c(targets, unneeded))
  not_sporadic <- dplyr::bind_rows(
    note(left, "too little observed in the unit to impute"),
    note(unneeded, "not needed: the unit records ethnicity")
  )
  if (length(intersect(targets, kept)) == 0) {
    none$units <- not_sporadic
    return(none)
  }

  # The model's columns: what is imputed, and what is complete and varies.
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
  wanted <- intersect(targets, kept)
  if (is.character(fit)) {
    none$units <- dplyr::bind_rows(
      note(wanted, paste("imputation failed:", fit)),
      not_sporadic
    )
    return(none)
  }
  drawn_cells <- function(names, convert) {
    out <- lapply(names, function(name) {
      drawn <- fit$imp[[name]]
      tibble::tibble(
        row = match(rownames(drawn), rownames(model)),
        variable = rep(name, nrow(drawn)),
        value = convert(drawn[[1]])
      )
    })
    start <- tibble::tibble(
      row = integer(),
      variable = character(),
      value = convert(character())
    )
    out <- dplyr::bind_rows(c(list(start), out))
    out[!is.na(out$value), ]
  }
  is_number <- wanted %in% frame$exposures
  numbers <- drawn_cells(wanted[is_number], as.numeric)
  cells <- drawn_cells(wanted[!is_number], as.character)
  filled <- table(factor(c(cells$variable, numbers$variable), levels = wanted))
  status <- ifelse(
    filled == n - known[wanted],
    "imputed",
    "some values could not be imputed"
  )
  list(
    cells = cells,
    numbers = numbers,
    units = dplyr::bind_rows(
      note(wanted, unname(status), as.integer(filled)),
      not_sporadic
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
    observed <- data[[name]]
    seen <- !is.na(observed)
    first_seen <- observed[seen][match(person, base$participant_id[seen])]
    order_in <- order(base$wave_order[cells$row[own]], cells$row[own])
    first_drawn <- cells$value[own][order_in][match(person, person[order_in])]
    cells$value[own] <- dplyr::coalesce(first_seen, first_drawn)
  }
  cells
}
