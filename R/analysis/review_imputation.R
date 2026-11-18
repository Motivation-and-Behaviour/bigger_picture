#' Check a dataset's imputed covariates, and set them beside observed ones
#'
#' The checks are hard: any failure stops the build, because an imputation
#' that breaks one of them is wrong, not merely poor (ANALYSIS_PLAN.md,
#' Step 8 task 5). They apply to imputed cells only; observed values may
#' legitimately break them.
#'
#' - an imputed cell was missing before, and is imputed once;
#' - only sex and the tier covariates are imputed here;
#' - an imputed category is one its unit observed; for a row that cannot
#'   change within a child it is the child's own record where they have
#'   one, and otherwise a category the dataset holds;
#' - imputed sex is female or male;
#' - nothing is imputed for a covariate its unit observed on less than the
#'   minimum share of rows, or for a child at an age at which the unit
#'   observed it on too few;
#' - the imputed cells of a row that cannot change within a child agree
#'   for each child in each imputation;
#' - the cells are exactly those each imputation recorded filling, and
#'   every imputation covers the same units and variables.
#'
#' Whether two imputations are the same draw is not checked on their
#' values: legitimate imputations can be identical (a covariate settled by
#' another, or by a child's own record), and each imputation's seed is
#' distinct by construction and tested.
#'
#' Imputed screen use is checked where it is drawn
#' (`impute_cell_exposure()`).
#'
#' The summaries hold no row of data.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param imputed Every imputation of the dataset, from
#'   `bind_imputations()`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return NULL where the dataset is not included; otherwise a list of
#'   tibbles with `dataset_id`: `units` (the imputations' notes, counted
#'   over imputations) and `levels` (the share of each category among
#'   observed and imputed values, in the units that had values imputed).
review_imputation <- function(
  prepared,
  imputed,
  outcome_registry,
  exposure_registry,
  settings = default_analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  id <- as.character(base$dataset_id[1])
  frame <- imputation_frame(prepared, outcome_registry, exposure_registry)
  data <- frame$data
  cells <- imputed$cells
  unit <- base$unit
  must <- function(ok, what) {
    if (!isTRUE(ok)) {
      stop(
        "Imputation check failed for dataset ",
        id,
        ": ",
        what,
        call. = FALSE
      )
    }
  }
  must(
    all(cells$variable %in% frame$covariates),
    "a variable was imputed that is not a covariate"
  )
  must(
    !anyDuplicated(cells[c("imputation", "row", "variable")]) > 0,
    "a cell was imputed twice in one imputation"
  )
  age_year <- floor(data$aux_age)
  for (name in unique(cells$variable)) {
    own <- cells[cells$variable == name, ]
    seen <- !is.na(data[[name]])
    must(
      !any(seen[own$row]),
      paste0("an observed `", name, "` was imputed over")
    )
    # A row that cannot change within a child may take the value drawn at
    # the child's earliest wave, so it need only be one the dataset holds;
    # and where the child has a record of their own it must be that.
    if (name %in% frame$constant) {
      ok <- own$value %in% data[[name]]
      record <- data[[name]]
      if (name == "sex") {
        record[!record %in% c("Female", "Male")] <- NA
      }
      known <- !is.na(record)
      children <- base$participant_id[own$row]
      recorded <- paste(children, own$value, sep = "\r") %in%
        paste(base$participant_id[known], record[known], sep = "\r")
      must(
        all(recorded | !children %in% base$participant_id[known]),
        paste0("`", name, "` imputed against the child's own record")
      )
    } else {
      in_unit <- paste(unit, data[[name]], sep = "\r")
      ok <- paste(unit[own$row], own$value, sep = "\r") %in% in_unit
    }
    must(
      all(ok),
      paste0("`", name, "` imputed with a category its unit did not observe")
    )
    share <- stats::ave(as.numeric(seen), unit, FUN = mean)
    must(
      all(share[own$row] >= settings$impute_min_observed),
      paste0("`", name, "` imputed in a unit that observed too little of it")
    )
    by_age <- stats::ave(as.numeric(seen), unit, age_year, FUN = mean)
    must(
      all(!is.na(age_year[own$row])) &&
        all(by_age[own$row] >= settings$impute_min_observed_age),
      paste0("`", name, "` imputed for an age group that observed too little")
    )
  }
  must(
    all(cells$value[cells$variable == "sex"] %in% c("Female", "Male")),
    "sex imputed as something other than female or male"
  )
  # Imputed cells of one child agree with each other. Observed values may
  # differ between waves, and are not held to this.
  for (name in frame$constant) {
    own <- cells[cells$variable == name, ]
    per_person <- dplyr::summarise(
      tibble::tibble(
        imputation = own$imputation,
        person = base$participant_id[own$row],
        value = own$value
      ),
      values = dplyr::n_distinct(.data$value),
      .by = c("imputation", "person")
    )
    must(
      all(per_person$values == 1),
      paste0("`", name, "` differs within a child after imputation")
    )
  }
  # The cells are the ones the imputations' notes say were filled, so an
  # imputation with cells missing is caught here. One that was never made
  # is caught by `fit_stage1_imputed()`, which knows how many were asked
  # for.
  noted <- imputed$units[imputed$units$n_imputed > 0, ]
  filled <- dplyr::count(
    tibble::tibble(
      imputation = cells$imputation,
      unit = unit[cells$row],
      variable = cells$variable
    ),
    .data$imputation,
    .data$unit,
    .data$variable,
    name = "cells"
  )
  both <- dplyr::full_join(
    noted[c("imputation", "unit", "variable", "n_imputed")],
    filled,
    by = c("imputation", "unit", "variable")
  )
  must(
    !anyNA(both$n_imputed) &&
      !anyNA(both$cells) &&
      all(both$n_imputed == both$cells),
    "the imputed cells are not the ones the imputations recorded"
  )
  per_index <- dplyr::count(imputed$units, .data$imputation)
  must(
    dplyr::n_distinct(per_index$n) <= 1,
    "the imputations do not cover the same units and variables"
  )

  level_summary <- dplyr::bind_rows(lapply(
    unique(cells$variable),
    function(name) {
      own <- cells[cells$variable == name, ]
      in_units <- unit %in% unique(unit[own$row]) & !is.na(data[[name]])
      shares <- function(values, source) {
        counts <- table(values)
        tibble::tibble(
          variable = name,
          source = source,
          level = names(counts),
          n = as.integer(counts),
          share = as.numeric(counts) / sum(counts)
        )
      }
      rbind(
        shares(data[[name]][in_units], "observed"),
        shares(own$value, "imputed")
      )
    }
  ))
  units <- dplyr::summarise(
    imputed$units,
    imputations = dplyr::n(),
    n_imputed = sum(.data$n_imputed),
    .by = c("unit", "variable", "rows", "n_missing", "n_age_group", "status")
  )
  with_id <- function(table) {
    table$dataset_id <- rep(id, nrow(table))
    dplyr::relocate(table, "dataset_id")
  }
  list(
    dataset_id = id,
    units = with_id(units),
    levels = with_id(level_summary)
  )
}
