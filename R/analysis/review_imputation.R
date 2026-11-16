#' Check a dataset's imputations, and set imputed beside observed values
#'
#' The checks are hard: any failure stops the build, because an imputation
#' that breaks one of them is wrong, not merely poor (ANALYSIS_PLAN.md,
#' Step 8 task 5). They apply to imputed cells only; observed values may
#' legitimately break them.
#'
#' - an imputed cell was missing before;
#' - only screen use and covariates are imputed, never an outcome;
#' - an imputed number lies within the values its unit observed, and
#'   between zero and the plausibility limit;
#' - an imputed category is one its unit observed;
#' - nothing is imputed for a variable its unit observed on less than the
#'   minimum share of rows;
#' - a variable that cannot change within a child has one value per child
#'   in each imputation;
#' - two imputations of the dataset are not the same draw.
#'
#' The summaries hold no row of data: per variable, the distribution of
#' the observed values and of the imputed ones, pooled over the dataset's
#' units and imputations.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param imputed Every imputation of the dataset, from
#'   `bind_imputations()`.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return NULL where the dataset is not included; otherwise a list of
#'   tibbles with `dataset_id`: `units` (the imputations' notes, counted
#'   over imputations), `numeric` (quantiles, mean and SD of observed and
#'   imputed values per variable) and `levels` (the share of each category
#'   among observed and imputed values).
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
  numeric <- imputed$numeric
  categorical <- imputed$categorical
  cells <- rbind(
    numeric[c("imputation", "row", "variable")],
    categorical[c("imputation", "row", "variable")]
  )
  must(
    all(numeric$variable %in% frame$exposures) &&
      all(categorical$variable %in% frame$covariates),
    "a variable was imputed that is not screen use or a covariate"
  )
  must(
    !anyDuplicated(cells) > 0,
    "a cell was imputed twice in one imputation"
  )
  was_missing <- function(table) {
    all(vapply(
      unique(table$variable),
      function(name) {
        all(is.na(data[[name]][table$row[table$variable == name]]))
      },
      logical(1)
    ))
  }
  must(was_missing(cells), "an observed value was imputed over")

  unit <- base$unit
  for (name in unique(numeric$variable)) {
    own <- numeric[numeric$variable == name, ]
    units_of <- unit[own$row]
    seen <- !is.na(data[[name]]) & unit %in% units_of
    low <- tapply(data[[name]][seen], unit[seen], min)
    high <- tapply(data[[name]][seen], unit[seen], max)
    must(
      all(own$value >= low[units_of] & own$value <= high[units_of]),
      paste0("`", name, "` imputed outside the values its unit observed")
    )
    must(
      all(own$value >= 0 & own$value <= settings$exposure_max_hours),
      paste0("`", name, "` imputed outside the plausible hours")
    )
  }
  for (name in unique(categorical$variable)) {
    own <- categorical[categorical$variable == name, ]
    seen <- paste(unit, data[[name]], sep = "\r")
    must(
      all(paste(unit[own$row], own$value, sep = "\r") %in% seen),
      paste0("`", name, "` imputed with a category its unit did not observe")
    )
  }
  # Sporadic missingness only.
  for (name in unique(cells$variable)) {
    share <- tapply(!is.na(data[[name]]), unit, mean)
    touched <- unique(unit[cells$row[cells$variable == name]])
    must(
      all(share[touched] >= settings$impute_min_observed),
      paste0("`", name, "` imputed in a unit that observed too little of it")
    )
  }
  for (name in frame$constant) {
    own <- categorical[categorical$variable == name, ]
    if (nrow(own) == 0) {
      next
    }
    seen <- !is.na(data[[name]])
    all_values <- rbind(
      tibble::tibble(
        imputation = own$imputation,
        person = base$participant_id[own$row],
        value = own$value
      ),
      tidyr::expand_grid(
        imputation = unique(own$imputation),
        tibble::tibble(
          person = base$participant_id[seen],
          value = data[[name]][seen]
        )
      )
    )
    all_values <- all_values[
      all_values$person %in% base$participant_id[own$row],
    ]
    per_person <- dplyr::summarise(
      all_values,
      values = dplyr::n_distinct(.data$value),
      .by = c("imputation", "person")
    )
    must(
      all(per_person$values == 1),
      paste0("`", name, "` differs within a child after imputation")
    )
  }
  indexes <- sort(unique(numeric$imputation))
  if (length(indexes) >= 2) {
    first <- numeric[numeric$imputation == indexes[1], ]
    second <- numeric[numeric$imputation == indexes[2], ]
    same_cells <- nrow(first) == nrow(second) &&
      all(first$row == second$row) &&
      all(first$variable == second$variable)
    must(same_cells, "two imputations filled different cells")
    # A handful of cells can match by chance; many cannot.
    must(
      nrow(first) < 20 || !all(first$value == second$value),
      "two imputations are the same draw"
    )
  }

  describe <- function(values, source, name) {
    tibble::tibble(
      variable = name,
      source = source,
      n = length(values),
      mean = mean(values),
      sd = stats::sd(values),
      q05 = stats::quantile(values, 0.05, names = FALSE),
      q25 = stats::quantile(values, 0.25, names = FALSE),
      q50 = stats::quantile(values, 0.50, names = FALSE),
      q75 = stats::quantile(values, 0.75, names = FALSE),
      q95 = stats::quantile(values, 0.95, names = FALSE)
    )
  }
  numeric_summary <- dplyr::bind_rows(lapply(
    unique(numeric$variable),
    function(name) {
      own <- numeric[numeric$variable == name, ]
      # Observed values of the units that had values imputed.
      in_units <- unit %in% unique(unit[own$row]) & !is.na(data[[name]])
      rbind(
        describe(data[[name]][in_units], "observed", name),
        describe(own$value, "imputed", name)
      )
    }
  ))
  level_summary <- dplyr::bind_rows(lapply(
    unique(categorical$variable),
    function(name) {
      own <- categorical[categorical$variable == name, ]
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
    .by = c("unit", "variable", "rows", "n_missing", "status")
  )
  with_id <- function(table) {
    table$dataset_id <- rep(id, nrow(table))
    dplyr::relocate(table, "dataset_id")
  }
  list(
    units = with_id(units),
    numeric = with_id(numeric_summary),
    levels = with_id(level_summary)
  )
}
