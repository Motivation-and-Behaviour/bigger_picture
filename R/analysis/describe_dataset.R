#' Descriptive statistics of one dataset, per unit
#'
#' Everything the descriptive sections of the reports need, computed within
#' each stage 1 unit so that no dataset label or row of data leaves this
#' function, and so that pooling can count each dataset once
#' (ANALYSIS_PLAN.md, Step 6). Survey weights are used where a unit has
#' them. A statistic is given only for a group with at least the minimum
#' number of rows. Screen use and outcomes are described separately: nothing
#' here relates one to the other.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @param settings From `analysis_settings()`.
#' @return A named list of tibbles, each with `dataset_id`, `unit`, `wave`,
#'   `country` and `year` (the unit's median collection year):
#'   `screen` (distribution of each screen measure, overall and by school
#'   stage, sex and household SES), `week` (weekday against weekend on the
#'   same rows), `relations` (rank correlations between screen measures),
#'   `outcomes` (prevalence of binary outcomes, overall and by school stage)
#'   and `units` (how each unit measured screen use, and its design). NULL
#'   when the dataset is not included.
describe_dataset <- function(
  prepared,
  outcome_registry,
  exposure_registry,
  settings = analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  exposures <- prepared$exposures
  n <- nrow(base)
  column <- function(name) {
    if (name %in% names(base)) base[[name]] else rep(NA, n)
  }
  weight <- as.numeric(base$survey_weight_norm)
  unit_weighted <- stats::ave(
    !is.na(weight) & weight > 0,
    base$unit,
    FUN = any
  ) >
    0
  # In a weighted unit a row without a weight is left out, as in stage 1.
  weight <- ifelse(unit_weighted, weight, 1)
  minimum <- settings$min_unit_rows
  rows_by_unit <- split(seq_len(n), base$unit)

  groupings <- list(
    all = rep("all", n),
    school_stage = as.character(base$school_stage),
    sex = as.character(column("sex")),
    ses_household = as.character(column("ses_household"))
  )
  hours <- exposure_registry[exposure_registry$unit %in% "hours", ]
  described <- intersect(
    c(
      "total_screen",
      hours$variable_name[
        hours$include %in% TRUE & hours$context %in% "any"
      ]
    ),
    names(exposures)
  )

  screen <- list()
  for (variable in described) {
    values <- exposures[[variable]]
    for (grouping in names(groupings)) {
      group <- groupings[[grouping]]
      for (unit in names(rows_by_unit)) {
        index <- rows_by_unit[[unit]]
        index <- index[!is.na(values[index]) & !is.na(group[index])]
        for (level in unique(group[index])) {
          rows <- index[group[index] == level]
          rows <- rows[!is.na(weight[rows]) & weight[rows] > 0]
          if (length(rows) < minimum) {
            next
          }
          stats <- weighted_summary(values[rows], weight[rows])
          stats$unit <- unit
          stats$variable_name <- variable
          stats$grouping <- grouping
          stats$group <- level
          screen[[length(screen) + 1]] <- stats
        }
      }
    }
  }

  # Weekday against weekend, on rows that have both.
  days <- hours[
    hours$day %in% c("weekday", "weekend") & hours$context %in% "any",
  ]
  pairs <- tidyr::pivot_wider(
    days[c("exposure", "day", "variable_name")],
    names_from = "day",
    values_from = "variable_name"
  )
  week <- list()
  if (all(c("weekday", "weekend") %in% names(pairs))) {
    pairs <- pairs[
      pairs$weekday %in% names(exposures) & pairs$weekend %in% names(exposures),
    ]
    for (i in seq_len(nrow(pairs))) {
      weekday <- exposures[[pairs$weekday[i]]]
      weekend <- exposures[[pairs$weekend[i]]]
      for (unit in names(rows_by_unit)) {
        rows <- rows_by_unit[[unit]]
        rows <- rows[
          !is.na(weekday[rows]) &
            !is.na(weekend[rows]) &
            !is.na(weight[rows]) &
            weight[rows] > 0
        ]
        if (length(rows) < minimum) {
          next
        }
        w <- weight[rows]
        week[[length(week) + 1]] <- tibble::tibble(
          unit = unit,
          exposure = pairs$exposure[i],
          n = length(rows),
          mean_weekday = sum(w * weekday[rows]) / sum(w),
          mean_weekend = sum(w * weekend[rows]) / sum(w),
          share_more_on_weekend = sum(w[weekend[rows] > weekday[rows]]) / sum(w)
        )
      }
    }
  }

  # How the screen measures relate: rank correlations within unit.
  related <- intersect(
    hours$variable_name[
      hours$include %in%
        TRUE &
        hours$day %in% "usual" &
        hours$context %in% "any" &
        hours$axis %in% c("activity", "device")
    ],
    names(exposures)
  )
  relations <- list()
  if (length(related) > 1) {
    combinations <- utils::combn(related, 2)
    for (j in seq_len(ncol(combinations))) {
      a <- exposures[[combinations[1, j]]]
      b <- exposures[[combinations[2, j]]]
      for (unit in names(rows_by_unit)) {
        rows <- rows_by_unit[[unit]]
        rows <- rows[!is.na(a[rows]) & !is.na(b[rows])]
        if (
          length(rows) < minimum ||
            stats::sd(a[rows]) == 0 ||
            stats::sd(b[rows]) == 0
        ) {
          next
        }
        relations[[length(relations) + 1]] <- tibble::tibble(
          unit = unit,
          variable_a = combinations[1, j],
          variable_b = combinations[2, j],
          n = length(rows),
          spearman = stats::cor(a[rows], b[rows], method = "spearman"),
          share_both = mean(a[rows] > 0 & b[rows] > 0)
        )
      }
    }
  }

  # Prevalence of binary outcomes.
  registry <- outcome_registry$rows
  binary <- intersect(
    registry$variable_name[
      registry$score_type %in% "binary" & registry$role %in% "outcome"
    ],
    names(prepared$outcomes)
  )
  outcomes <- list()
  for (variable in binary) {
    values <- prepared$outcomes[[variable]]
    for (grouping in c("all", "school_stage")) {
      group <- groupings[[grouping]]
      for (unit in names(rows_by_unit)) {
        index <- rows_by_unit[[unit]]
        index <- index[!is.na(values[index]) & !is.na(group[index])]
        for (level in unique(group[index])) {
          rows <- index[group[index] == level]
          rows <- rows[!is.na(weight[rows]) & weight[rows] > 0]
          if (length(rows) < minimum) {
            next
          }
          w <- weight[rows]
          outcomes[[length(outcomes) + 1]] <- tibble::tibble(
            unit = unit,
            variable_name = variable,
            grouping = grouping,
            group = level,
            n = length(rows),
            prevalence = sum(w * values[rows]) / sum(w)
          )
        }
      }
    }
  }

  modal <- function(values) {
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(NA_character_)
    }
    names(sort(table(values), decreasing = TRUE))[1]
  }
  units <- dplyr::summarise(
    tibble::tibble(
      unit = base$unit,
      dataset_id = base$dataset_id,
      wave = base$wave,
      country = as.character(column("country")),
      year = as.numeric(column("data_year")),
      age = as.numeric(base$age_years),
      participant = base$participant_id,
      measure_type = as.character(column("st_measure_type")),
      responder = as.character(column("st_responder")),
      weighted = unit_weighted,
      total_source = exposures$total_source,
      total_coverage = exposures$total_coverage
    ),
    dataset_id = .data$dataset_id[1],
    wave = .data$wave[1],
    country = .data$country[1],
    year = bp_stat(.data$year, stats::median),
    rows = dplyr::n(),
    participants = dplyr::n_distinct(.data$participant),
    age_median = bp_stat(.data$age, stats::median),
    measure_type = modal(.data$measure_type),
    responder = modal(.data$responder),
    weighted = any(.data$weighted),
    total_source = modal(.data$total_source),
    total_coverage = bp_stat(.data$total_coverage, max),
    .by = "unit"
  )
  # A panel follows the same participants: more rows than participants.
  units$repeats_participants <- nrow(base) >
    1.2 * dplyr::n_distinct(base$participant_id)

  attach_unit <- function(parts) {
    out <- dplyr::bind_rows(parts)
    if (nrow(out) == 0) {
      return(NULL)
    }
    out <- dplyr::left_join(
      out,
      units[c(
        "unit",
        "dataset_id",
        "wave",
        "country",
        "year",
        "repeats_participants"
      )],
      by = "unit"
    )
    dplyr::relocate(out, "dataset_id", "unit", "wave", "country", "year")
  }
  list(
    screen = attach_unit(screen),
    week = attach_unit(week),
    relations = attach_unit(relations),
    outcomes = attach_unit(outcomes),
    units = units
  )
}
