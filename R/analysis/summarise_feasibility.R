#' What the signed-off datasets can support, cell by cell
#'
#' The feasibility report's tables: for
#' every exposure and every level of the outcome tree, how many datasets,
#' units and participants could enter a model, and whether that is enough to
#' pool; and how many each covariate tier keeps, which moderators have two
#' levels to compare, whether a trend over collection years could run, and
#' which datasets could answer the school against home question. Built from
#' `count_cells()` alone, so it shows no association.
#'
#' A unit counts towards a cell where `count_cells()` finds it usable, and a
#' row, a specific outcome by informant or a specific outcome can be pooled
#' from the minimum number of datasets. A family can be pooled where at
#' least one of its specific outcomes can, as `pool_tree_family()` needs.
#'
#' Participants are a lower bound. Within a unit a group takes its largest
#' outcome row, since several rows are mostly the same children. Across a
#' dataset's units of one country, a dataset that follows participants
#' takes its largest unit, and one that does not (repeated cross-sections,
#' separate samples) sums them.
#'
#' The tier counts are approximate: a unit's covariates are judged on all
#' its rows, where stage 1 judges them on each cell's rows and also needs a
#' minimum share of rows with all of them.
#'
#' @param cells The `cells` tables of `count_cells()`, bound across datasets.
#' @param unit_measures The `units` tables of `count_cells()`, bound.
#' @param units Per-unit descriptions, as the `units` table of
#'   `describe_dataset()`.
#' @param outcome_registry,exposure_registry The registries.
#' @param datasets The table from `read_analysis_datasets()`.
#' @param mapping_status See `tree_moderators()`.
#' @param settings From `analysis_settings()`.
#' @return A list of tibbles: `levels` (one row per exposure row, tree level
#'   and group); `tiers`, `moderators` and `years` (per exposure row and
#'   specific outcome; the years also per family and outcome row); `days`
#'   (datasets under a usual-day row against any day's row); `school_home`;
#'   `not_usable` (unit cells by the check they fail); and `scope`.
summarise_feasibility <- function(
  cells,
  unit_measures,
  units,
  outcome_registry,
  exposure_registry,
  datasets,
  mapping_status = NULL,
  settings = default_analysis_settings()
) {
  signed <- datasets$dataset_id[datasets$signed_off %in% TRUE]
  scope <- tibble::tibble(
    datasets_with_cells = dplyr::n_distinct(cells$dataset_id),
    datasets_signed_off = sum(unique(cells$dataset_id) %in% signed)
  )
  cells <- cells[cells$dataset_id %in% signed, ]
  not_usable <- dplyr::count(
    cells,
    reason = dplyr::coalesce(.data$not_usable, "usable"),
    name = "unit_cells"
  )
  cells <- cells[is.na(cells$not_usable), ]

  rows <- outcome_registry$rows
  place <- tree_place(cells$outcome, outcome_registry)
  cells$node <- place$node
  cells$family <- place$family
  cells$informant <- place$informant
  cells$pools <- is.na(place$left_out)
  at <- match(cells$outcome, rows$variable_name)
  cells$score_type <- rows$score_type[at]
  cells$role <- rows$role[at]
  unit <- match(cells$unit, units$unit)
  cells$country <- as.character(units$country[unit])
  cells$year <- units$year[unit]
  follows <- tapply(units$repeats_participants, units$dataset_id, any)
  cells$follows <- unname(follows[cells$dataset_id]) %in% TRUE
  at <- match(cells$exposure, exposure_registry$variable_name)
  cells$exposure_group <- dplyr::coalesce(
    exposure_registry$exposure[at],
    "total"
  )
  cells$day <- dplyr::coalesce(exposure_registry$day[at], "usual")
  cells$context <- dplyr::coalesce(exposure_registry$context[at], "any")
  cells$exposure_headline <- dplyr::coalesce(
    exposure_registry$headline[at],
    cells$exposure %in% "total_screen"
  )

  count <- function(data, ...) {
    grouped <- dplyr::group_by(data, .data$exposure, ...)
    if (nrow(data) == 0) {
      out <- dplyr::summarise(grouped, .groups = "drop")
      out$k_datasets <- integer()
      out$k_units <- integer()
      out$k_rows <- integer()
      out$participants <- integer()
      out$poolable <- logical()
      return(out)
    }
    per_unit <- dplyr::summarise(
      dplyr::group_by(
        grouped,
        .data$dataset_id,
        .data$country,
        .data$follows,
        .data$unit,
        .add = TRUE
      ),
      participants = max(.data$n_participants),
      .groups = "drop_last"
    )
    per_country <- dplyr::summarise(
      per_unit,
      participants = if (.data$follows[1]) {
        max(.data$participants)
      } else {
        sum(.data$participants)
      },
      .groups = "drop"
    )
    people <- dplyr::summarise(
      dplyr::group_by(per_country, .data$exposure, ...),
      participants = sum(.data$participants),
      .groups = "drop"
    )
    out <- dplyr::summarise(
      grouped,
      k_datasets = dplyr::n_distinct(.data$dataset_id),
      k_units = dplyr::n_distinct(.data$unit),
      k_rows = dplyr::n_distinct(.data$outcome),
      .groups = "drop"
    )
    out <- dplyr::left_join(
      out,
      people,
      by = setdiff(names(people), "participants")
    )
    out$poolable <- out$k_datasets >= settings$min_datasets
    out
  }
  any_location <- cells[cells$context != "school", ]
  pooling <- any_location[any_location$pools, ]
  l3 <- count(
    any_location,
    .data$family,
    .data$node,
    .data$informant,
    .data$outcome
  )
  l3$level <- "L3"
  l2 <- count(pooling, .data$family, .data$node, .data$informant)
  l2$level <- "L2"
  l1 <- count(pooling, .data$family, .data$node)
  l1$level <- "L1"
  l0 <- count(pooling, .data$family)
  l0$level <- "L0"
  nodes_poolable <- dplyr::count(
    l1[l1$poolable, ],
    .data$exposure,
    .data$family,
    name = "k_nodes_poolable"
  )
  l0 <- dplyr::left_join(l0, nodes_poolable, by = c("exposure", "family"))
  l0$k_nodes_poolable <- dplyr::coalesce(l0$k_nodes_poolable, 0L)
  # A family is the average of its pooled specific outcomes.
  l0$poolable <- l0$k_nodes_poolable >= 1
  levels <- dplyr::bind_rows(l0, l1, l2, l3)
  at <- match(levels$outcome, rows$variable_name)
  levels$role <- rows$role[at]
  levels$use <- rows$use[at]
  labels <- dplyr::distinct(cells[c(
    "exposure",
    "exposure_group",
    "day",
    "exposure_headline"
  )])
  levels <- dplyr::left_join(levels, labels, by = "exposure")
  levels <- dplyr::relocate(
    levels,
    "exposure_group",
    "exposure",
    "day",
    "exposure_headline",
    "level",
    "family",
    "node",
    "informant",
    "outcome"
  )

  # Tiers: the datasets with a usable unit that records the covariates.
  measures <- unit_measures[match(pooling$unit, unit_measures$unit), ]
  pooling$tier2 <- !is.na(measures$ses_measure)
  pooling$tier3 <- pooling$tier2 &
    !is.na(measures$ethnicity_measure) &
    !is.na(measures$urbanicity_measure)
  tier <- function(flag) {
    out <- count(pooling[flag, ], .data$family, .data$node)
    out[c("exposure", "family", "node", "k_datasets", "participants")]
  }
  by_node <- c("exposure", "family", "node")
  tiers <- tier(TRUE) |>
    dplyr::left_join(
      tier(pooling$tier2),
      by = by_node,
      suffix = c("", "_tier2")
    ) |>
    dplyr::left_join(
      tier(pooling$tier3),
      by = by_node,
      suffix = c("", "_tier3")
    )
  names(tiers)[4:5] <- c("k_datasets_tier1", "participants_tier1")
  counts <- grep("^(k_datasets|participants)_tier", names(tiers))
  tiers[counts] <- lapply(tiers[counts], function(x) dplyr::coalesce(x, 0L))

  # Moderators: the labels `tree_moderators()` gives, all known before a fit.
  labelled <- tree_moderators(pooling, units, mapping_status, datasets)
  moderators <- dplyr::bind_rows(lapply(bp_tree_moderators(), function(name) {
    data <- pooling
    data$level <- as.character(labelled[[name]])
    data <- data[!is.na(data$level), ]
    if (nrow(data) == 0) {
      return(NULL)
    }
    out <- count(data, .data$family, .data$node, .data$level)
    out$moderator <- rep(name, nrow(out))
    out$enough <- out$poolable
    out[c(
      by_node,
      "moderator",
      "level",
      "k_datasets",
      "participants",
      "enough"
    )]
  }))
  moderators <- dplyr::ungroup(dplyr::mutate(
    dplyr::group_by(moderators, .data$exposure, .data$node, .data$moderator),
    comparable = sum(.data$enough) >= 2
  ))

  # Years: whether a trend could run, and what its within-study part would
  # rest on (datasets of repeated cross-sections over more than one year).
  dated <- any_location[!is.na(any_location$year), ]
  trend <- function(data, level, ...) {
    out <- dplyr::summarise(
      dplyr::group_by(data, .data$exposure, ...),
      k_datasets = dplyr::n_distinct(.data$dataset_id),
      first_year = min(.data$year),
      last_year = max(.data$year),
      k_repeated_cross_sections = dplyr::n_distinct(.data$dataset_id[
        !.data$follows & .data$several_years
      ]),
      .groups = "drop"
    )
    out$level <- rep(level, nrow(out))
    out
  }
  spans <- function(data, ...) {
    dplyr::ungroup(dplyr::mutate(
      dplyr::group_by(data, .data$exposure, .data$dataset_id, ...),
      several_years = dplyr::n_distinct(.data$year) > 1
    ))
  }
  dated_pooling <- dated[dated$pools, ]
  years <- dplyr::bind_rows(
    trend(spans(dated_pooling, .data$family), "L0", .data$family),
    trend(
      spans(dated_pooling, .data$node),
      "L1",
      .data$family,
      .data$node
    ),
    trend(
      spans(dated, .data$outcome),
      "L3",
      .data$family,
      .data$node,
      .data$outcome
    )
  )
  years$trend_can_run <- years$k_datasets >= settings$year_min_datasets &
    years$last_year - years$first_year >= settings$year_min_span

  # Days: datasets that have an exposure only on a weekday or weekend row.
  days <- dplyr::summarise(
    dplyr::group_by(pooling, .data$exposure_group, .data$family),
    k_datasets_any_day = dplyr::n_distinct(.data$dataset_id),
    k_datasets_usual_day = dplyr::n_distinct(.data$dataset_id[
      .data$day == "usual"
    ]),
    .groups = "drop"
  )

  # School against home: a unit with a school-context row and the
  # any-location row of the same exposure, on the same outcome row.
  school <- cells[cells$context == "school" & cells$pools, ]
  school$any_location <- sub("_school$", "", school$exposure_group)
  matched <- dplyr::inner_join(
    school[c(
      "dataset_id",
      "unit",
      "outcome",
      "family",
      "exposure",
      "any_location"
    )],
    dplyr::distinct(any_location[c("unit", "outcome", "exposure_group")]),
    by = c("unit", "outcome", "any_location" = "exposure_group")
  )
  school_home <- dplyr::left_join(
    dplyr::summarise(
      dplyr::group_by(school, .data$exposure, .data$family),
      k_datasets_school = dplyr::n_distinct(.data$dataset_id),
      .groups = "drop"
    ),
    dplyr::summarise(
      dplyr::group_by(matched, .data$exposure, .data$family),
      k_datasets_matched = dplyr::n_distinct(.data$dataset_id),
      .groups = "drop"
    ),
    by = c("exposure", "family")
  )
  school_home$k_datasets_matched <- dplyr::coalesce(
    school_home$k_datasets_matched,
    0L
  )

  list(
    levels = levels,
    tiers = tiers,
    moderators = moderators,
    years = years,
    days = days,
    school_home = school_home,
    not_usable = not_usable,
    scope = scope
  )
}
