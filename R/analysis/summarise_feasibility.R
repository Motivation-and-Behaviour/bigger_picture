#' What the signed-off datasets can support, cell by cell
#'
#' The feasibility report's tables (ANALYSIS_PLAN.md, Step 7 task 7): for
#' every exposure and every level of the outcome tree, how many datasets,
#' units and participants could enter a model, and whether that is enough to
#' pool; and, for each specific outcome, how many datasets each covariate
#' tier keeps, which moderators have two levels to compare, and whether a
#' trend over collection years could run. Built from `count_cells()` alone,
#' so it shows no association.
#'
#' A unit counts towards a cell where it has the minimum number of rows with
#' both the exposure and the outcome row, and a cell can be pooled from the
#' minimum number of datasets. Participants are counted once per dataset and
#' country: the largest unit and row a dataset gives a cell, summed over
#' countries and datasets, since a panel's waves hold the same children.
#'
#' @param cells The `cells` tables of `count_cells()`, bound across datasets.
#' @param unit_measures The `units` tables of `count_cells()`, bound.
#' @param units Per-unit descriptions, as the `units` table of
#'   `describe_dataset()`.
#' @param outcome_registry,exposure_registry The registries.
#' @param datasets The table from `read_analysis_datasets()`.
#' @param settings From `analysis_settings()`.
#' @return A list of tibbles: `levels` (one row per exposure row, tree level
#'   and group), `tiers`, `moderators` and `years` (one row per exposure row
#'   and specific outcome, the moderators also by moderator and level), and
#'   `scope` (the datasets counted).
summarise_feasibility <- function(
  cells,
  unit_measures,
  units,
  outcome_registry,
  exposure_registry,
  datasets,
  settings = analysis_settings()
) {
  signed <- datasets$dataset_id[datasets$signed_off %in% TRUE]
  scope <- tibble::tibble(
    datasets_with_cells = dplyr::n_distinct(cells$dataset_id),
    datasets_signed_off = sum(unique(cells$dataset_id) %in% signed)
  )
  cells <- cells[
    cells$dataset_id %in% signed & cells$n_rows >= settings$min_unit_rows,
  ]
  rows <- outcome_registry$rows
  tree <- outcome_registry$tree
  at <- match(cells$outcome, rows$variable_name)
  cells$node <- rows$node[at]
  cells$informant <- dplyr::coalesce(rows$informant[at], "not recorded")
  cells$family <- tree$parent[match(cells$node, tree$node)]
  cells$pools <- rows$synthesis[at] %in%
    TRUE &
    rows$direction[at] %in% c("higher_better", "higher_worse") &
    tree$level[match(cells$node, tree$node)] %in% "L1"
  unit <- match(cells$unit, units$unit)
  cells$country <- as.character(units$country[unit])
  cells$year <- units$year[unit]
  cells$measure_type <- as.character(units$measure_type[unit])
  cells$responder <- as.character(units$responder[unit])

  count <- function(data, ...) {
    grouped <- dplyr::group_by(data, .data$exposure, ...)
    per_country <- dplyr::summarise(
      dplyr::group_by(grouped, .data$dataset_id, .data$country, .add = TRUE),
      participants = max(.data$n_participants),
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
  pooling <- cells[cells$pools, ]
  l3 <- count(cells, .data$family, .data$node, .data$informant, .data$outcome)
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
  levels <- dplyr::bind_rows(l0, l1, l2, l3)
  at <- match(levels$exposure, exposure_registry$variable_name)
  levels$exposure_group <- dplyr::coalesce(
    exposure_registry$exposure[at],
    "total"
  )
  levels$day <- dplyr::coalesce(exposure_registry$day[at], "usual")
  levels$exposure_headline <- dplyr::coalesce(
    exposure_registry$headline[at],
    levels$exposure %in% "total_screen"
  )
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

  # Tiers: the datasets with a countable unit that records the covariates.
  measures <- unit_measures[match(pooling$unit, unit_measures$unit), ]
  pooling$tier2 <- !is.na(measures$ses_measure)
  pooling$tier3 <- pooling$tier2 &
    !is.na(measures$ethnicity_measure) &
    !is.na(measures$urbanicity_measure)
  datasets_where <- function(flag, dataset) dplyr::n_distinct(dataset[flag])
  tiers <- dplyr::summarise(
    dplyr::group_by(pooling, .data$exposure, .data$family, .data$node),
    k_datasets_tier1 = dplyr::n_distinct(.data$dataset_id),
    k_datasets_tier2 = datasets_where(.data$tier2, .data$dataset_id),
    k_datasets_tier3 = datasets_where(.data$tier3, .data$dataset_id),
    .groups = "drop"
  )

  # Moderators: the datasets at each level of the labels known before a fit.
  screen <- unname(c(
    "Self" = "self",
    "Parent or carer" = "parent",
    "Teacher" = "teacher",
    "Device or objective measure" = "device"
  )[pooling$responder])
  known <- pooling$informant %in%
    c("self", "parent", "teacher", "task") &
    !is.na(screen)
  pooling$reporter <- ifelse(
    !known,
    NA_character_,
    ifelse(pooling$informant == screen, "same reporter", "different reporter")
  )
  pooling$informant_level <- ifelse(
    pooling$informant %in% c("mixed", "not recorded"),
    NA_character_,
    pooling$informant
  )
  pooling$instrument <- pooling$outcome
  labels <- c(
    informant = "informant_level",
    reporter = "reporter",
    measure_type = "measure_type",
    responder = "responder",
    instrument = "instrument"
  )
  moderators <- dplyr::bind_rows(lapply(names(labels), function(moderator) {
    data <- pooling
    data$level <- data[[labels[[moderator]]]]
    out <- dplyr::summarise(
      dplyr::group_by(
        data[!is.na(data$level), ],
        .data$exposure,
        .data$family,
        .data$node,
        .data$level
      ),
      k_datasets = dplyr::n_distinct(.data$dataset_id),
      .groups = "drop"
    )
    out$moderator <- rep(moderator, nrow(out))
    out$enough <- out$k_datasets >= settings$min_datasets
    out
  }))
  moderators <- dplyr::mutate(
    dplyr::group_by(
      moderators,
      .data$exposure,
      .data$node,
      .data$moderator
    ),
    comparable = sum(.data$enough) >= 2
  )
  moderators <- dplyr::relocate(
    dplyr::ungroup(moderators),
    "moderator",
    "level",
    .after = "node"
  )

  years <- dplyr::summarise(
    dplyr::group_by(
      pooling[!is.na(pooling$year), ],
      .data$exposure,
      .data$family,
      .data$node
    ),
    k_datasets = dplyr::n_distinct(.data$dataset_id),
    first_year = min(.data$year),
    last_year = max(.data$year),
    .groups = "drop"
  )
  years$trend_can_run <- years$k_datasets >= settings$year_min_datasets &
    years$last_year - years$first_year >= settings$year_min_span

  list(
    levels = levels,
    tiers = tiers,
    moderators = moderators,
    years = years,
    scope = scope
  )
}
