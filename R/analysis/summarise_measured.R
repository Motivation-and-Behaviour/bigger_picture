#' What was measured: coverage of every outcome node and exposure
#'
#' For the descriptive sections: how many datasets, units and observations
#' stand behind each specific outcome and each screen measure, and which
#' informants reported it (ANALYSIS_PLAN.md, Step 6). Counts are of what the
#' preparation step used, from the row audit.
#'
#' @param row_audit The combined row audit.
#' @param outcome_registry The outcome registry.
#' @param exposure_registry The exposure registry.
#' @return A list of two tibbles: `outcomes` (one row per family and node)
#'   and `exposures` (one row per exposure).
summarise_measured <- function(row_audit, outcome_registry, exposure_registry) {
  used <- row_audit[row_audit$status %in% c("used", "derived"), ]
  rows <- outcome_registry$rows
  tree <- outcome_registry$tree
  outcome_rows <- dplyr::inner_join(
    used[used$kind %in% "outcome", ],
    rows[c(
      "variable_name",
      "node",
      "prereg_family",
      "informant",
      "score_type"
    )],
    by = "variable_name"
  )
  outcome_rows <- outcome_rows[!is.na(outcome_rows$node), ]
  outcomes <- dplyr::summarise(
    outcome_rows,
    datasets = dplyr::n_distinct(.data$dataset_id),
    dataset_waves = dplyr::n_distinct(paste(.data$dataset_id, .data$wave)),
    observations = sum(.data$n_used),
    schema_rows = dplyr::n_distinct(.data$variable_name),
    informants = paste(sort(unique(.data$informant)), collapse = ", "),
    score_types = paste(sort(unique(.data$score_type)), collapse = ", "),
    .by = c("prereg_family", "node")
  )
  outcomes <- dplyr::left_join(
    outcomes,
    tree[c("node", "label", "headline")],
    by = "node"
  )
  outcomes <- outcomes[order(outcomes$prereg_family, -outcomes$datasets), ]

  exposure_rows <- dplyr::inner_join(
    used[used$kind %in% "exposure", ],
    exposure_registry[c(
      "variable_name",
      "exposure",
      "content_class",
      "axis",
      "day",
      "context"
    )],
    by = "variable_name"
  )
  exposures <- dplyr::summarise(
    exposure_rows,
    datasets = dplyr::n_distinct(.data$dataset_id),
    dataset_waves = dplyr::n_distinct(paste(.data$dataset_id, .data$wave)),
    observations = sum(.data$n_used),
    .by = c("exposure", "content_class", "axis", "context", "day")
  )
  exposures <- exposures[order(-exposures$datasets), ]
  list(outcomes = outcomes, exposures = exposures)
}
