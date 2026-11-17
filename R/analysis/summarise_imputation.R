#' What imputation did across the datasets, in counts
#'
#' @param units The `units` tables of `review_imputation()`, bound.
#' @param estimates Stage 1 slopes fitted on imputed data, from
#'   `fit_stage1_imputed()`, bound: the tiers, and the models adjusted for
#'   the rest of the child's screen use.
#' @param complete The same cells fitted on complete cases, from
#'   `fit_stage1()`.
#' @return A list of tibbles: `variables`, per covariate the units and cells
#'   imputed and the units and cells left because too little was observed
#'   or the imputation failed; `fmi`, per covariate tier the distribution
#'   over unit slopes of the fraction of information missing, and how many
#'   slopes had screen use imputed and what share of their rows; and `gain`, per tier the slopes and
#'   rows fitted on complete cases and on imputed data.
summarise_imputation <- function(units, estimates, complete) {
  per_cell <- units$n_imputed / pmax(units$imputations, 1)
  units$cells_imputed <- per_cell
  units$outcome <- dplyr::case_when(
    units$status %in% "imputed" ~ "imputed",
    grepl("^too little", units$status) ~ "left: too little observed",
    grepl("^fewer rows", units$status) ~ "left: unit too small",
    grepl("^not needed", units$status) ~ "left: not needed",
    grepl("^takes one value", units$status) ~ "left: one value in the unit",
    .default = "failed or incomplete"
  )
  variables <- dplyr::summarise(
    units,
    datasets = dplyr::n_distinct(.data$dataset_id),
    units = dplyr::n(),
    cells_missing = sum(.data$n_missing),
    cells_imputed = sum(.data$cells_imputed),
    cells_age_group = sum(.data$n_age_group),
    .by = c("variable", "outcome")
  )
  # The models adjusted for the rest of the child's screen use are counted
  # apart from the others at the same tier.
  estimates$adjusted_other <- !is.na(estimates$n_no_other)
  complete$adjusted_other <- !is.na(complete$n_no_other)
  fitted <- estimates[estimates$status %in% "fitted", ]
  quantile_of <- function(x, p) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    stats::quantile(x, p, na.rm = TRUE, names = FALSE)
  }
  fmi <- dplyr::summarise(
    fitted,
    slopes = dplyr::n(),
    datasets = dplyr::n_distinct(.data$dataset_id),
    nothing_imputed = sum(.data$fmi %in% 0),
    fmi_median = quantile_of(.data$fmi, 0.5),
    fmi_q90 = quantile_of(.data$fmi, 0.9),
    fmi_max = quantile_of(.data$fmi, 1),
    fmi_over_half = sum(.data$fmi > 0.5, na.rm = TRUE),
    covariate_share_median = quantile_of(.data$covariate_imputed_share, 0.5),
    covariate_share_q90 = quantile_of(.data$covariate_imputed_share, 0.9),
    covariate_share_max = quantile_of(.data$covariate_imputed_share, 1),
    exposure_imputed = sum(.data$n_exposure_imputed > 0, na.rm = TRUE),
    exposure_failed = sum(!is.na(.data$exposure_imputation_failed)),
    exposure_share_median = quantile_of(
      (.data$n_exposure_imputed / .data$n_available)[
        .data$n_exposure_imputed > 0
      ],
      0.5
    ),
    exposure_share_max = quantile_of(
      .data$n_exposure_imputed / .data$n_available,
      1
    ),
    design_varied = sum(.data$design_varied),
    .by = c("tier", "adjusted_other")
  )
  count <- function(table, label) {
    out <- dplyr::summarise(
      table[table$status %in% "fitted", ],
      slopes = dplyr::n(),
      datasets = dplyr::n_distinct(.data$dataset_id),
      rows = sum(.data$n),
      .by = c("tier", "adjusted_other")
    )
    names(out)[-(1:2)] <- paste(names(out)[-(1:2)], label, sep = "_")
    out
  }
  partial <- dplyr::summarise(
    estimates[grepl("^fitted in", estimates$status), ],
    slopes_partly_fitted = dplyr::n(),
    .by = c("tier", "adjusted_other")
  )
  gain <- dplyr::left_join(
    dplyr::full_join(
      count(complete, "complete"),
      count(estimates, "imputed"),
      by = c("tier", "adjusted_other")
    ),
    partial,
    by = c("tier", "adjusted_other")
  )
  gain$slopes_partly_fitted <- dplyr::coalesce(gain$slopes_partly_fitted, 0L)
  list(
    variables = variables[order(variables$variable, variables$outcome), ],
    fmi = fmi[order(fmi$adjusted_other, fmi$tier), ],
    gain = gain[order(gain$adjusted_other, gain$tier), ]
  )
}
