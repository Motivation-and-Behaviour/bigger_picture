#' The one-stage check on a cell's pooled slope
#'
#' Two-stage is the primary analysis (DR-0010). With matching assumptions a
#' one-stage model gives the same answer for a continuous outcome, so this
#' check tests the assumptions; it does not offer a second result to choose
#' from. The one-stage model has an intercept per unit, the exposure centred
#' within unit, a residual variance per unit and each unit's own age and sex
#' effects, with the slope on screen use varying between datasets and between
#' the units of a dataset, as stage 2 has it. It runs on complete cases and
#' without survey weights, so it is compared with a two-stage estimate built
#' the same way: unweighted per-unit linear models with linear age and sex,
#' pooled with the same two levels of variation. With only a dataset-level
#' slope in the one-stage model, the two differ for that reason alone.
#'
#' The comparison is on the contrast of 1 against 4 hours a day. A gap above
#' the trigger means the assumptions need a look; two-stage stays primary. A
#' model that fails to converge is reported as failed, never refitted in a
#' simpler form.
#'
#' @param rows Combined output of `extract_cell_rows()` for one cell.
#' @param settings From `analysis_settings()`.
#' @return A one-row tibble: the one-stage and matched two-stage slopes, the
#'   one-stage model-based and the two-stage robust standard error, the
#'   between-dataset and between-unit SDs from each, the gap on the contrast,
#'   whether it exceeds the trigger, and `status`.
fit_onestage_check <- function(rows, settings = analysis_settings()) {
  result <- function(status, ...) {
    values <- list(
      k_datasets = NA_integer_,
      k_units = NA_integer_,
      n = NA_integer_,
      onestage_estimate = NA_real_,
      onestage_se = NA_real_,
      onestage_tau = NA_real_,
      onestage_tau_unit = NA_real_,
      twostage_estimate = NA_real_,
      twostage_se = NA_real_,
      twostage_tau = NA_real_,
      twostage_tau_unit = NA_real_,
      contrast_gap = NA_real_,
      trigger = settings$onestage_trigger,
      exceeds_trigger = NA,
      status = status
    )
    extra <- list(...)
    values[names(extra)] <- extra
    tibble::as_tibble(values)
  }
  if (is.null(rows) || nrow(rows) == 0) {
    return(result("no rows"))
  }
  rows <- rows[!is.na(rows$age) & rows$sex %in% c("Female", "Male"), ]

  # Keep units the unweighted two-stage comparator can also fit.
  usable <- dplyr::summarise(
    rows,
    n = dplyr::n(),
    x_sd = stats::sd(.data$x),
    sexes = dplyr::n_distinct(.data$sex),
    age_sd = stats::sd(.data$age),
    .by = "unit"
  )
  usable <- usable[
    usable$n >= settings$min_unit_rows &
      usable$x_sd > 0 &
      usable$sexes == 2 &
      usable$age_sd > 0,
  ]
  rows <- rows[rows$unit %in% usable$unit, ]
  k_datasets <- dplyr::n_distinct(rows$dataset_id)
  counts <- list(
    k_datasets = k_datasets,
    k_units = dplyr::n_distinct(rows$unit),
    n = nrow(rows)
  )
  if (k_datasets < settings$min_datasets) {
    return(do.call(result, c(list(status = "insufficient data"), counts)))
  }

  centre <- function(values, group) {
    values - stats::ave(values, group, FUN = mean)
  }
  rows$x_c <- centre(rows$x, rows$unit)
  rows$age_c <- centre(rows$age, rows$unit)
  rows$unit <- factor(rows$unit)
  rows$dataset_id <- factor(rows$dataset_id)

  # Matched two-stage estimate: unweighted, linear age and sex per unit.
  per_unit <- lapply(split(rows, rows$unit, drop = TRUE), function(unit_rows) {
    fit <- stats::lm(y ~ x_c + age_c + sex, data = unit_rows)
    table <- stats::coef(summary(fit))
    tibble::tibble(
      dataset_id = as.character(unit_rows$dataset_id[1]),
      unit = as.character(unit_rows$unit[1]),
      estimate = table["x_c", "Estimate"],
      se = table["x_c", "Std. Error"],
      n = nrow(unit_rows),
      status = "fitted"
    )
  })
  twostage <- pool_stage2(dplyr::bind_rows(per_unit), settings)

  onestage <- tryCatch(
    glmmTMB::glmmTMB(
      y ~ 0 +
        unit +
        x_c +
        unit:age_c +
        unit:sex +
        (0 + x_c | dataset_id) +
        (0 + x_c | unit),
      dispformula = ~ 0 + unit,
      data = rows,
      REML = TRUE,
      control = glmmTMB::glmmTMBControl(
        optCtrl = list(iter.max = 5000, eval.max = 5000)
      )
    ),
    error = function(e) conditionMessage(e)
  )
  two <- list(
    twostage_estimate = twostage$estimate,
    twostage_se = twostage$se,
    twostage_tau = twostage$tau_dataset,
    twostage_tau_unit = twostage$tau_unit
  )
  if (is.character(onestage)) {
    return(do.call(
      result,
      c(list(status = paste("one-stage model failed:", onestage)), counts, two)
    ))
  }
  converged <- isTRUE(onestage$sdr$pdHess) && onestage$fit$convergence == 0
  if (!converged) {
    return(do.call(
      result,
      c(list(status = "one-stage model did not converge"), counts, two)
    ))
  }
  table <- stats::coef(summary(onestage))$cond
  estimate <- unname(table["x_c", "Estimate"])
  components <- glmmTMB::VarCorr(onestage)$cond
  tau <- attr(components$dataset_id, "stddev")[[1]]
  tau_unit <- attr(components$unit, "stddev")[[1]]
  gap <- abs(estimate - twostage$estimate) * (4 - 1)
  do.call(
    result,
    c(
      list(
        status = "fitted",
        onestage_estimate = estimate,
        onestage_se = unname(table["x_c", "Std. Error"]),
        onestage_tau = tau,
        onestage_tau_unit = tau_unit,
        contrast_gap = gap,
        exceeds_trigger = gap > settings$onestage_trigger
      ),
      counts,
      two
    )
  )
}
