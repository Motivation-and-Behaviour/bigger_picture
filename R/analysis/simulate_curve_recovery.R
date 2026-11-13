#' The curves the simulation builds into the data
#'
#' Each is a function of hours of screen use a day, in SD of the outcome.
#' They cover what the threshold method has to tell apart: no association,
#' a straight line, a line that bends at two hours, and a rise then a fall.
#'
#' @return A named list of functions.
curve_truths <- function() {
  list(
    flat = function(hours) 0 * hours,
    linear = function(hours) -0.05 * hours,
    kinked = function(hours) -0.10 * pmax(hours - 2, 0),
    rise_then_fall = function(hours) 0.08 * hours - 0.02 * hours^2
  )
}

#' Does the threshold method recover a curve built into real screen use?
#'
#' A plasmode simulation (ANALYSIS_PLAN.md, Step 7 task 8). It keeps the
#' real units of one cell, with their real hours of screen use, ages, sexes
#' and sizes, and replaces the outcome with one that follows a known curve
#' plus noise, each dataset's curve a little stronger or weaker than the
#' average. The curve engine (`fit_curve_unit()`, `pool_curve()`) is then
#' run as it is on real data, and what it reports is compared with the
#' curve that was built in. Real hours matter because the method's
#' difficulties are in them: banded answers, a top band, and units where
#' few children report long hours.
#'
#' Three things are varied:
#'
#' - `band_error`: with `within bands`, a child's true hours differ from
#'   the recorded hours in units with a banded item (by a random share of
#'   the recorded value, and upwards from the top value, which stands for
#'   "this much or more"), the outcome follows the true hours, and the
#'   analysis sees the recorded ones. With `none`, the recorded hours are
#'   the truth.
#' - `units`: `all` pools every unit a curve can be fitted in; `long hours
#'   only` leaves out units with fewer than the minimum number of rows at
#'   four hours or more.
#' - `knots`: where the curve's interior knots sit, in hours.
#'
#' @param rows A tibble of `dataset_id`, `unit`, `x` (hours), `age` and
#'   `sex`, as `extract_cell_rows()` gives.
#' @param units Per-unit descriptions with `unit` and `measure_type`.
#' @param reps Replicates per scenario.
#' @param settings From `analysis_settings()`, with `seed`.
#' @param knot_sets A named list of interior knots to compare; the first is
#'   the one in `settings`.
#' @param max_rows The most rows kept per unit, to bound the run time.
#' @param cores Processes to run replicates in.
#' @return A list: `replicates`, one row per scenario, replicate, way of
#'   choosing units and hour, with the estimate, its interval and the true
#'   value; `thresholds`, one row per scenario, replicate and way, with the
#'   threshold found; `truth`, the true curve and threshold per scenario;
#'   and `base`, the units used.
simulate_curve_recovery <- function(
  rows,
  units,
  reps = 200L,
  settings = analysis_settings(),
  knot_sets = bp_curve_knot_sets(settings),
  max_rows = 4000L,
  cores = 1L
) {
  set.seed(settings$seed)
  rows <- rows[!is.na(rows$x), c("dataset_id", "unit", "x", "age", "sex")]
  keep <- unlist(lapply(split(seq_len(nrow(rows)), rows$unit), function(i) {
    if (length(i) > max_rows) sample(i, max_rows) else i
  }))
  rows <- rows[sort(keep), ]
  rows$banded <- units$measure_type[match(rows$unit, units$unit)] %in%
    "Categorical"
  rows$top <- rows$x >= stats::ave(rows$x, rows$unit, FUN = max)
  rows$weight <- NA_real_
  rows$cluster <- NA_character_
  rows$family <- NA_character_
  by_unit <- split(rows, rows$unit)
  dataset_of <- vapply(by_unit, function(unit) unit$dataset_id[1], "")
  datasets <- unique(dataset_of)

  grid <- settings$curve_grid
  reference <- settings$curve_reference
  truths <- curve_truths()
  scenarios <- expand.grid(
    truth = names(truths),
    band_error = c("none", "within bands"),
    stringsAsFactors = FALSE
  )
  truth <- dplyr::bind_rows(lapply(names(truths), function(name) {
    value <- truths[[name]](grid) - truths[[name]](reference)
    fine <- seq(reference, max(grid), by = 0.01)
    beyond <- abs(truths[[name]](fine) - truths[[name]](reference)) >=
      settings$smallest_effect
    tibble::tibble(
      truth = name,
      hours = grid,
      true_value = value,
      true_threshold = if (any(beyond)) min(fine[beyond]) else NA_real_
    )
  }))

  one <- function(job) {
    scenario <- scenarios[job$scenario, ]
    set.seed(settings$seed + 1000L * job$scenario + job$rep)
    curve <- truths[[scenario$truth]]
    # Each dataset's curve is the average one, a little stronger or weaker.
    strength <- stats::setNames(
      1 + stats::rnorm(length(datasets), 0, 0.3),
      datasets
    )
    simulated <- lapply(names(by_unit), function(name) {
      data <- by_unit[[name]]
      true_hours <- data$x
      if (scenario$band_error == "within bands" && data$banded[1]) {
        true_hours <- data$x * exp(stats::rnorm(nrow(data), 0, 0.25))
        true_hours[data$top] <- data$x[data$top] +
          stats::rexp(sum(data$top), rate = 1)
      }
      data$y <- strength[[data$dataset_id[1]]] *
        curve(true_hours) +
        stats::rnorm(nrow(data))
      data
    })
    names(simulated) <- names(by_unit)
    out <- lapply(names(knot_sets), function(knots) {
      with_knots <- settings
      with_knots$curve_knots <- knot_sets[[knots]]
      fits <- dplyr::bind_rows(lapply(names(simulated), function(name) {
        fit <- fit_curve_unit(simulated[[name]], with_knots)
        fit$dataset_id <- simulated[[name]]$dataset_id[1]
        fit$unit <- name
        fit
      }))
      at_four <- vapply(
        fits$rows_at_or_above,
        function(counts) counts[match(4, grid)],
        numeric(1)
      )
      ways <- list(
        all = fits,
        `long hours only` = fits[at_four >= settings$min_unit_rows, ]
      )
      lapply(names(ways), function(way) {
        pooled <- pool_curve(ways[[way]], with_knots)
        label <- function(table) {
          table$truth <- scenario$truth
          table$band_error <- scenario$band_error
          table$knots <- knots
          table$units <- way
          table$rep <- job$rep
          table
        }
        list(
          curve = label(pooled$curve[c(
            "hours",
            "estimate",
            "ci_low",
            "ci_high",
            "interpretable"
          )]),
          threshold = label(pooled$summary[c(
            "k_datasets",
            "k_units",
            "threshold_hours",
            "threshold_direction",
            "status"
          )])
        )
      })
    })
    out <- unlist(out, recursive = FALSE)
    list(
      curve = dplyr::bind_rows(lapply(out, function(way) way$curve)),
      threshold = dplyr::bind_rows(lapply(out, function(way) way$threshold))
    )
  }
  jobs <- expand.grid(
    scenario = seq_len(nrow(scenarios)),
    rep = seq_len(reps)
  )
  jobs <- split(jobs, seq_len(nrow(jobs)))
  done <- if (cores > 1) {
    parallel::mclapply(jobs, one, mc.cores = cores, mc.set.seed = FALSE)
  } else {
    lapply(jobs, one)
  }
  failed <- vapply(done, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop(
      "simulate_curve_recovery(): ",
      sum(failed),
      " replicate(s) failed; the first said: ",
      as.character(done[[which(failed)[1]]]),
      call. = FALSE
    )
  }
  replicates <- dplyr::bind_rows(lapply(done, function(job) job$curve))
  replicates <- dplyr::left_join(
    replicates,
    truth[c("truth", "hours", "true_value")],
    by = c("truth", "hours")
  )
  list(
    replicates = replicates,
    thresholds = dplyr::bind_rows(lapply(done, function(job) job$threshold)),
    truth = truth,
    base = tibble::tibble(
      datasets = length(datasets),
      units = length(by_unit),
      units_banded = sum(vapply(by_unit, function(u) u$banded[1], TRUE)),
      rows = nrow(rows),
      reps = reps
    )
  )
}

#' Summarise the curve simulation
#'
#' @param simulation Output of `simulate_curve_recovery()`.
#' @return A list of two tibbles, one row per scenario, knot placement and
#'   way of choosing units. `curve`: by hour, the mean error of the pooled curve, the share
#'   of intervals that cover the truth, and the share of replicates in
#'   which the hour was interpretable. `threshold`: the true threshold, the
#'   share of replicates that found one, the median and quartiles of those
#'   found, and the share that found one in the wrong direction.
summarise_curve_recovery <- function(simulation) {
  replicates <- simulation$replicates
  replicates <- replicates[!is.na(replicates$ci_low), ]
  curve <- dplyr::summarise(
    dplyr::group_by(
      replicates,
      .data$truth,
      .data$band_error,
      .data$knots,
      .data$units,
      .data$hours
    ),
    true_value = .data$true_value[1],
    mean_error = mean(.data$estimate - .data$true_value),
    coverage = mean(
      .data$ci_low <= .data$true_value & .data$true_value <= .data$ci_high
    ),
    interpretable = mean(.data$interpretable),
    .groups = "drop"
  )
  true_threshold <- dplyr::distinct(
    simulation$truth[c("truth", "true_threshold")]
  )
  true_direction <- dplyr::summarise(
    dplyr::group_by(simulation$truth, .data$truth),
    falls = .data$true_value[which.max(abs(.data$true_value))] < 0,
    .groups = "drop"
  )
  thresholds <- dplyr::left_join(
    simulation$thresholds,
    true_direction,
    by = "truth"
  )
  found <- !is.na(thresholds$threshold_hours)
  thresholds$wrong <- found &
    (thresholds$threshold_direction == "lower outcome") != thresholds$falls
  threshold <- dplyr::summarise(
    dplyr::group_by(
      thresholds,
      .data$truth,
      .data$band_error,
      .data$knots,
      .data$units
    ),
    k_datasets = stats::median(.data$k_datasets),
    k_units = stats::median(.data$k_units),
    found = mean(!is.na(.data$threshold_hours)),
    q25 = stats::quantile(.data$threshold_hours, 0.25, na.rm = TRUE),
    median = stats::median(.data$threshold_hours, na.rm = TRUE),
    q75 = stats::quantile(.data$threshold_hours, 0.75, na.rm = TRUE),
    wrong_direction = mean(.data$wrong),
    .groups = "drop"
  )
  threshold <- dplyr::left_join(threshold, true_threshold, by = "truth")
  list(
    curve = curve,
    threshold = dplyr::relocate(threshold, "true_threshold", .after = "units")
  )
}

# The knot placements the simulation compares: the one in the settings, and
# one with two interior knots.
bp_curve_knot_sets <- function(settings = analysis_settings()) {
  sets <- list(settings$curve_knots, c(1, 3))
  names(sets) <- vapply(sets, paste, character(1), collapse = ", ")
  sets[!duplicated(names(sets))]
}
