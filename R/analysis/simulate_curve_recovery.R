#' The curves the simulation builds into the data
#'
#' Each is a function of hours of screen use a day, in SD of the outcome.
#' They cover what the threshold method has to tell apart: no association;
#' a straight line; a line that bends, at a knot and away from one; curves
#' that level off just short of the smallest effect of interest and just
#' past it; a dip that comes back; and a rise then a fall.
#'
#' @return A named list of functions.
curve_truths <- function() {
  list(
    flat = function(hours) 0 * hours,
    linear = function(hours) -0.05 * hours,
    bends_at_2 = function(hours) -0.10 * pmax(hours - 2, 0),
    bends_at_3 = function(hours) -0.10 * pmax(hours - 3, 0),
    # Against one hour these level off at 0.08 and 0.15 SD.
    levels_off_short = function(hours) -0.16 * (1 - exp(-hours / 1.5)),
    levels_off_past = function(hours) -0.30 * (1 - exp(-hours / 1.5)),
    dip = function(hours) -0.20 * exp(-(hours - 3)^2),
    rise_then_fall = function(hours) {
      capped <- pmin(hours, 6)
      0.08 * capped - 0.02 * capped^2
    }
  )
}

#' Does the threshold method recover a curve built into real screen use?
#'
#' A plasmode simulation (ANALYSIS_PLAN.md, Step 7 task 8). It keeps the
#' real units of one cell, with their real hours of screen use, ages, sexes
#' and sizes, and replaces the outcome with one that follows a known curve
#' plus noise. Datasets differ as real ones do: each has the curve a little
#' stronger or weaker, stretched or squeezed along the hours so its shape
#' sits elsewhere, and tilted by a slope of its own. The curve engine
#' (`fit_curve_unit()`, `pool_curve()`) is then run as it is on real data,
#' and what it reports is compared with the curve that was built in. Real
#' hours matter because the method's difficulties are in them: banded
#' answers and units where few children report long hours.
#'
#' Four things are varied:
#'
#' - `band_error`: with `within bands`, a child's true hours differ from
#'   the recorded hours in units with a banded item (by a random share of
#'   the recorded value, and upwards from the unit's top value), the
#'   outcome follows the true hours, and the analysis sees the recorded
#'   ones. With `none`, the recorded hours are the truth. This is the mild
#'   kind of error: it does not model answers that misreport the true
#'   hours, or a band value that is not the band's mean.
#' - `knots`: where the curve's interior knots sit, in hours.
#' - `units`: `every stretch` keeps a unit only with the minimum number of
#'   rows in every stretch between knots (`curve_interval_rows`); `any`
#'   keeps every unit a curve can be fitted in.
#' - `datasets`: `all` of the cell's datasets, or `five` drawn at random
#'   from those with a unit to pool under the rule on units,
#'   since most cells have few.
#'
#' Not simulated: weights and clustered errors, outcomes that are not
#' normal, confounding by age that is not smooth, and other exposures.
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
#' @return A list: `replicates`, one row per scenario, replicate, choice and
#'   hour, with the estimate, its interval and the true value;
#'   `thresholds`, one row per scenario, replicate and choice, with the
#'   threshold found and the pool's status; `truth`, the true curve and
#'   threshold per scenario; and `base`, the units used.
simulate_curve_recovery <- function(
  rows,
  units,
  reps = 200L,
  settings = default_analysis_settings(),
  knot_sets = bp_curve_knot_sets(settings),
  max_rows = 10000L,
  cores = 1L
) {
  if (is.null(settings$seed)) {
    stop("simulate_curve_recovery(): `settings` needs a `seed`.", call. = FALSE)
  }
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
  # How much a dataset's curve is stretched along the hours (SD of the log).
  stretch_sd <- 0.15
  scenarios <- expand.grid(
    truth = names(truths),
    band_error = c("none", "within bands"),
    stringsAsFactors = FALSE
  )
  # The truth a pooled curve is scored against is the mean of the datasets'
  # curves. Each dataset's curve is stretched along the hours, so for a
  # curve that bends this is not the curve as written.
  stretches <- exp(stats::qnorm(stats::ppoints(401)) * stretch_sd)
  mean_curve <- function(name, hours) {
    vapply(
      hours,
      function(hour) mean(truths[[name]](hour * stretches)),
      numeric(1)
    )
  }
  truth <- dplyr::bind_rows(lapply(names(truths), function(name) {
    value <- mean_curve(name, grid) - mean_curve(name, reference)
    fine <- seq(reference, settings$curve_boundary[2], by = 0.01)
    beyond <- abs(mean_curve(name, fine) - mean_curve(name, reference)) >=
      settings$smallest_effect
    tibble::tibble(
      truth = name,
      hours = grid,
      true_value = value,
      true_threshold = if (any(beyond)) min(fine[beyond]) else NA_real_
    )
  }))
  # Units are fitted without the rule on rows per stretch, which is then
  # applied, or not, when pooling.
  fit_settings <- settings
  fit_settings$curve_interval_rows <- 0L

  one <- function(job) {
    scenario <- scenarios[job$scenario, ]
    set.seed(settings$seed + 100000L * job$scenario + job$rep)
    curve <- truths[[scenario$truth]]
    k <- length(datasets)
    strength <- stats::setNames(1 + stats::rnorm(k, 0, 0.3), datasets)
    stretch <- stats::setNames(exp(stats::rnorm(k, 0, stretch_sd)), datasets)
    tilt <- stats::setNames(stats::rnorm(k, 0, 0.03), datasets)
    # The order datasets are taken in where only five are used.
    drawn <- sample(datasets)
    simulated <- lapply(names(by_unit), function(name) {
      data <- by_unit[[name]]
      id <- data$dataset_id[1]
      true_hours <- data$x
      if (scenario$band_error == "within bands" && data$banded[1]) {
        true_hours <- data$x * exp(stats::rnorm(nrow(data), 0, 0.25))
        true_hours[data$top] <- data$x[data$top] +
          stats::rexp(sum(data$top), rate = 1)
      }
      data$y <- strength[[id]] *
        curve(true_hours * stretch[[id]]) +
        tilt[[id]] * true_hours +
        stats::rnorm(nrow(data))
      data
    })
    names(simulated) <- names(by_unit)
    out <- lapply(names(knot_sets), function(knots) {
      with_knots <- fit_settings
      with_knots$curve_knots <- knot_sets[[knots]]
      fits <- dplyr::bind_rows(lapply(names(simulated), function(name) {
        fit <- fit_curve_unit(simulated[[name]], with_knots)
        fit$dataset_id <- simulated[[name]]$dataset_id[1]
        fit$unit <- name
        fit
      }))
      full <- fits$rows_least_stretch >= settings$curve_interval_rows
      choices <- expand.grid(
        units = c("every stretch", "any"),
        datasets = c("all", "five"),
        stringsAsFactors = FALSE
      )
      lapply(seq_len(nrow(choices)), function(i) {
        choice <- choices[i, ]
        # Five of the datasets that have a unit to pool under this rule,
        # so that five are pooled and not the few of five that pass it.
        kept <- choice$units == "any" | full
        poolable <- fits$dataset_id[kept & fits$status %in% "fitted"]
        five <- utils::head(drawn[drawn %in% poolable], 5L)
        used <- fits[
          kept & (choice$datasets == "all" | fits$dataset_id %in% five),
        ]
        pooled <- pool_curve(used, with_knots)
        label <- function(table) {
          table$truth <- scenario$truth
          table$band_error <- scenario$band_error
          table$knots <- knots
          table$units <- choice$units
          table$datasets <- choice$datasets
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
            "threshold_holds",
            "threshold_central_hours",
            "threshold_central_direction",
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
  # A process that was killed returns nothing, and one that failed an error.
  broken <- !vapply(
    done,
    function(job) is.list(job) && !is.null(job$threshold),
    logical(1)
  )
  if (any(broken)) {
    stop(
      "simulate_curve_recovery(): ",
      sum(broken),
      " of ",
      length(done),
      " replicates did not return; the first said: ",
      paste(as.character(done[[which(broken)[1]]]), collapse = " "),
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
#' @return A list of two tibbles. `curve`, one row per scenario, choice of
#'   knots, units and datasets, and hour: the mean error of the pooled
#'   curve, the share of intervals that cover the truth, and the share of
#'   replicates in which the hour was interpretable, over the replicates
#'   that pooled. `threshold`, one row per scenario, choice and `rule`
#'   (`conservative` or `central`, as in `pool_curve()`): the true
#'   threshold; the share of replicates whose pool failed; the share that
#'   found a threshold, with the median and quartiles of those found;
#'   `false_or_early`, the share that found one where the truth has none,
#'   or more than a quarter of an hour before the true one; and the share
#'   that found one in the wrong direction.
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
      .data$datasets,
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
  both <- simulation$thresholds
  central <- both
  central$threshold_hours <- both$threshold_central_hours
  central$threshold_direction <- both$threshold_central_direction
  both$rule <- "conservative"
  central$rule <- "central"
  thresholds <- dplyr::left_join(
    dplyr::bind_rows(both, central),
    true_threshold,
    by = "truth"
  )
  found <- !is.na(thresholds$threshold_hours)
  thresholds$false_or_early <- found &
    (is.na(thresholds$true_threshold) |
      thresholds$threshold_hours < thresholds$true_threshold - 0.25)
  # Every built-in curve with a threshold reaches it by falling.
  thresholds$wrong <- found &
    !is.na(thresholds$true_threshold) &
    thresholds$threshold_direction %in% "higher outcome"
  threshold <- dplyr::summarise(
    dplyr::group_by(
      thresholds,
      .data$truth,
      .data$band_error,
      .data$knots,
      .data$units,
      .data$datasets,
      .data$rule
    ),
    true_threshold = .data$true_threshold[1],
    k_datasets = stats::median(.data$k_datasets),
    k_units = stats::median(.data$k_units),
    failed = mean(
      !.data$status %in% c("pooled", "too few studies to say")
    ),
    found = mean(!is.na(.data$threshold_hours)),
    q25 = stats::quantile(.data$threshold_hours, 0.25, na.rm = TRUE),
    median = stats::median(.data$threshold_hours, na.rm = TRUE),
    q75 = stats::quantile(.data$threshold_hours, 0.75, na.rm = TRUE),
    false_or_early = mean(.data$false_or_early),
    wrong_direction = mean(.data$wrong),
    .groups = "drop"
  )
  list(curve = curve, threshold = threshold)
}

# The knot placements the simulation compares: the one in the settings, and
# one with two interior knots.
bp_curve_knot_sets <- function(settings = default_analysis_settings()) {
  sets <- list(settings$curve_knots, c(1, 3))
  names(sets) <- vapply(sets, paste, character(1), collapse = ", ")
  sets[!duplicated(names(sets))]
}
