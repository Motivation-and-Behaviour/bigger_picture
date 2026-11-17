# Curves and the threshold duration (ANALYSIS_PLAN.md, Step 11): known-answer
# tests on simulated units whose curve is set by construction.

skip_unless_engine_needed()
skip_if_not_installed("fixest")
skip_if_not_installed("metafor")
skip_if_not_installed("clubSandwich")

# One unit's rows: screen use in hours and an outcome that follows `truth`.
curve_rows <- function(n, truth, seed, hours = NULL, noise = 1) {
  set.seed(seed)
  x <- if (is.null(hours)) {
    pmin(stats::rgamma(n, shape = 2, scale = 1.2), 10)
  } else {
    sample(hours, n, replace = TRUE)
  }
  data.frame(
    x = x,
    y = truth(x) + noise * stats::rnorm(n),
    age = stats::runif(n, 8, 12),
    sex = sample(c("Female", "Male"), n, replace = TRUE),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_,
    stringsAsFactors = FALSE
  )
}
# The units of `k` datasets, two each, fitted.
curve_fits <- function(k, truth, n = 3000, seed = 1, noise = 1) {
  fits <- lapply(seq_len(2 * k), function(i) {
    rows <- curve_rows(n, truth, seed = 100 * seed + i, noise = noise)
    fit <- fit_curve_unit(rows)
    fit$dataset_id <- sprintf("d%02d", (i + 1) %/% 2)
    fit$unit <- paste0("u", i)
    fit
  })
  dplyr::bind_rows(fits)
}
flat <- function(x) 0 * x
# No association up to two hours, then 0.15 SD lower per hour.
kinked <- function(x) -0.15 * pmax(x - 2, 0)
at_hour <- function(curve, hour) curve[curve$hours == hour, ]

test_that("the curve terms are the same in every unit", {
  settings <- analysis_settings()
  basis <- curve_basis(c(0, 1, 3, 6, 9))
  expect_equal(dim(basis), c(5, 4))
  expect_equal(colnames(basis), c("s1", "s2", "s3", "s4"))
  # Whatever hours a unit has, an hour gets the same terms.
  expect_equal(curve_basis(c(3, 0.2, 7.5))[1, ], basis[3, ])
  # Beyond the last knot the curve is a straight line.
  beyond <- curve_basis(c(6, 7, 8))
  expect_equal(beyond[2, ] - beyond[1, ], beyond[3, ] - beyond[2, ])
  expect_equal(settings$smallest_effect / 3, settings$smallest_effect_per_hour)
})

test_that("a unit's curve is fitted, or says why not", {
  rows <- curve_rows(3000, kinked, seed = 1)
  fit <- fit_curve_unit(rows)
  expect_equal(fit$status, "fitted")
  expect_equal(names(fit$coef[[1]]), c("s1", "s2", "s3", "s4"))
  expect_equal(dim(fit$vcov[[1]]), c(4, 4))
  expect_equal(fit$covariates, "age (spline) + sex")
  # The unit's own curve, read at four hours against one: about -0.30.
  contrast <- curve_basis(4)[1, ] - curve_basis(1)[1, ]
  expect_equal(sum(contrast * fit$coef[[1]]), -0.30, tolerance = 0.1, scale = 1)
  # The rows near each hour of the grid, and in the thinnest stretch
  # between knots.
  grid <- analysis_settings()$curve_grid
  expect_length(fit$rows_near[[1]], length(grid))
  expect_equal(fit$rows_near[[1]][grid == 2], sum(abs(rows$x - 2) <= 0.5))
  stretches <- table(cut(rows$x, c(-Inf, 1, 2, 4, Inf)))
  expect_equal(fit$rows_least_stretch, min(stretches))

  # Four bands cannot carry four curve terms.
  bands <- fit_curve_unit(curve_rows(3000, kinked, 2, hours = c(0.5, 2, 4, 6)))
  expect_equal(
    bands$status,
    "too few distinct values of screen use for a curve"
  )
  expect_null(bands$coef[[1]])
  small <- fit_curve_unit(curve_rows(20, kinked, seed = 3))
  expect_equal(small$status, "fewer rows than the minimum unit size")
  still <- rows
  still$y <- 1
  expect_equal(fit_curve_unit(still)$status, "no variation in the outcome")
})

test_that("a unit with few children in part of the range is not fitted", {
  # Twenty-nine rows above four hours are one short of the minimum.
  rows <- curve_rows(3000, kinked, seed = 4)
  long <- which(rows$x > 4)
  thin <- rows[-long[-(1:29)], ]
  fit <- fit_curve_unit(thin)
  expect_equal(fit$rows_least_stretch, 29)
  expect_equal(
    fit$status,
    "too few rows in a stretch between knots for a curve"
  )
  enough <- rows[-long[-(1:30)], ]
  expect_equal(fit_curve_unit(enough)$status, "fitted")
  # The rule can be turned off, as the simulation does to compare.
  off <- analysis_settings()
  off$curve_interval_rows <- 0L
  expect_equal(fit_curve_unit(thin, off)$status, "fitted")
})

test_that("a unit's curve uses the clustered errors, and needs enough clusters", {
  rows <- curve_rows(3000, kinked, seed = 5)
  set.seed(6)
  school <- sample(40, nrow(rows), replace = TRUE)
  rows$y <- rows$y + stats::rnorm(40)[school]
  plain <- fit_curve_unit(rows)
  rows$cluster <- as.character(school)
  clustered <- fit_curve_unit(rows)
  expect_equal(clustered$se_type, "clustered on sampling unit")
  expect_equal(clustered$n_clusters, 40)
  expect_false(clustered$few_clusters)
  expect_equal(clustered$coef[[1]], plain$coef[[1]])
  expect_false(isTRUE(all.equal(clustered$vcov[[1]], plain$vcov[[1]])))
  # Eight clusters are flagged; four cannot carry four terms.
  rows$cluster <- as.character(school %% 8)
  expect_true(fit_curve_unit(rows)$few_clusters)
  rows$cluster <- as.character(school %% 4)
  few <- fit_curve_unit(rows)
  expect_equal(few$status, "too few clusters for a curve")
  expect_null(few$vcov[[1]])
})

test_that("a pooled curve recovers the curve built into the units", {
  fits <- curve_fits(10, kinked)
  pooled <- pool_curve(fits, higher_is = "better")
  curve <- pooled$curve
  truth <- kinked(curve$hours) - kinked(1)
  expect_equal(pooled$summary$status, "pooled")
  expect_equal(pooled$summary$k_datasets, 10)
  expect_equal(pooled$summary$k_units, 20)
  expect_equal(pooled$summary$k_units_not_fitted, 0)
  expect_equal(pooled$summary$random_levels, "dataset, unit")
  expect_equal(at_hour(curve, 1)$estimate, 0)
  # Within the hours the curve bends freely in, it is within 0.03 SD of
  # the truth; a smooth curve rounds the kink a little.
  inside <- curve$hours >= 0.5 & curve$hours <= 6 & curve$hours != 1
  expect_lt(max(abs(curve$estimate - truth)[inside]), 0.03)
  expect_true(all((curve$ci_low <= truth & truth <= curve$ci_high)[
    curve$hours %in% c(3, 4, 5)
  ]))
  # The truth is 0.075 below the reference at 2.5 hours and 0.15 below at
  # 3: the threshold is 3, where the whole interval first clears 0.10.
  expect_lt(at_hour(curve, 2.5)$ci_high, 0)
  expect_gt(at_hour(curve, 2.5)$ci_high, -0.10)
  expect_lt(at_hour(curve, 3)$ci_high, -0.10)
  expect_equal(pooled$summary$threshold_hours, 3)
  expect_equal(pooled$summary$threshold_direction, "worse outcome")
  expect_true(pooled$summary$threshold_holds)
  # The central reading asks only that the estimate be beyond 0.10 with an
  # interval clear of no difference: here the same hour, as the estimate
  # at 2.5 hours is short of 0.10.
  expect_gt(at_hour(curve, 2.5)$estimate, -0.10)
  expect_equal(pooled$summary$threshold_central_hours, 3)
  expect_equal(pooled$summary$threshold_central_direction, "worse outcome")
  expect_true(pooled$summary$threshold_central_holds)
  # The same curve on a row where higher is worse is a better outcome, and
  # with no direction given it is a lower one.
  expect_equal(
    pool_curve(fits, higher_is = "worse")$summary$threshold_direction,
    "better outcome"
  )
  expect_equal(pool_curve(fits)$summary$threshold_direction, "lower outcome")
})

test_that("the threshold is two-sided and needs the whole interval beyond", {
  rising <- function(x) 0.15 * pmax(x - 2, 0)
  up <- pool_curve(curve_fits(10, rising, seed = 4), higher_is = "better")
  expect_equal(up$summary$threshold_hours, 3)
  expect_equal(up$summary$threshold_direction, "better outcome")

  # With far noisier units the estimate at three hours is still about 0.15
  # below, but its interval reaches inside 0.10, so that is not a threshold.
  noisy <- pool_curve(curve_fits(10, kinked, n = 400, seed = 5, noise = 3))
  three <- at_hour(noisy$curve, 3)
  expect_lt(three$estimate, -0.05)
  expect_gt(three$ci_high, -0.10)
  expect_true(
    is.na(noisy$summary$threshold_hours) || noisy$summary$threshold_hours > 3
  )
  # The central reading comes no later than the conservative one, and is
  # the first hour with an estimate beyond 0.10 and an interval clear of
  # zero.
  curve <- noisy$curve
  meets <- curve$hours > 1 &
    curve$interpretable &
    abs(curve$estimate) >= 0.10 &
    (curve$ci_low > 0 | curve$ci_high < 0)
  expect_true(any(meets))
  expect_equal(
    noisy$summary$threshold_central_hours,
    min(curve$hours[meets])
  )
  expect_true(
    is.na(noisy$summary$threshold_hours) ||
      noisy$summary$threshold_central_hours <= noisy$summary$threshold_hours
  )
})

test_that("a flat curve has no threshold, and a dip is said not to hold", {
  pooled <- pool_curve(curve_fits(10, flat, seed = 2))
  expect_true(is.na(pooled$summary$threshold_hours))
  expect_true(is.na(pooled$summary$threshold_direction))
  expect_true(is.na(pooled$summary$threshold_holds))
  expect_true(is.na(pooled$summary$threshold_central_hours))
  inside <- pooled$curve$hours >= 0.5 & pooled$curve$hours <= 6
  expect_lt(max(abs(pooled$curve$estimate[inside])), 0.05)

  # Half an SD lower around three hours, and back by six.
  dip <- function(x) -0.5 * exp(-((x - 3) / 0.9)^2)
  back <- pool_curve(curve_fits(10, dip, seed = 3))
  expect_false(is.na(back$summary$threshold_hours))
  expect_lt(back$summary$threshold_hours, 3.5)
  expect_false(back$summary$threshold_holds)
})

test_that("a curve is read only at hours with data near them, inside the knots", {
  fits <- curve_fits(8, kinked, seed = 6)
  pooled <- pool_curve(fits)
  curve <- pooled$curve
  # Beyond the last knot the curve is a straight line continued: not read.
  expect_false(any(curve$interpretable[curve$hours > 6]))
  expect_true(all(curve$interpretable[curve$hours >= 0.5 & curve$hours <= 5]))
  expect_equal(
    pooled$summary$interpretable_to,
    max(
      curve$hours[curve$k_datasets_support >= 3 & curve$hours <= 6]
    )
  )
  # No child reports no screen use at all, so half an hour is where the
  # data start; the count at an hour is of datasets with rows near it.
  expect_equal(at_hour(curve, 3)$k_datasets_support, 8)
  rows <- curve_rows(3000, kinked, seed = 601)
  expect_equal(
    fits$rows_near[[1]][curve$hours == 3],
    sum(abs(rows$x - 3) <= 0.5)
  )

  # Take away the rows near five hours in six of the eight datasets.
  thinned <- fits
  five <- which(curve$hours == 5)
  for (i in which(thinned$dataset_id %in% sprintf("d%02d", 1:6))) {
    thinned$rows_near[[i]][five] <- 5
  }
  expect_equal(at_hour(pool_curve(thinned)$curve, 5)$k_datasets_support, 2)
  expect_false(at_hour(pool_curve(thinned)$curve, 5)$interpretable)

  # Two datasets are too few for a curve.
  two <- pool_curve(fits[1:4, ])
  expect_equal(two$summary$status, "insufficient data")
  expect_true(all(is.na(two$curve$estimate)))
  # Units that could not be fitted do not count, and are counted as such.
  fits$status[1:12] <- "curve not estimable in the unit"
  left <- pool_curve(fits)
  expect_equal(left$summary$k_datasets, 2)
  expect_equal(left$summary$k_units_not_fitted, 12)
})

test_that("the reference must be supported, and a threshold needs studies", {
  fits <- curve_fits(8, kinked, seed = 7)
  grid <- analysis_settings()$curve_grid
  # Six datasets have too few rows near one hour.
  thin <- fits
  for (i in which(thin$dataset_id %in% sprintf("d%02d", 1:6))) {
    thin$rows_near[[i]][grid == 1] <- 5
  }
  unsupported <- pool_curve(thin)
  expect_equal(
    unsupported$summary$status,
    "too few datasets support the reference"
  )
  expect_true(is.na(unsupported$summary$threshold_hours))
  expect_true(all(is.na(unsupported$curve$estimate)))

  # Three datasets pool, but with too few degrees of freedom for a
  # threshold: the curve is given and no threshold is.
  three <- pool_curve(curve_fits(3, kinked, seed = 8))
  expect_true(all(three$curve$df[three$curve$hours > 1] < 4))
  expect_equal(three$summary$status, "too few studies to say")
  expect_true(is.na(three$summary$threshold_hours))
  expect_false(is.na(at_hour(three$curve, 4)$estimate))
})

test_that("the curve's settings hold everything its functions read", {
  files <- file.path(
    repo_root,
    "R/analysis",
    c("fit_stage1_curves.R", "pool_curve.R")
  )
  code <- unlist(lapply(files, readLines))
  read <- unique(unlist(regmatches(
    code,
    gregexpr("(?<=settings\\$)[a-z_0-9]+", code, perl = TRUE)
  )))
  expect_true(all(read %in% bp_curve_settings()))
  # `fit_curve_unit()` also prepares rows with `stage1_design()`.
  expect_true(all(
    c("sex_known_min", "covariate_known_min") %in% bp_curve_settings()
  ))
  expect_true(all(bp_curve_settings() %in% names(analysis_settings())))
  expect_equal(names(bp_curve_knot_sets()), c("1, 2, 4", "1, 3"))
  same <- analysis_settings()
  same$curve_knots <- c(1, 3)
  expect_equal(names(bp_curve_knot_sets(same)), "1, 3")
})

test_that("every cell gets its pooled curve, with its row's direction", {
  fits <- curve_fits(6, kinked, n = 2000, seed = 3)
  fits$exposure <- "st_watch"
  fits$outcome <- "a"
  again <- fits
  again$outcome <- "b"
  registry <- list(
    rows = tibble::tibble(
      variable_name = c("a", "b"),
      direction = c("higher_better", "higher_worse")
    )
  )
  both <- dplyr::bind_rows(fits, again)
  pooled <- pool_curve_cells(both, outcome_registry = registry)
  expect_equal(pooled$summaries$outcome, c("a", "b"))
  expect_equal(nrow(pooled$curves), 2 * length(analysis_settings()$curve_grid))
  expect_equal(
    pooled$curves$estimate[pooled$curves$outcome == "a"],
    pooled$curves$estimate[pooled$curves$outcome == "b"]
  )
  expect_equal(
    pooled$summaries$threshold_direction,
    c("worse outcome", "better outcome")
  )
  expect_equal(
    unique(pool_curve_cells(both)$summaries$threshold_direction),
    "lower outcome"
  )
  expect_equal(nrow(pool_curve_cells(NULL)$summaries), 0)
})

test_that("the simulation rebuilds outcomes on the real hours and scores them", {
  set.seed(5)
  rows <- dplyr::bind_rows(lapply(1:12, function(i) {
    n <- 800
    banded <- i > 6
    tibble::tibble(
      dataset_id = sprintf("d%d", (i + 1) %/% 2),
      unit = paste0("u", i),
      x = if (banded) {
        sample(c(0.25, 0.75, 1.5, 2.5, 3.5, 5), n, replace = TRUE)
      } else {
        pmin(stats::rgamma(n, shape = 2, scale = 1.2), 10)
      },
      y = 99,
      age = stats::runif(n, 8, 12),
      sex = sample(c("Female", "Male"), n, replace = TRUE)
    )
  }))
  units <- tibble::tibble(
    unit = paste0("u", 1:12),
    measure_type = rep(c("Continuous", "Categorical"), each = 6)
  )
  settings <- analysis_settings()
  # Four of the built-in curves and one replicate show that the machinery
  # works; the pipeline's own run uses all of them, 200 times.
  all_truths <- curve_truths()
  kept <- c("flat", "bends_at_2", "levels_off_short", "levels_off_past")
  rlang::local_bindings(
    curve_truths = function() all_truths[kept],
    .env = environment(simulate_curve_recovery)
  )
  sim <- simulate_curve_recovery(rows, units, reps = 1L, max_rows = 600L)
  expect_equal(sim$base$units, 12)
  expect_equal(sim$base$units_banded, 6)
  expect_equal(sim$base$rows, 12 * 600)
  # Every truth, with and without band error; two knot placements, two
  # rules on units and two numbers of datasets.
  choices <- c("truth", "band_error", "knots", "units", "datasets")
  scenarios <- dplyr::distinct(sim$thresholds[choices])
  expect_true("threshold_central_hours" %in% names(sim$thresholds))
  expect_equal(nrow(scenarios), length(curve_truths()) * 2 * 2 * 2 * 2)
  expect_setequal(scenarios$knots, c("1, 3", "1, 2, 4"))
  expect_equal(nrow(sim$thresholds), nrow(scenarios))
  expect_equal(
    nrow(sim$replicates),
    nrow(sim$thresholds) * length(settings$curve_grid)
  )
  # Five datasets are pooled where five are asked for.
  five <- sim$thresholds[
    sim$thresholds$datasets == "five" & sim$thresholds$units == "any",
  ]
  expect_true(all(five$k_datasets == 5))
  # The outcome in the rows is never used: the estimates are not near 99.
  expect_lt(max(abs(sim$replicates$estimate), na.rm = TRUE), 5)
  # The truth is the mean of the datasets' curves against the reference,
  # and its threshold where that passes the smallest effect of interest.
  # Each dataset's curve is stretched along the hours, so a bend at two
  # hours is a little softer on average than the curve as written, which
  # gives -0.2 at four hours and a threshold at three.
  bends <- sim$truth[sim$truth$truth == "bends_at_2", ]
  expect_equal(at_hour(bends, 4)$true_value, -0.2, tolerance = 0.01, scale = 1)
  expect_lt(at_hour(bends, 4)$true_value, -0.2)
  expect_equal(unique(bends$true_threshold), 2.97, tolerance = 0.02, scale = 1)
  flat <- sim$truth[sim$truth$truth == "flat", ]
  expect_equal(flat$true_value, rep(0, nrow(flat)))
  none <- c("flat", "levels_off_short")
  expect_true(all(is.na(sim$truth$true_threshold[sim$truth$truth %in% none])))
  past <- unique(sim$truth$true_threshold[sim$truth$truth == "levels_off_past"])
  expect_equal(past, 2.61, tolerance = 0.02, scale = 1)
  # The same seed gives the same replicates; no seed is an error.
  # A scenario's seed follows its place in the list, so the first one is
  # the same in a run of one curve with one knot placement.
  rlang::local_bindings(
    curve_truths = function() all_truths["flat"],
    .env = environment(simulate_curve_recovery)
  )
  again <- simulate_curve_recovery(
    rows,
    units,
    reps = 1L,
    knot_sets = bp_curve_knot_sets(settings)[1],
    max_rows = 600L
  )
  first <- function(run) {
    replicates <- run$replicates
    replicates$estimate[
      replicates$truth == "flat" &
        replicates$band_error == "none" &
        replicates$knots == again$replicates$knots[1]
    ]
  }
  expect_gt(length(first(again)), 0)
  expect_equal(first(again), first(sim))
  seedless <- settings[setdiff(names(settings), "seed")]
  expect_error(
    simulate_curve_recovery(rows, units, reps = 1L, settings = seedless),
    "needs a `seed`"
  )
})

test_that("the simulation's summary counts failures, early and false thresholds", {
  truth <- tibble::tibble(
    truth = rep(c("flat", "bends"), each = 2),
    hours = rep(c(1, 4), 2),
    true_value = c(0, 0, 0, -0.2),
    true_threshold = rep(c(NA, 3), each = 2)
  )
  one <- function(
    name,
    rep,
    threshold,
    status = "pooled",
    direction = "lower outcome"
  ) {
    tibble::tibble(
      truth = name,
      band_error = "none",
      knots = "1, 2, 4",
      units = "any",
      datasets = "all",
      rep = rep,
      k_datasets = 14,
      k_units = 85,
      threshold_hours = threshold,
      threshold_direction = if (is.na(threshold)) NA_character_ else direction,
      threshold_holds = NA,
      threshold_central_hours = if (is.na(threshold)) 2 else threshold - 0.5,
      threshold_central_direction = direction,
      status = status
    )
  }
  thresholds <- dplyr::bind_rows(
    # Flat: one false threshold in four, and one failed pool.
    one("flat", 1, NA),
    one("flat", 2, 4),
    one("flat", 3, NA),
    one("flat", 4, NA, status = "model failed: no convergence"),
    # Bends at a true threshold of 3: on time, late, early, wrong way.
    one("bends", 1, 3),
    one("bends", 2, 4),
    one("bends", 3, 2.5),
    one("bends", 4, 3, direction = "higher outcome")
  )
  replicates <- tibble::tibble(
    truth = "bends",
    band_error = "none",
    knots = "1, 2, 4",
    units = "any",
    datasets = "all",
    rep = 1:4,
    hours = 4,
    estimate = c(-0.2, -0.3, -0.1, -0.2),
    ci_low = c(-0.3, -0.4, -0.15, NA),
    ci_high = c(-0.1, -0.25, -0.05, NA),
    interpretable = c(TRUE, TRUE, TRUE, FALSE),
    true_value = -0.2
  )
  out <- summarise_curve_recovery(list(
    replicates = replicates,
    thresholds = thresholds,
    truth = truth
  ))
  expect_setequal(out$threshold$rule, c("conservative", "central"))
  rule <- function(name) out$threshold[out$threshold$rule == name, ]
  # The central reading here is half an hour before the conservative one,
  # or at two hours where that found none: always found, false for the flat
  # curve, and early for the bend except where the conservative was late.
  central <- rule("central")
  expect_equal(central$found, c(1, 1))
  expect_equal(central$false_or_early[central$truth == "flat"], 1)
  expect_equal(central$false_or_early[central$truth == "bends"], 0.75)
  out$threshold <- rule("conservative")
  flat_row <- out$threshold[out$threshold$truth == "flat", ]
  expect_equal(flat_row$failed, 0.25)
  expect_equal(flat_row$found, 0.25)
  expect_equal(flat_row$false_or_early, 0.25)
  expect_equal(flat_row$wrong_direction, 0)
  bends_row <- out$threshold[out$threshold$truth == "bends", ]
  expect_equal(bends_row$found, 1)
  expect_equal(bends_row$median, 3)
  expect_equal(bends_row$false_or_early, 0.25)
  expect_equal(bends_row$wrong_direction, 0.25)
  # The curve's error and coverage are over the replicates that pooled:
  # the fourth has no interval.
  expect_equal(out$curve$mean_error, mean(c(0, -0.1, 0.1)))
  expect_equal(out$curve$coverage, 1 / 3)
  expect_equal(out$curve$interpretable, 1)
})
