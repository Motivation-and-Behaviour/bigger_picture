# Curves and the threshold duration (ANALYSIS_PLAN.md, Step 11): known-answer
# tests on simulated units whose curve is set by construction.

skip_if_not_installed("fixest")
skip_if_not_installed("metafor")
skip_if_not_installed("clubSandwich")

# One unit's rows: screen use in hours and an outcome that follows `truth`.
curve_rows <- function(n, truth, seed, hours = NULL, shift = 0) {
  set.seed(seed)
  x <- if (is.null(hours)) {
    pmin(stats::rgamma(n, shape = 2, scale = 1.2), 10)
  } else {
    sample(hours, n, replace = TRUE)
  }
  data.frame(
    x = x,
    y = truth(x) + shift + stats::rnorm(n),
    age = stats::runif(n, 8, 12),
    sex = sample(c("Female", "Male"), n, replace = TRUE),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_,
    stringsAsFactors = FALSE
  )
}
# The units of `k` datasets, two each, fitted.
curve_fits <- function(k, truth, n = 3000, seed = 1, hours = NULL) {
  fits <- lapply(seq_len(2 * k), function(i) {
    rows <- curve_rows(n, truth, seed = 100 * seed + i, hours = hours)
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
  fit <- fit_curve_unit(curve_rows(3000, kinked, seed = 1))
  expect_equal(fit$status, "fitted")
  expect_equal(names(fit$coef[[1]]), c("s1", "s2", "s3", "s4"))
  expect_equal(dim(fit$vcov[[1]]), c(4, 4))
  expect_equal(fit$covariates, "age (spline) + sex")
  # The unit's own curve, read at four hours against one: about -0.30.
  contrast <- curve_basis(4)[1, ] - curve_basis(1)[1, ]
  expect_equal(sum(contrast * fit$coef[[1]]), -0.30, tolerance = 0.1, scale = 1)
  grid <- analysis_settings()$curve_grid
  expect_length(fit$rows_at_or_above[[1]], length(grid))
  expect_equal(fit$rows_at_or_above[[1]][1], 3000)
  rows <- curve_rows(3000, kinked, seed = 1)
  expect_equal(fit$rows_at_or_below[[1]][length(grid)], sum(rows$x <= 8))

  # Four bands cannot carry four curve terms.
  bands <- fit_curve_unit(curve_rows(3000, kinked, 2, hours = c(0.5, 2, 4, 6)))
  expect_equal(
    bands$status,
    "too few distinct values of screen use for a curve"
  )
  expect_null(bands$coef[[1]])
  small <- fit_curve_unit(curve_rows(20, kinked, seed = 3))
  expect_equal(small$status, "fewer rows than the minimum unit size")
})

test_that("a pooled curve recovers the curve built into the units", {
  fits <- curve_fits(10, kinked)
  pooled <- pool_curve(fits)
  curve <- pooled$curve
  truth <- kinked(curve$hours) - kinked(1)
  expect_equal(pooled$summary$status, "pooled")
  expect_equal(pooled$summary$k_datasets, 10)
  expect_equal(pooled$summary$k_units, 20)
  expect_equal(curve$estimate[curve$hours == 1], 0)
  # Within the hours children have, the curve is within 0.05 SD of the
  # truth, and its interval covers the truth give or take that much: a
  # smooth curve rounds the kink.
  inside <- curve$hours >= 0.5 & curve$hours <= 6 & curve$hours != 1
  expect_lt(max(abs(curve$estimate - truth)[inside]), 0.05)
  covered <- curve$ci_low - 0.05 <= truth & truth <= curve$ci_high + 0.05
  expect_true(all(covered[inside]))
  # The truth passes 0.10 SD below the reference at 2.67 hours; the first
  # hour on the grid whose whole interval is beyond it comes soon after.
  expect_equal(pooled$summary$threshold_direction, "lower outcome")
  expect_true(pooled$summary$threshold_hours %in% c(3, 3.5))
})

test_that("a flat curve has no threshold", {
  pooled <- pool_curve(curve_fits(10, flat, seed = 2))
  expect_true(is.na(pooled$summary$threshold_hours))
  expect_true(is.na(pooled$summary$threshold_direction))
  inside <- pooled$curve$hours >= 0.5 & pooled$curve$hours <= 6
  expect_lt(max(abs(pooled$curve$estimate[inside])), 0.05)
})

test_that("a curve is read only at hours enough datasets support", {
  # No child has more than four hours, so nothing supports the hours above.
  capped <- function(n, seed) {
    rows <- curve_rows(n, kinked, seed)
    rows$x <- pmin(rows$x, 4)
    rows
  }
  fits <- dplyr::bind_rows(lapply(1:8, function(i) {
    fit <- fit_curve_unit(capped(3000, i))
    fit$dataset_id <- sprintf("d%02d", i)
    fit$unit <- paste0("u", i)
    fit
  }))
  pooled <- pool_curve(fits)
  curve <- pooled$curve
  expect_true(all(curve$interpretable[curve$hours >= 0.5 & curve$hours <= 4]))
  expect_false(any(curve$interpretable[curve$hours > 4]))
  # Nor does anything support zero hours: no child reports none at all.
  expect_false(curve$interpretable[curve$hours == 0])
  expect_equal(pooled$summary$interpretable_from, 0.5)
  expect_equal(curve$k_datasets_support[curve$hours == 5], 0)
  expect_equal(pooled$summary$interpretable_to, 4)
  # A threshold is not read beyond the supported hours.
  expect_true(
    is.na(pooled$summary$threshold_hours) ||
      pooled$summary$threshold_hours <= 4
  )

  # Two datasets are too few for a curve.
  two <- pool_curve(fits[1:2, ])
  expect_equal(two$summary$status, "insufficient data")
  expect_true(all(is.na(two$curve$estimate)))
  # Units that could not be fitted do not count.
  fits$status[1:6] <- "curve not estimable in the unit"
  expect_equal(pool_curve(fits)$summary$k_datasets, 2)
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
})

test_that("every cell gets its pooled curve", {
  fits <- curve_fits(4, kinked, n = 1000, seed = 3)
  fits$exposure <- "st_watch"
  fits$outcome <- "a"
  again <- fits
  again$outcome <- "b"
  pooled <- pool_curve_cells(dplyr::bind_rows(fits, again))
  expect_equal(pooled$summaries$outcome, c("a", "b"))
  expect_equal(nrow(pooled$curves), 2 * length(analysis_settings()$curve_grid))
  expect_equal(
    pooled$curves$estimate[pooled$curves$outcome == "a"],
    pooled$curves$estimate[pooled$curves$outcome == "b"]
  )
  expect_equal(nrow(pool_curve_cells(NULL)$summaries), 0)
})

test_that("the simulation rebuilds outcomes on the real hours and scores them", {
  set.seed(5)
  rows <- dplyr::bind_rows(lapply(1:6, function(i) {
    n <- 600
    banded <- i > 3
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
    unit = paste0("u", 1:6),
    measure_type = rep(c("Continuous", "Categorical"), each = 3)
  )
  settings <- analysis_settings()
  sim <- simulate_curve_recovery(rows, units, reps = 2L, max_rows = 500L)
  expect_equal(sim$base$units, 6)
  expect_equal(sim$base$units_banded, 3)
  expect_equal(sim$base$rows, 6 * 500)
  # Every truth, with and without band error, two knot placements, pooled
  # two ways, twice.
  scenarios <- dplyr::distinct(
    sim$thresholds[c("truth", "band_error", "knots", "units")]
  )
  expect_equal(nrow(scenarios), length(curve_truths()) * 2 * 2 * 2)
  expect_setequal(scenarios$knots, c("1, 3", "1, 2, 4"))
  expect_equal(nrow(sim$thresholds), 2 * nrow(scenarios))
  expect_equal(
    nrow(sim$replicates),
    nrow(sim$thresholds) * length(settings$curve_grid)
  )
  # The outcome in the rows is never used: the estimates are not near 99.
  expect_lt(max(abs(sim$replicates$estimate), na.rm = TRUE), 5)
  # The truth is the built-in curve against the reference, and its
  # threshold where that passes the smallest effect of interest.
  kinked <- sim$truth[sim$truth$truth == "kinked", ]
  expect_equal(kinked$true_value[kinked$hours == 4], -0.2)
  expect_equal(unique(kinked$true_threshold), 3)
  expect_true(all(is.na(sim$truth$true_threshold[sim$truth$truth == "flat"])))
  # The same seed gives the same replicates.
  again <- simulate_curve_recovery(rows, units, reps = 2L, max_rows = 500L)
  expect_equal(again$replicates$estimate, sim$replicates$estimate)

  summary <- summarise_curve_recovery(sim)
  expect_equal(nrow(summary$threshold), nrow(scenarios))
  expect_true(all(summary$curve$coverage >= 0 & summary$curve$coverage <= 1))
  expect_true(all(
    c("mean_error", "coverage", "interpretable") %in% names(summary$curve)
  ))
})
