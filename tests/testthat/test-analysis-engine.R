# The model engine (ANALYSIS_PLAN.md, Step 5): one known-answer test per
# function, on the mock dataset, whose effects are known by construction.
# Real data can show that an estimate is implausible, never that it is wrong.

skip_if_not_installed("fixest")
skip_if_not_installed("metafor")
skip_if_not_installed("clubSandwich")

engine_registries <- function() {
  schema <- read_dataschema(file.path(repo_root, bp_dataschema_path()))
  list(
    schema = schema,
    outcomes = read_outcome_registry(
      file.path(repo_root, bp_outcome_tree_path()),
      file.path(repo_root, bp_outcome_rows_path()),
      schema
    ),
    exposures = read_exposure_registry(
      file.path(repo_root, bp_exposures_path()),
      schema
    ),
    derivations = read_derivations(
      file.path(repo_root, bp_derivations_path()),
      schema
    )
  )
}
engine_mock <- make_mock_analysis_data(n_per_wave = 600)
engine_prepare <- function(id, reg, edit = identity) {
  rows <- engine_mock$data[engine_mock$data$dataset_id == id, ]
  rows[] <- lapply(rows, function(x) if (is.factor(x)) as.character(x) else x)
  info <- tibble::tibble(
    dataset_id = id,
    baseline_wave = NA_character_,
    wave_order = NA_character_,
    notes = NA_character_,
    included = TRUE
  )
  prepare_analysis_data(
    edit(rows),
    info,
    reg$outcomes,
    reg$exposures,
    reg$derivations,
    reg$schema,
    read_analysis_exceptions(character(), reg$schema, reg$outcomes)
  )
}
engine_cells <- tibble::tibble(
  exposure = c("st_game", "st_socmedia", "st_watch"),
  outcome = c("mh_dep_self", "beh_alcohol_self", "wb_selfratedhealth_self")
)
engine_stage1 <- function(reg) {
  ids <- unique(as.character(engine_mock$data$dataset_id))
  dplyr::bind_rows(lapply(ids, function(id) {
    fit_stage1(engine_prepare(id, reg), engine_cells, reg$outcomes, reg$schema)
  }))
}

test_that("stage 1 and stage 2 recover the mock's known effects", {
  reg <- engine_registries()
  stage1 <- engine_stage1(reg)
  expect_true(all(stage1$status == "fitted"))
  expect_setequal(
    stage1$effect_measure,
    c("sd_per_hour", "log_odds_per_hour")
  )
  # Panels and repeated cross-sections give a unit per wave, and the
  # two-country dataset a unit per country.
  units <- dplyr::count(stage1[stage1$outcome == "mh_dep_self", ], dataset_id)
  expect_equal(units$n, c(2, 2, 2, 2, 1, 2))

  pooled <- pool_stage2_cells(stage1)
  expect_equal(nrow(pooled), 3)
  cell <- function(outcome) pooled[pooled$outcome == outcome, ]
  # The mock draws each dataset's effect around a mean; the target is the
  # mean of the effects it drew.
  truth <- function(outcome) {
    mean(engine_mock$truth$effect[engine_mock$truth$outcome == outcome])
  }

  # Depression per hour of gaming, in SD units: the outcome's SD is a little
  # over 1, so the standardised slope sits just under the raw one.
  dep <- cell("mh_dep_self")
  expect_equal(dep$status, "pooled")
  expect_equal(dep$k_datasets, 6)
  expect_equal(dep$k_units, 11)
  expect_lt(abs(dep$estimate - truth("mh_dep_self")), 0.02)
  expect_true(
    dep$ci_low < truth("mh_dep_self") &&
      dep$ci_high > 0.9 * truth("mh_dep_self")
  )
  expect_true(dep$pi_low <= dep$ci_low && dep$pi_high >= dep$ci_high)

  # Alcohol use per hour of social media, in log odds, from four datasets:
  # recovered, but four datasets are too few to say much.
  alcohol <- cell("beh_alcohol_self")
  expect_equal(alcohol$k_datasets, 4)
  expect_equal(alcohol$effect_measure, "log_odds_per_hour")
  expect_lt(abs(alcohol$estimate - truth("beh_alcohol_self")), 0.04)
  expect_equal(alcohol$status, "too few studies to say")

  # Self-rated health has five levels, so it is treated as numeric: the
  # known effect is on the log-odds scale, so only its direction is checked.
  health <- cell("wb_selfratedhealth_self")
  expect_equal(health$effect_measure, "sd_per_hour")
  expect_gt(health$ci_low, 0)
})

test_that("a unit's slope matches a design-based fit of the same model", {
  skip_if_not_installed("survey")
  reg <- engine_registries()
  prepared <- engine_prepare("9995", reg)
  fit <- fit_stage1(prepared, engine_cells[1, ], reg$outcomes, reg$schema)
  expect_equal(fit$se_type, "clustered on sampling unit")
  expect_true(fit$weighted)
  expect_equal(fit$n_clusters, 20)

  data <- data.frame(
    y = prepared$outcomes_z$mh_dep_self,
    x = prepared$exposures$st_game,
    age = prepared$base$age_years,
    sex = prepared$base$sex,
    w = prepared$base$survey_weight_norm,
    cluster = prepared$base$survey_cluster
  )
  data$y <- (data$y - mean(data$y)) / stats::sd(data$y)
  design <- survey::svydesign(ids = ~cluster, weights = ~w, data = data)
  reference <- survey::svyglm(
    y ~ x + splines::ns(age, df = 2) + sex,
    design = design
  )
  expect_equal(fit$estimate, unname(stats::coef(reference)["x"]))
  expect_equal(
    fit$se,
    unname(sqrt(diag(stats::vcov(reference)))["x"]),
    tolerance = 0.05
  )
})

test_that("a unit that cannot be fitted says why", {
  reg <- engine_registries()
  # 40 rows have a score, but only 25 of them have an age.
  few <- engine_prepare("9995", reg, function(rows) {
    rows <- rows[1:40, ]
    rows$age_years[1:15] <- NA
    rows$age_years[16:40] <- seq(9, 15, length.out = 25)
    rows
  })
  expect_null(fit_stage1(few, engine_cells[1, ], reg$outcomes, reg$schema))
  small <- fit_stage1_unit(
    data.frame(
      x = stats::runif(40),
      y = stats::rnorm(40),
      age = c(rep(NA, 15), seq(9, 15, length.out = 25)),
      sex = "Female",
      weight = NA_real_,
      cluster = NA_character_,
      family = NA_character_
    ),
    "linear"
  )
  expect_equal(small$status, "fewer rows than the minimum unit size")
  expect_equal(small$n, 25)
  expect_true(is.na(small$estimate))

  flat <- engine_prepare("9995", reg, function(rows) {
    rows$st_game <- 2
    rows
  })
  none <- fit_stage1(flat, engine_cells[1, ], reg$outcomes, reg$schema)
  expect_equal(none$status, "no variation in the exposure")

  rare <- engine_prepare("9994", reg, function(rows) {
    rows$beh_alcohol_self <- "FALSE"
    rows$beh_alcohol_self[1:3] <- "TRUE"
    rows
  })
  events <- fit_stage1(rare, engine_cells[2, ], reg$outcomes, reg$schema)
  expect_true(all(events$status == "fewer than 10 rows in one class"))

  expect_null(fit_stage1(NULL, engine_cells, reg$outcomes, reg$schema))
  no_cell <- tibble::tibble(exposure = "st_tablet", outcome = "mh_dep_self")
  expect_null(
    fit_stage1(engine_prepare("9995", reg), no_cell, reg$outcomes, reg$schema)
  )
})

test_that("age takes the most flexible form a unit's ages allow", {
  unit <- function(age) {
    n <- length(age)
    data.frame(
      x = stats::runif(n, 0, 5),
      y = stats::rnorm(n),
      age = age,
      sex = rep(c("Female", "Male"), length.out = n),
      weight = NA_real_,
      cluster = NA_character_,
      family = NA_character_
    )
  }
  set.seed(5)
  covariates <- function(age) fit_stage1_unit(unit(age), "linear")$covariates
  expect_equal(covariates(stats::runif(300, 8, 16)), "age (spline) + sex")
  expect_equal(covariates(stats::runif(300, 10, 11)), "age + sex")
  expect_equal(covariates(rep(12, 300)), "sex")
  # Ages heaped on three grades: a spline's knot would sit on a boundary.
  grades <- fit_stage1_unit(unit(rep(c(14, 16, 18), 100)), "linear")
  expect_equal(grades$status, "fitted")
  expect_equal(grades$covariates, "age (categories) + sex")
  # Many distinct ages, nearly all at one value: no room for a knot either.
  heaped <- fit_stage1_unit(
    unit(c(rep(15, 280), seq(10, 19, length.out = 20))),
    "linear"
  )
  expect_equal(heaped$status, "fitted")
  expect_equal(heaped$covariates, "age + sex")
})

test_that("an ordinal row with few levels takes a proportional-odds model", {
  skip_if_not_installed("survey")
  expect_equal(stage1_model_type("ordinal", 4), "ordinal")
  expect_equal(stage1_model_type("ordinal", 5), "linear")
  expect_equal(stage1_model_type("diagnosis", 3), "none")

  set.seed(3)
  n <- 3000
  x <- stats::rgamma(n, 2, 1)
  latent <- 0.3 * x + stats::rlogis(n)
  data <- data.frame(
    x = x,
    y = findInterval(latent, c(0, 1.5)) + 1,
    age = 12,
    sex = sample(c("Female", "Male"), n, TRUE),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_
  )
  fit <- fit_stage1_unit(data, "ordinal")
  expect_equal(fit$status, "fitted")
  expect_equal(fit$engine, "survey")
  expect_equal(fit$effect_measure, "log_odds_per_hour")
  expect_lt(abs(fit$estimate - 0.3), 0.06)
})

test_that("stage 2 needs three datasets and flags too few to say", {
  estimates <- tibble::tibble(
    dataset_id = c("a", "a", "b"),
    unit = c("a1", "a2", "b1"),
    estimate = c(0.1, 0.12, 0.09),
    se = 0.02,
    n = 100,
    status = "fitted"
  )
  two <- pool_stage2(estimates)
  expect_equal(two$status, "insufficient data")
  expect_true(is.na(two$estimate))
  expect_equal(two$k_datasets, 2)

  three <- pool_stage2(dplyr::bind_rows(
    estimates,
    tibble::tibble(
      dataset_id = "c",
      unit = "c1",
      estimate = 0.11,
      se = 0.02,
      n = 100,
      status = "fitted"
    )
  ))
  expect_equal(three$status, "too few studies to say")
  expect_lt(three$df, 4)
  expect_false(is.na(three$estimate))

  # Not-fitted rows are ignored, not pooled as zeros.
  estimates$status[1] <- "no variation in the exposure"
  expect_equal(pool_stage2(estimates)$k_units, 2)
})

test_that("stage 2 separates variation between datasets from sampling error", {
  set.seed(11)
  k <- 30
  true_slopes <- stats::rnorm(k, 0.2, 0.1)
  estimates <- tibble::tibble(
    dataset_id = rep(sprintf("d%02d", seq_len(k)), each = 2),
    unit = paste0(dataset_id, c("-1", "-2")),
    se = 0.02,
    estimate = rep(true_slopes, each = 2) + stats::rnorm(2 * k, 0, 0.02),
    n = 500,
    status = "fitted"
  )
  pooled <- pool_stage2(estimates)
  centre <- mean(true_slopes)
  spread <- stats::sd(true_slopes)
  expect_equal(pooled$status, "pooled")
  expect_lt(abs(pooled$estimate - centre), 0.01)
  expect_lt(abs(pooled$tau_dataset - spread), 0.01)
  expect_lt(pooled$tau_unit, 0.03)
  # The prediction interval is about the spread of slopes, far wider than
  # the interval for their mean.
  expect_true(
    pooled$pi_low < centre - 1.5 * spread &&
      pooled$pi_high > centre + 1.5 * spread
  )
  expect_true(
    pooled$ci_low > centre - spread && pooled$ci_high < centre + spread
  )
})

test_that("the one-stage check agrees with a matched two-stage estimate", {
  skip_if_not_installed("glmmTMB")
  reg <- engine_registries()
  ids <- unique(as.character(engine_mock$data$dataset_id))
  rows <- dplyr::bind_rows(lapply(ids, function(id) {
    extract_cell_rows(engine_prepare(id, reg), "st_game", "mh_dep_self")
  }))
  expect_equal(dplyr::n_distinct(rows$unit), 11)
  expect_named(rows, c("dataset_id", "unit", "x", "y", "age", "sex"))

  check <- fit_onestage_check(rows)
  truth <- mean(
    engine_mock$truth$effect[engine_mock$truth$outcome == "mh_dep_self"]
  )
  expect_equal(check$status, "fitted")
  expect_equal(check$k_datasets, 6)
  expect_lt(abs(check$onestage_estimate - truth), 0.02)
  expect_lt(abs(check$onestage_estimate - check$twostage_estimate), 0.005)
  expect_lt(check$contrast_gap, check$trigger)
  expect_false(check$exceeds_trigger)

  expect_equal(fit_onestage_check(NULL)$status, "no rows")
  two <- rows[rows$dataset_id %in% ids[1:2], ]
  expect_equal(fit_onestage_check(two)$status, "insufficient data")
  expect_null(
    extract_cell_rows(engine_prepare("9995", reg), "st_tablet", "mh_dep_self")
  )
})
