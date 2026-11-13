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

test_that("the one-stage check recovers a slope that simpler models miss", {
  skip_if_not_installed("glmmTMB")
  # Units differ in level, residual spread and age effect, and a unit's mean
  # screen use is tied to its mean outcome, so a model without an intercept
  # per unit, without centring, or with shared covariate effects is biased.
  set.seed(21)
  design <- expand.grid(dataset = 1:8, wave = 1:3)
  design$slope <- 0.10 +
    rep(stats::rnorm(8, 0, 0.03), times = 3) +
    stats::rnorm(24, 0, 0.01)
  design$x_mean <- stats::runif(24, 1, 5)
  design$sd <- rep(c(0.5, 1, 2), length.out = 24)
  design$age_effect <- stats::rnorm(24, 0, 0.3)
  rows <- dplyr::bind_rows(lapply(seq_len(nrow(design)), function(i) {
    d <- design[i, ]
    n <- 400
    x <- pmax(stats::rnorm(n, d$x_mean, 1), 0)
    age <- stats::runif(n, 9, 15)
    sex <- sample(c("Female", "Male"), n, TRUE)
    tibble::tibble(
      dataset_id = paste0("d", d$dataset),
      unit = paste0("d", d$dataset, "|", d$wave),
      x = x,
      age = age,
      sex = sex,
      y = -2 *
        d$x_mean +
        d$slope * x +
        d$age_effect * (age - 12) +
        0.4 * (sex == "Male") +
        stats::rnorm(n, 0, d$sd)
    )
  }))
  naive <- unname(stats::coef(stats::lm(y ~ x + age + sex, data = rows))["x"])
  expect_lt(naive, -1)

  check <- fit_onestage_check(rows)
  expect_equal(check$status, "fitted")
  expect_equal(check$k_datasets, 8)
  expect_equal(check$k_units, 24)
  expect_lt(abs(check$onestage_estimate - mean(design$slope)), 0.015)
  # Matched structures: the two approaches agree closely on the slope, on
  # both levels of variation, and the gap is on the 1 against 4 hours scale.
  difference <- abs(check$onestage_estimate - check$twostage_estimate)
  expect_lt(difference, 0.003)
  expect_equal(check$contrast_gap, 3 * difference)
  expect_lt(abs(check$onestage_tau - check$twostage_tau), 0.01)
  expect_false(is.na(check$onestage_tau_unit))
  expect_false(check$exceeds_trigger)

  strict <- analysis_settings()
  strict$onestage_trigger <- 0
  expect_true(fit_onestage_check(rows, strict)$exceeds_trigger)

  expect_equal(fit_onestage_check(NULL)$status, "no rows")
  two <- rows[rows$dataset_id %in% c("d1", "d2"), ]
  expect_equal(fit_onestage_check(two)$status, "insufficient data")
})

test_that("cell rows for the one-stage check come from the prepared tables", {
  reg <- engine_registries()
  prepared <- engine_prepare("9996", reg)
  rows <- extract_cell_rows(prepared, "st_game", "mh_dep_self")
  expect_named(rows, c("dataset_id", "unit", "x", "y", "age", "sex"))
  expect_equal(rows$y, prepared$outcomes_z$mh_dep_self)
  expect_equal(dplyr::n_distinct(rows$unit), 2)
  expect_null(extract_cell_rows(prepared, "st_tablet", "mh_dep_self"))

  cells <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  cell <- c(exposure = "st_game", outcome = "mh_dep_self")
  expect_equal(check_onestage_cell(cell, cells), cell)
  expect_error(
    check_onestage_cell(
      c(exposure = "st_watch", outcome = "mh_dep_self"),
      cells
    ),
    "not a development cell"
  )
})

test_that("clustered standard errors follow the clusters", {
  skip_if_not_installed("survey")
  set.seed(8)
  clusters <- 40
  n <- clusters * 30
  id <- rep(seq_len(clusters), each = 30)
  # Exposure and outcome both vary by cluster, so independence is badly wrong.
  x <- stats::rnorm(clusters)[id] + stats::rnorm(n)
  y <- 0.1 * x + stats::rnorm(clusters)[id] + stats::rnorm(n)
  unit <- data.frame(
    x = x,
    y = y,
    age = 12,
    sex = rep(c("Female", "Male"), length.out = n),
    weight = stats::runif(n, 0.5, 2),
    cluster = as.character(id),
    family = NA_character_
  )
  fit <- fit_stage1_unit(unit, "linear")
  expect_equal(fit$se_type, "clustered on sampling unit")
  expect_equal(fit$n_clusters, clusters)
  expect_equal(fit$df, clusters - 1)
  expect_false(fit$few_clusters)

  design <- survey::svydesign(ids = ~cluster, weights = ~weight, data = unit)
  reference <- survey::svyglm(y ~ x + sex, design = design)
  expect_equal(fit$estimate, unname(stats::coef(reference)["x"]))
  expect_equal(
    fit$se,
    unname(sqrt(diag(stats::vcov(reference)))["x"]),
    tolerance = 0.002
  )
  plain <- unit
  plain$cluster <- NA_character_
  independent <- fit_stage1_unit(plain, "linear")
  expect_equal(independent$se_type, "heteroskedasticity-robust")
  expect_gt(fit$se, 1.5 * independent$se)

  # A few rows with no sampling unit keep the clustering, each as its own.
  some_missing <- unit
  some_missing$cluster[1:10] <- NA
  kept <- fit_stage1_unit(some_missing, "linear")
  expect_equal(kept$se_type, "clustered on sampling unit")
  expect_equal(kept$n_clusters, clusters + 10)

  # Families, where no sampling unit is recorded and families repeat.
  families <- plain
  families$family <- as.character(id)
  expect_equal(
    fit_stage1_unit(families, "linear")$se_type,
    "clustered on family"
  )
  # Few clusters are fitted and flagged.
  few <- unit[id <= 6, ]
  expect_true(fit_stage1_unit(few, "linear")$few_clusters)
})

test_that("rows left out by the weight and sex rules are counted", {
  set.seed(4)
  n <- 300
  unit <- data.frame(
    x = stats::runif(n, 0, 5),
    y = stats::rnorm(n),
    age = stats::runif(n, 9, 15),
    sex = rep(c("Female", "Male"), length.out = n),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_
  )
  complete <- fit_stage1_unit(unit, "linear")
  expect_equal(c(complete$n, complete$n_available), c(n, n))
  expect_equal(c(complete$n_no_weight, complete$n_no_sex), c(0, 0))

  # Sex unknown for a few rows: they are left out and counted.
  few_unknown <- unit
  few_unknown$sex[1:12] <- NA
  few_unknown$sex[13:15] <- "Other"
  fit <- fit_stage1_unit(few_unknown, "linear")
  expect_equal(fit$n_no_sex, 15)
  expect_equal(fit$n, n - 15)
  expect_match(fit$covariates, "sex")

  # Sex unknown for most rows: the term goes, the rows stay.
  mostly_unknown <- unit
  mostly_unknown$sex[1:200] <- NA
  fit <- fit_stage1_unit(mostly_unknown, "linear")
  expect_equal(fit$status, "fitted")
  expect_equal(fit$n, n)
  expect_equal(fit$n_no_sex, 0)
  expect_false(grepl("sex", fit$covariates))

  # Weights on some rows: the unit is weighted on those rows, and says so.
  part_weighted <- unit
  part_weighted$weight[1:250] <- stats::runif(250, 0.5, 2)
  part_weighted$weight[251:260] <- 0
  fit <- fit_stage1_unit(part_weighted, "linear")
  expect_true(fit$weighted)
  expect_equal(fit$n_no_weight, 50)
  expect_equal(fit$n, 250)
  expect_equal(fit$n_available, n)
})

test_that("an outcome's scale does not depend on which exposure is fitted", {
  reg <- engine_registries()
  # Half the rows lack gaming, and they are the low scorers.
  prepared <- engine_prepare("9995", reg, function(rows) {
    rows$st_game[rows$mh_dep_self < stats::median(rows$mh_dep_self)] <- NA
    rows
  })
  cells <- tibble::tibble(
    exposure = c("st_game", "st_watch"),
    outcome = "mh_dep_self"
  )
  fits <- fit_stage1(prepared, cells, reg$outcomes, reg$schema)
  has_game <- !is.na(prepared$exposures$st_game)
  data <- data.frame(
    y = prepared$outcomes_z$mh_dep_self[has_game],
    x = prepared$exposures$st_game[has_game],
    age = prepared$base$age_years[has_game],
    sex = prepared$base$sex[has_game],
    w = prepared$base$survey_weight_norm[has_game],
    cluster = prepared$base$survey_cluster[has_game]
  )
  # The slope is in the preparation step's SD units, not rescaled to the SD
  # of the rows that happen to have this exposure.
  reference <- fixest::feols(
    y ~ x + splines::ns(age, df = 2) + sex,
    data = data,
    weights = ~w,
    vcov = ~cluster
  )
  expect_equal(
    fits$estimate[fits$exposure == "st_game"],
    unname(stats::coef(reference)["x"])
  )
  expect_lt(stats::sd(data$y), 0.8)
})

test_that("the proportional-odds path runs from prepared data", {
  skip_if_not_installed("survey")
  reg <- engine_registries()
  ratings <- c("Very good", "Good", "Average", "Below average")
  prepared <- engine_prepare("9995", reg, function(rows) {
    latent <- 0.4 * rows$st_watch + stats::rlogis(nrow(rows))
    rows$acc_academachment_parentrated <- ratings[
      findInterval(latent, c(0.3, 1.5, 2.8)) + 1
    ]
    rows
  })
  cell <- tibble::tibble(
    exposure = "st_watch",
    outcome = "acc_academachment_parentrated"
  )
  fit <- fit_stage1(prepared, cell, reg$outcomes, reg$schema)
  expect_equal(fit$status, "fitted")
  expect_equal(fit$engine, "survey")
  expect_equal(fit$effect_measure, "log_odds_per_hour")
  # More watching moves towards the later levels, in the schema's order.
  expect_lt(abs(fit$estimate - 0.4), 0.1)
  expect_equal(fit$n_clusters, 20)
})

test_that("development cells expand a family to its pooled outcome rows", {
  reg <- engine_registries()
  dir <- withr::local_tempdir()
  write <- function(lines) {
    path <- file.path(dir, "development_cells.csv")
    writeLines(c("exposure,family,variable_name,reason", lines), path)
    path
  }
  read <- function(lines) {
    read_development_cells(write(lines), reg$outcomes, reg$exposures)
  }
  cells <- read(c(
    "st_watch,wellbeing,,A family",
    "st_watch,,acc_graderepeated,One row",
    "st_watch,,wb_lifesat_self,Already in the family"
  ))
  rows <- reg$outcomes$rows
  expected <- rows$variable_name[
    rows$prereg_family %in%
      "wellbeing" &
      rows$role %in% "outcome" &
      rows$use %in% "core" &
      rows$synthesis %in% TRUE
  ]
  expect_setequal(cells$outcome, c(expected, "acc_graderepeated"))
  expect_false(anyDuplicated(cells) > 0)
  expect_false("wb_bodyimage" %in% cells$outcome)
  expect_equal(unique(cells$exposure), "st_watch")

  expect_error(read("st_nothing,wellbeing,,why"), "not exposure rows")
  expect_error(read("st_watch,no_family,,why"), "not outcome families")
  expect_error(read("st_watch,,no_row,why"), "not outcome rows")
  expect_error(read("st_watch,wellbeing,wb_lifesat_self,why"), "not both")
  expect_error(read("st_watch,,,why"), "not both")
  expect_error(read("st_watch,wellbeing,,"), "needs a reason")

  # The committed file passes, and holds the one-stage check's cell.
  committed <- read_development_cells(
    file.path(repo_root, bp_development_cells_path()),
    reg$outcomes,
    reg$exposures
  )
  expect_no_error(
    check_onestage_cell(analysis_settings()$onestage_cell, committed)
  )
})

test_that("every cell asked for gets a stage 2 row", {
  estimates <- tibble::tibble(
    dataset_id = c("a", "b", "c", "a"),
    unit = c("a1", "b1", "c1", "a1"),
    exposure = "st_watch",
    outcome = c(rep("mh_dep_self", 3), "mh_anx_self"),
    score_type = "continuous",
    effect_measure = c(rep("sd_per_hour", 3), NA),
    estimate = c(0.1, 0.12, 0.09, NA),
    se = c(0.02, 0.02, 0.02, NA),
    n = 100,
    status = c(rep("fitted", 3), "no variation in the exposure")
  )
  cells <- tibble::tibble(
    exposure = "st_watch",
    outcome = c("mh_dep_self", "mh_anx_self", "wb_lifesat_self")
  )
  pooled <- pool_stage2_cells(estimates, cells)
  status <- stats::setNames(pooled$status, pooled$outcome)
  expect_equal(nrow(pooled), 3)
  expect_equal(status[["mh_anx_self"]], "no unit could be fitted")
  expect_equal(status[["wb_lifesat_self"]], "no data")
  expect_false(is.na(pooled$estimate[pooled$outcome == "mh_dep_self"]))
  expect_equal(nrow(pool_stage2_cells(estimates)), 1)
})

test_that("a weighted summary gives weighted quantiles and shares", {
  plain <- weighted_summary(c(0, 0, 1, 2, 3, 5))
  expect_equal(plain$n, 6)
  expect_equal(plain$median, 1)
  expect_equal(plain$share_zero, 2 / 6)
  expect_equal(plain$share_2plus, 3 / 6)
  expect_equal(plain$share_4plus, 1 / 6)
  # All the weight on the top value moves every statistic to it.
  heavy <- weighted_summary(c(0, 1, 5), c(1, 1, 98))
  expect_equal(heavy$median, 5)
  expect_equal(heavy$p10, 5)
  expect_equal(heavy$share_zero, 0.01)
  expect_equal(heavy$mean, (1 + 5 * 98) / 100)
  # Missing values and non-positive weights are left out.
  expect_equal(weighted_summary(c(1, NA, 3), c(1, 1, 0))$n, 1)
  expect_true(is.na(weighted_summary(numeric())$median))
})

test_that("descriptives are per unit, weighted, and never relate x to y", {
  reg <- engine_registries()
  prepared <- engine_prepare("9996", reg)
  described <- describe_dataset(prepared, reg$outcomes, reg$exposures)
  expect_named(
    described,
    c("screen", "week", "relations", "outcomes", "units")
  )
  expect_equal(nrow(described$units), 2)
  expect_setequal(described$units$country, c("Ireland", "Scotland"))

  screen <- described$screen
  game <- screen[screen$variable_name == "st_game" & screen$grouping == "all", ]
  ireland <- prepared$base$unit == game$unit[game$country == "Ireland"]
  expect_equal(
    game$median[game$country == "Ireland"],
    stats::median(prepared$exposures$st_game[ireland]),
    tolerance = 0.01
  )
  expect_setequal(
    unique(screen$grouping),
    c("all", "school_stage", "sex")
  )
  expect_true("total_screen" %in% screen$variable_name)
  expect_true(all(screen$n >= analysis_settings()$min_unit_rows))

  # The same two screen measures in every unit, one correlation each.
  expect_equal(nrow(described$relations), 2)
  expect_true(all(abs(described$relations$spearman) < 0.2))
  # No table holds an outcome beside an exposure.
  expect_false(any(c("mh_dep_self") %in% unlist(lapply(described, names))))
  expect_null(describe_dataset(NULL, reg$outcomes, reg$exposures))

  # Weekday against weekend on the same rows, and outcome prevalence.
  week_rows <- engine_prepare("9996", reg, function(rows) {
    rows$st_watch_wd <- rows$st_watch
    rows$st_watch_we <- rows$st_watch + 1
    rows$st_watch_we[1:50] <- NA
    rows
  })
  week <- describe_dataset(week_rows, reg$outcomes, reg$exposures)$week
  expect_equal(week$mean_weekend - week$mean_weekday, c(1, 1))
  expect_equal(week$share_more_on_weekend, c(1, 1))
  expect_equal(sum(week$n), nrow(week_rows$base) - 50)
  with_alcohol <- engine_prepare("9994", reg)
  prevalence <- describe_dataset(
    with_alcohol,
    reg$outcomes,
    reg$exposures
  )$outcomes
  overall <- prevalence[
    prevalence$variable_name == "beh_alcohol_self" &
      prevalence$grouping == "all",
  ]
  first <- with_alcohol$base$unit == overall$unit[1]
  expect_equal(
    overall$prevalence[1],
    mean(with_alcohol$outcomes$beh_alcohol_self[first])
  )
  # A panel repeats participants; a repeated cross-section does not.
  expect_true(all(
    describe_dataset(
      engine_prepare("9991", reg),
      reg$outcomes,
      reg$exposures
    )$units$repeats_participants
  ))
  expect_false(any(described$units$repeats_participants))

  # Weighted where the unit has weights: dataset 9995 does.
  weighted <- engine_prepare("9995", reg)
  out <- describe_dataset(weighted, reg$outcomes, reg$exposures)
  expect_true(out$units$weighted)
  w <- weighted$base$survey_weight_norm
  watch <- out$screen[
    out$screen$variable_name == "st_watch" & out$screen$grouping == "all",
  ]
  expect_equal(watch$mean, sum(w * weighted$exposures$st_watch) / sum(w))
  expect_equal(out$units$rows_without_weight, 0)
  # Rows with no weight in a weighted unit are left out and counted.
  partly <- engine_prepare("9995", reg, function(rows) {
    rows$survey_weight[1:100] <- NA
    rows
  })
  part <- describe_dataset(partly, reg$outcomes, reg$exposures)
  expect_equal(part$units$rows_without_weight, 100)
  part_watch <- part$screen[
    part$screen$variable_name == "st_watch" & part$screen$grouping == "all",
  ]
  expect_equal(part_watch$n, nrow(partly$base) - 100)
})

test_that("pooled descriptives count each dataset once and need three", {
  screen <- tibble::tibble(
    dataset_id = c("a", "a", "b", "c", "d", "d"),
    unit = c("a1", "a2", "b1", "c1", "d1", "d2"),
    variable_name = c(rep("st_watch", 4), rep("st_game", 2)),
    grouping = "all",
    group = "all",
    n = c(100, 300, 50, 1e6, 10, 10),
    median = c(1, 3, 4, 2, 9, 9),
    p25 = 1,
    p75 = 3,
    p90 = 4,
    share_zero = 0.1,
    share_2plus = 0.5,
    share_4plus = 0.2
  )
  units <- tibble::tibble(
    unit = screen$unit,
    total_source = NA_character_,
    total_coverage = NA_integer_
  )
  pooled <- pool_descriptives(screen, units)
  # Gaming has one dataset and is not pooled.
  expect_equal(pooled$variable_name, "st_watch")
  expect_equal(pooled$datasets, 3)
  # Dataset a's units average to 2.5; the huge dataset c counts once.
  expect_equal(pooled$median_mid, stats::median(c(2.5, 4, 2)))

  total <- screen[1:4, ]
  total$variable_name <- "total_screen"
  units$total_source <- c("summed", "summed", "summed", "dedicated", NA, NA)
  units$total_coverage <- c(2L, 2L, 2L, 4L, NA, NA)
  apart <- pool_descriptives(total, units)
  expect_equal(nrow(apart), 0)
})

test_that("coverage and the scale of the work are counted, not typed", {
  reg <- engine_registries()
  audit <- tibble::tibble(
    dataset_id = c("1", "1", "2", "2", "2"),
    wave = c("a", "b", "a", "a", "a"),
    variable_name = c(
      "mh_dep_self",
      "mh_dep_self",
      "mh_dep_smfq_self",
      "st_watch",
      "mh_anx_self"
    ),
    kind = c("outcome", "outcome", "outcome", "exposure", "outcome"),
    n_used = c(10, 20, 30, 40, 50),
    status = c("used", "used", "used", "used", "dropped_repeat")
  )
  measured <- summarise_measured(audit, reg$outcomes, reg$exposures)
  depression <- measured$outcomes[measured$outcomes$node == "depression", ]
  expect_equal(depression$datasets, 2)
  expect_equal(depression$dataset_waves, 3)
  expect_equal(depression$scores, 60)
  expect_equal(depression$schema_rows, 2)
  expect_false("anxiety" %in% measured$outcomes$node)
  expect_equal(measured$exposures$exposure, "watch")

  # A column used by rows of two statuses counts once; a column named only
  # by a row that was not mapped does not count; free text is not a column.
  variables <- tibble::tibble(
    status = c(
      "compatible",
      "partial",
      "incompatible",
      "unavailable",
      "inferred"
    ),
    source_columns = c("a, b", "b; c", "d", NA, "CBCL 101")
  )
  mapping <- count_mapping(variables, 1)
  expect_equal(sum(mapping$rows), 5)
  expect_equal(unique(mapping$tidied_columns), 3)
  resources <- count_resources(
    tibble::tibble(
      role = c("data", "data", "codebook"),
      reader = c("csv", "csv", NA)
    ),
    1
  )
  expect_equal(resources$files[resources$role == "data"], 2)

  tidiers <- withr::local_tempdir()
  file.create(file.path(
    tidiers,
    c("tidy_BPIPD_1.R", "tidy_BPIPD_7.R", "tidy_BPIPD_TEMPLATE.R")
  ))
  lookups <- withr::local_tempdir()
  dir.create(file.path(lookups, "BPIPD-1", "lookups"), recursive = TRUE)
  dir.create(file.path(lookups, "BPIPD-7", "lookups"), recursive = TRUE)
  dir.create(file.path(lookups, "lookups"))
  file.create(file.path(lookups, "BPIPD-1", "lookups", "own.csv"))
  file.create(file.path(lookups, "BPIPD-7", "lookups", "other.csv"))
  file.create(file.path(lookups, "lookups", "shared.csv"))
  scale <- summarise_work_scale(
    resources,
    mapping,
    reg$schema,
    tibble::tibble(dataset_id = "1", participants = 5, primary_rows = 9),
    tibble::tibble(rows_kept = 8),
    tidier_dir = tidiers,
    lookup_dirs = lookups
  )
  count <- function(item) scale$totals$count[scale$totals$item == item]
  expect_equal(count("Raw data files read"), 2)
  expect_equal(count("Codebooks and other documents supplied with the data"), 1)
  expect_equal(count("Of those, mapped with data"), 3)
  expect_equal(count("Mapping decisions recorded (a variable in a dataset)"), 5)
  expect_equal(count("Columns of the tidied data those mappings draw on"), 3)
  expect_equal(count("Observations after the age and trial rules"), 8)
  # Only the included dataset's tidier and lookups, and never the template.
  expect_equal(count("Study-specific tidying scripts"), 1)
  expect_equal(count("Lookup tables"), 2)
  expect_equal(count("Variables in the shared schema"), nrow(reg$schema))
})

test_that("any per-unit table pools with each dataset counted once", {
  table <- tibble::tibble(
    dataset_id = c("a", "a", "b", "c", "d", "e"),
    key = c(rep("x", 4), rep("y", 2)),
    n = c(100, 300, 50, 1e6, 10, 10),
    value = c(1, 3, 4, 2, 9, 9),
    other = c(0.1, 0.3, 0.9, 0.5, 0, 0)
  )
  pooled <- pool_unit_table(table, c("value", "other"), "key")
  expect_equal(pooled$key, "x")
  expect_equal(pooled$datasets, 3)
  # Dataset a averages to 2.5 and 0.25; the three datasets are then ranked.
  expect_equal(pooled$value_mid, 2.5)
  expect_equal(pooled$value_low, 2.25)
  expect_equal(pooled$value_high, 3.25)
  expect_equal(pooled$other_mid, 0.5)
  expect_equal(pooled$other_low, 0.375)
  expect_equal(pooled$other_high, 0.7)
})

test_that("units form a series only where they measure the same thing", {
  reg <- engine_registries()
  units <- tibble::tibble(
    dataset_id = c("1", "1", "1", "1", "1", "2", "170", "170"),
    country = c("A", "A", "B", "A", "A", "A", "USA", "USA"),
    responder = c(
      "Self",
      "Self",
      "Self",
      "Parent or carer",
      "Self",
      "Self",
      "Self",
      "Self"
    ),
    measure_type = "Categorical",
    age_median = c(13, 14, 13, 13, 17, 13, 15, 15),
    year = c(2000, 2005, 2000, 2000, 2010, 2000, 2015, 2019),
    wave = as.character(1:8)
  )
  breaks <- read_series_breaks(
    file.path(repo_root, bp_series_breaks_path()),
    reg$exposures
  )
  expect_true(all(c("dataset_id", "exposure", "from_year") %in% names(breaks)))
  series <- assign_series(units, "watch", breaks)
  # Same country, respondent, format and school stage: one series.
  expect_equal(series[1], series[2])
  # Another country, respondent, school stage or dataset: another series.
  expect_equal(dplyr::n_distinct(series[1:6]), 5)
  # A recorded change to the watching item splits the years either side.
  expect_false(series[7] == series[8])
  expect_equal(
    assign_series(units[7:8, ], "educational", breaks)[1],
    assign_series(units[7:8, ], "educational", breaks)[2]
  )

  dir <- withr::local_tempdir()
  path <- file.path(dir, "series_breaks.csv")
  header <- "dataset_id,exposure,day,from_year,waves,reason"
  write_breaks <- function(...) writeLines(c(header, ...), path)
  write_breaks("1,not_an_exposure,,2000,,why")
  expect_error(read_series_breaks(path, reg$exposures), "not exposures")
  write_breaks("1,watch,someday,2000,,why")
  expect_error(read_series_breaks(path, reg$exposures), "not days")
  write_breaks("1,watch,,,,why")
  expect_error(read_series_breaks(path, reg$exposures), "either a year")
  write_breaks("1,watch,,2000,a;b,why")
  expect_error(read_series_breaks(path, reg$exposures), "either a year")
  write_breaks("1,watch,,soon,,why")
  expect_error(read_series_breaks(path, reg$exposures), "must be a year")
  write_breaks("1,watch,,2000,,")
  expect_error(read_series_breaks(path, reg$exposures), "a reason")
})

test_that("a change keyed by waves or by day splits only those units", {
  reg <- engine_registries()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "series_breaks.csv")
  writeLines(
    c(
      "dataset_id,exposure,day,from_year,waves,reason",
      "p,watch,,2002,,new bands",
      "p,watch,,,b; d,mail-out form",
      "p,watch,weekend,2004,,weekend item only",
      "p,game,,,zz,no such wave",
      "q,game,,1990,,before any unit"
    ),
    path
  )
  breaks <- read_series_breaks(path, reg$exposures)
  expect_equal(breaks$waves[[2]], c("b", "d"))
  units <- tibble::tibble(
    dataset_id = c(rep("p", 6), "q"),
    wave = c("a", "b", "c", "d", "e", "f", "a"),
    country = "A",
    responder = "Self",
    measure_type = "Categorical",
    age_median = 13,
    year = c(2000, 2001, 2002, 2002, 2003, 2004, 2000)
  )
  series <- assign_series(units, "watch", breaks, day = "weekday")
  # Waves a and b are before the new bands; b and d are the mail-out form,
  # which came and went, so c, e and f are one series around it.
  expect_equal(series[3], series[5])
  expect_equal(series[5], series[6])
  expect_equal(dplyr::n_distinct(series[1:6]), 4)
  # Waves sharing a year are told apart by wave.
  expect_false(series[3] == series[4])
  # The weekend-only change splits the weekend row and no other.
  weekend <- assign_series(units, "watch", breaks, day = "weekend")
  expect_false(weekend[5] == weekend[6])
  expect_equal(weekend[3], weekend[5])
  # The derived total has no exposure, so every change in the dataset counts.
  total <- assign_series(units, NA_character_, breaks)
  expect_false(total[5] == total[6])
  # Lines that separate nothing are reported.
  units$unit <- paste0(units$dataset_id, units$wave)
  screen <- tibble::tibble(
    unit = c(units$unit[1:6], units$unit[6]),
    variable_name = c(rep("st_watch_wd", 6), "st_watch_we")
  )
  unused <- check_series_breaks(breaks, units, screen, reg$exposures)
  # The weekend change has weekend values only after it, so splits nothing;
  # game has no statistics, so its lines are checked against every unit.
  expect_equal(unused$exposure, c("watch", "game", "game"))
  expect_equal(unused$day, c("weekend", NA, NA))
  expect_equal(unused$units, c(1L, 6L, 1L))
  expect_equal(unused$matched, c(1L, 0L, 1L))
  expect_error(assign_series(units[names(units) != "wave"], "watch", breaks))

  header <- "dataset_id,exposure,day,from_year,waves,reason"
  writeLines(c(header, "p,watch,,,a;;b,why"), path)
  expect_error(read_series_breaks(path, reg$exposures), "empty entry")
  writeLines(c(header, "p,watch,,2012.5,,why"), path)
  expect_error(read_series_breaks(path, reg$exposures), "must be a year")
  writeLines(c(header, "p,watch,,2012,,why", "p,watch,,2012,,again"), path)
  expect_error(read_series_breaks(path, reg$exposures), "appears twice")
})

test_that("the dataset table gives one row per dataset, by number", {
  units <- tibble::tibble(
    dataset_id = c("2", "2", "1", "3", "3"),
    wave = c("a", "b", NA, "a", "b"),
    country = c("A", "A", "B", "A", "C"),
    year = c(2000, 2004, 2010, 2001, 2001),
    age_median = c(10, 12, 15, 9, 9),
    rows = c(100, 90, 50, 30, 40),
    repeats_participants = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    weighted = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  mapping <- tibble::tibble(
    dataset_id = c("1", "1", "2", "3"),
    status = c("compatible", "unavailable", "partial", "inferred"),
    rows = c(4, 500, 7, 2)
  )
  table <- summarise_dataset_table(units, mapping)
  expect_equal(table$dataset_id, c("1", "2", "3"))
  expect_equal(
    table$design,
    c(
      "Single occasion",
      "Follows the same children",
      "Repeated survey, new sample each time"
    )
  )
  expect_equal(table$variables_supplied, c(4, 7, 2))
  expect_equal(table$countries, c(1, 1, 2))
  expect_false("dataset_name" %in% names(table))
  # Citation keys become a pandoc citation; a dataset with none is blank.
  expect_equal(table$sources, c("", "", ""))
  cited <- summarise_dataset_table(
    units,
    mapping,
    tibble::tibble(
      dataset_id = c("2", "2", "9"),
      citekey = c("a2020", "b2021", "z")
    )
  )
  expect_equal(cited$sources, c("", "[@a2020; @b2021]", ""))
})

test_that("dataset outcomes are entered by hand and can stay blank", {
  committed <- read_dataset_outcomes(
    file.path(repo_root, bp_dataset_outcomes_path())
  )
  expect_equal(committed$outcome, names(bp_dataset_outcomes()))
  expect_false(anyNA(committed$definition))

  dir <- withr::local_tempdir()
  path <- file.path(dir, "dataset_outcomes.csv")
  write <- function(counts) {
    writeLines(
      c(
        "outcome,datasets,definition",
        paste0(names(bp_dataset_outcomes()), ",", counts, ",what it means")
      ),
      path
    )
    read_dataset_outcomes(path)
  }
  manifest <- tibble::tibble(dataset_id = as.character(1:3))
  blank <- summarise_dataset_outcomes(write(rep("", 6)), manifest)
  expect_false(blank$complete)
  expect_true(is.na(blank$sought))
  expect_true(is.na(blank$agrees))
  expect_equal(blank$table$in_analysis[1], 3)

  partly <- summarise_dataset_outcomes(write(c(3, "", "", 4, "", "")), manifest)
  expect_false(partly$complete)
  expect_true(partly$agrees)

  full <- summarise_dataset_outcomes(write(c(5, 1, 2, 4, 6, 3)), manifest)
  expect_true(full$complete)
  expect_equal(full$sought, 21)
  expect_false(full$agrees)

  expect_error(write(c("many", 1, 2, 4, 6, 3)), "whole number or blank")
  writeLines(c("outcome,datasets,definition", "included,1,x"), path)
  expect_error(read_dataset_outcomes(path), "exactly one line for each")
})

test_that("a trend over years is split within and between datasets", {
  set.seed(31)
  # Twelve repeated surveys and eight panels. Slopes rise 0.05 per decade
  # inside a survey; across surveys they rise 0.02 per decade. In the panels
  # the year moves with age, and age shifts the estimate strongly.
  survey <- function(id, start, panel) {
    years <- start + c(0, 5, 10)
    centre <- mean(years)
    age <- if (panel) 8 + (years - start) / 2 else rep(13, 3)
    tibble::tibble(
      dataset_id = id,
      unit = paste(id, years),
      year = years,
      series = paste(id, if (panel) years else "all"),
      age_median = age,
      measure_type = "Categorical",
      responder = "Self",
      sei = 0.01,
      yi = 0.02 *
        (centre - 2010) /
        10 +
        (if (panel) 0 else 0.05 * (years - centre) / 10) +
        0.03 * (age - 13) +
        stats::rnorm(3, 0, 0.01)
    )
  }
  units <- dplyr::bind_rows(
    lapply(1:12, function(i) survey(paste0("s", i), 1990 + 2 * i, FALSE)),
    lapply(1:8, function(i) survey(paste0("p", i), 1992 + 3 * i, TRUE))
  )
  fit <- regress_units_on_year(units)
  expect_equal(fit$status, "fitted")
  expect_equal(fit$k_datasets, 20)
  expect_equal(fit$k_within, 12)
  expect_lt(abs(fit$within_per_decade - 0.05), 0.01)
  expect_lt(abs(fit$between_per_decade - 0.02), 0.015)
  expect_true(fit$within_ci_low < 0.05 && fit$within_ci_high > 0.05)
  expect_match(fit$adjusted_for, "median age")

  # A panel's waves at different ages are separate series, so they add
  # nothing within; put in one series, each panel would add its own years.
  one_series <- units
  one_series$series <- one_series$dataset_id
  expect_equal(regress_units_on_year(one_series)$k_within, 20)
  expect_equal(fit$within_status, "estimated")

  expect_equal(
    regress_units_on_year(units[
      units$dataset_id %in% paste0("s", 1:5),
    ])$status,
    "too few datasets or years"
  )
  narrow <- units
  narrow$year <- 2010 + (narrow$year %% 3)
  expect_equal(
    regress_units_on_year(narrow)$status,
    "too few datasets or years"
  )
})

test_that("year moderation and the cross-dataset trend use unit descriptions", {
  set.seed(32)
  ids <- sprintf("d%02d", 1:12)
  reg <- engine_registries()
  breaks <- read_series_breaks(
    file.path(repo_root, bp_series_breaks_path()),
    reg$exposures
  )
  units <- tibble::tibble(
    dataset_id = rep(ids, each = 2),
    unit = paste0(dataset_id, c("a", "b")),
    wave = rep(c("a", "b"), 12),
    country = "A",
    year = rep(seq(1998, 2020, length.out = 12), each = 2) + c(0, 4),
    age_median = stats::runif(24, 12.5, 15.5),
    measure_type = "Categorical",
    responder = "Self"
  )
  stage1 <- tibble::tibble(
    dataset_id = units$dataset_id,
    unit = units$unit,
    exposure = "st_watch",
    outcome = "mh_dep_self",
    effect_measure = "sd_per_hour",
    estimate = 0.04 * (units$year - 2010) / 10 + stats::rnorm(24, 0, 0.005),
    se = 0.01,
    status = "fitted"
  )
  moderation <- moderate_by_year(stage1, units, reg$exposures, breaks)
  expect_equal(nrow(moderation), 1)
  expect_equal(moderation$status, "fitted")
  expect_lt(abs(moderation$between_per_decade - 0.04), 0.015)
  expect_lt(abs(moderation$within_per_decade - 0.04), 0.03)
  expect_equal(
    nrow(moderate_by_year(stage1[0, ], units, reg$exposures, breaks)),
    0
  )
  # A change at every dataset's second wave leaves no series over years; one
  # limited to the weekend row leaves the usual-day row whole.
  split_all <- breaks[rep(1, 12), ]
  split_all$dataset_id <- ids
  split_all$exposure <- "watch"
  split_all$day <- NA_character_
  split_all$from_year <- NA_real_
  split_all$waves <- rep(list("b"), 12)
  weekend_only <- split_all
  weekend_only$day <- "weekend"
  expect_gt(moderation$k_within, 0)
  expect_equal(
    moderate_by_year(stage1, units, reg$exposures, split_all)$k_within,
    0
  )
  expect_equal(
    moderate_by_year(stage1, units, reg$exposures, weekend_only)$k_within,
    moderation$k_within
  )

  screen <- tibble::tibble(
    dataset_id = rep(units$dataset_id, 2),
    unit = rep(units$unit, 2),
    variable_name = rep(c("st_watch", "total_screen"), each = 24),
    grouping = "all",
    n = 500,
    mean = 2 +
      0.5 * (rep(units$year, 2) - 2010) / 10 +
      stats::rnorm(48, 0, 0.02),
    se_mean = 0.03,
    share_2plus = 0.4
  )
  trend <- trend_across_datasets(screen, units, reg$exposures, breaks)
  expect_equal(trend$trends$variable_name, "st_watch")
  expect_lt(abs(trend$trends$between_per_decade - 0.5), 0.1)
  expect_equal(
    trend_across_datasets(
      screen,
      units,
      reg$exposures,
      split_all
    )$trends$k_within,
    0
  )
  expect_equal(
    trend_across_datasets(
      screen,
      units,
      reg$exposures,
      weekend_only
    )$trends$k_within,
    trend$trends$k_within
  )
  # The figure's table counts each dataset once per period, needs three
  # datasets, carries no dataset label, and leaves the total out.
  expect_false("dataset_id" %in% names(trend$periods))
  expect_equal(unique(trend$periods$variable_name), "st_watch")
  expect_true(all(trend$periods$datasets >= 3))
  expect_true(all(trend$periods$low <= trend$periods$mid))
})

test_that("the tree pools the mock's rows, counting a copied score once", {
  reg <- engine_registries()
  ids <- unique(as.character(engine_mock$data$dataset_id))
  cells <- tibble::tibble(
    exposure = "st_game",
    outcome = c("mh_dep_self", "mh_anx_self", "mh_distress_self")
  )
  # The depression score is entered again as an anxiety row, a different
  # specific outcome, so the preparation step keeps both; and a distress row
  # is made that shares half its variation with depression.
  set.seed(11)
  edit <- function(rows) {
    rows$mh_anx_self <- rows$mh_dep_self
    rows$mh_distress_self <- rows$mh_dep_self +
      stats::rnorm(nrow(rows), 0, stats::sd(rows$mh_dep_self, na.rm = TRUE))
    rows
  }
  prepared <- lapply(ids, engine_prepare, reg = reg, edit = edit)
  stage1 <- dplyr::bind_rows(lapply(prepared, function(p) {
    fit_stage1(p, cells, reg$outcomes, reg$schema)
  }))
  # Each row is its own specific outcome, so no pair is within one.
  expect_null(stage1_correlations(prepared[[1]], cells, reg$outcomes))

  tree <- pool_tree(stage1, reg$outcomes)
  level <- function(name) tree[tree$level == name, ]
  l1 <- level("L1")
  expect_setequal(l1$node, c("depression", "anxiety", "psychological_distress"))
  expect_true(all(tree$family == "mental_health"))
  # Higher depression is worse, so the mock's positive slope turns negative,
  # and a specific outcome of one row is that row's stage 2 estimate.
  dep <- l1[l1$node == "depression", ]
  stage2 <- pool_stage2(stage1[stage1$outcome == "mh_dep_self", ])
  expect_equal(dep$estimate, -stage2$estimate)
  expect_equal(dep$se, stage2$se)
  anx <- l1[l1$node == "anxiety", ]
  expect_equal(anx$estimate, dep$estimate)

  # Depression and its copy alone: the family is exactly one of them.
  pair <- pool_tree(
    stage1[stage1$outcome != "mh_distress_self", ],
    reg$outcomes
  )
  l0 <- pair[pair$level == "L0", ]
  expect_equal(l0$k_nodes, 2)
  expect_equal(l0$estimate, dep$estimate)
  expect_equal(l0$se, dep$se)
  expect_equal(l0$df, dep$df)

  # With distress the family is the plain average of the three.
  l0 <- level("L0")
  expect_equal(l0$k_nodes, 3)
  expect_equal(l0$estimate, mean(l1$estimate))
  expect_equal(l0$status, "pooled")
})

# One unit where screen use and the outcome both rise with household SES;
# the true slope is 0.10.
tier_unit <- function(n = 3000, seed = 31) {
  set.seed(seed)
  ses <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
  level <- match(ses, c("Low", "Medium", "High"))
  data <- data.frame(
    x = stats::rgamma(n, shape = 2) + level,
    age = stats::runif(n, 8, 12),
    sex = sample(c("Female", "Male"), n, replace = TRUE),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_,
    ses_household = ses,
    parental_education = sample(c("A", "B"), n, replace = TRUE),
    ses_area = sample(c("P", "Q"), n, replace = TRUE),
    ethnicity = NA_character_,
    minority = sample(c("Minority", "Non-minority"), n, replace = TRUE),
    urbanicity = sample(c("Rural", "Urban"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  data$y <- 0.10 * data$x + 0.8 * level + stats::rnorm(n)
  data
}

test_that("a further covariate tier removes confounding the first leaves in", {
  data <- tier_unit()
  data$ses_household[1:100] <- NA
  first <- fit_stage1_unit(data, "linear", tier = 1L)
  second <- fit_stage1_unit(data, "linear", tier = 2L)
  expect_equal(c(first$tier, second$tier), 1:2)
  expect_gt(first$estimate, 0.2)
  expect_equal(second$estimate, 0.10, tolerance = 0.03, scale = 1)
  # Household SES is used first, and rows where it is unknown are left out.
  expect_equal(second$ses_measure, "ses_household")
  expect_true(is.na(first$ses_measure))
  expect_equal(second$n_no_covariate, 100)
  expect_equal(second$n, 2900)
  expect_match(second$covariates, "ses_household")
  # The tier 1 model on the tier's 2,900 rows is still confounded, and is
  # not the tier 1 slope on all 3,000.
  expect_gt(second$tier1_same_rows_estimate, 0.2)
  expect_false(second$tier1_same_rows_estimate == first$estimate)
  expect_true(is.na(first$tier1_same_rows_estimate))
  expect_error(fit_stage1_unit(data, "linear", tier = 4L), "must be 1, 2 or 3")
})

test_that("a tier takes the first measure the unit records well enough", {
  data <- tier_unit()
  # Exactly at the minimum share it counts; just under, the next is taken.
  data$ses_household[1:900] <- NA
  expect_equal(
    fit_stage1_unit(data, "linear", tier = 2L)$ses_measure,
    "ses_household"
  )
  data$ses_household[1:901] <- NA
  expect_equal(
    fit_stage1_unit(data, "linear", tier = 2L)$ses_measure,
    "parental_education"
  )
  data$parental_education <- NA_character_
  expect_equal(
    fit_stage1_unit(data, "linear", tier = 2L)$ses_measure,
    "ses_area"
  )
  data$ses_area <- NA_character_
  none <- fit_stage1_unit(data, "linear", tier = 2L)
  expect_equal(none$status, "no socioeconomic measure that varies in the unit")
  expect_true(is.na(none$estimate))
})

test_that("tier 3 adds ethnicity, or else minority status, and urbanicity", {
  data <- tier_unit()
  third <- fit_stage1_unit(data, "linear", tier = 3L)
  expect_equal(third$status, "fitted")
  expect_equal(third$ethnicity_measure, "minority")
  expect_equal(
    third$covariates,
    paste(
      "age (spline) + sex + ses_household + minority + urbanicity"
    )
  )
  expect_equal(third$estimate, 0.10, tolerance = 0.03, scale = 1)
  # Ethnicity is taken before minority status where it is recorded, and a
  # group of fewer than five is merged with the other small ones.
  data$ethnicity <- sample(c("A", "B", "C"), nrow(data), replace = TRUE)
  data$ethnicity[1:3] <- "D"
  data$ethnicity[4:6] <- "E"
  third <- fit_stage1_unit(data, "linear", tier = 3L)
  expect_equal(third$ethnicity_measure, "ethnicity")
  expect_equal(third$status, "fitted")

  # Three rural rows are too few for a group: urbanicity does not vary.
  data$urbanicity <- c(rep("Rural", 3), rep("Urban", nrow(data) - 3))
  third <- fit_stage1_unit(data, "linear", tier = 3L)
  expect_equal(third$status, "no urbanicity measure that varies in the unit")

  # Each covariate known for most rows, but few rows with all three.
  data <- tier_unit()
  data$ses_household[1:800] <- NA
  data$minority[801:1600] <- NA
  data$urbanicity[1601:2400] <- NA
  third <- fit_stage1_unit(data, "linear", tier = 3L)
  expect_equal(third$status, "too few rows have all of the tier's covariates")
  expect_equal(third$n_no_covariate, 2400)
})

test_that("a covariate that stops varying, repeats another or separates is said", {
  # Every rural row is of unknown sex, so urbanicity has one group left.
  data <- tier_unit()
  data$urbanicity <- c(rep("Rural", 100), rep("Urban", nrow(data) - 100))
  data$sex[1:100] <- NA
  third <- fit_stage1_unit(data, "linear", tier = 3L)
  expect_equal(third$status, "a tier covariate does not vary in the rows left")

  # Minority status repeats household SES: the fit leaves it out, and says.
  data <- tier_unit()
  data$minority <- ifelse(
    data$ses_household == "Low",
    "Minority",
    "Non-minority"
  )
  data$ses_household <- ifelse(data$ses_household == "Low", "Low", "Other")
  third <- fit_stage1_unit(data, "linear", tier = 3L)
  expect_equal(third$status, "fitted")
  expect_match(third$terms_dropped, "minority")
  expect_true(is.na(fit_stage1_unit(data, "linear", tier = 2L)$terms_dropped))

  # A binary outcome that never occurs in one SES group.
  data <- tier_unit()
  data$y <- as.numeric(data$y > stats::median(data$y))
  expect_false(
    fit_stage1_unit(data, "logistic", tier = 2L)$covariate_separation
  )
  data$y[data$ses_household == "Low"] <- 0
  expect_true(fit_stage1_unit(data, "logistic", tier = 2L)$covariate_separation)
  expect_true(is.na(
    fit_stage1_unit(data, "linear", tier = 2L)$covariate_separation
  ))
})

test_that("small groups of a covariate are merged, or left out", {
  values <- c(rep("a", 50), rep("b", 40), rep("c", 3), rep("d", 4), NA)
  groups <- stage1_covariate_groups(values)
  expect_equal(sum(groups %in% "(small groups)"), 7)
  values <- c(rep("a", 50), rep("b", 40), rep("c", 3), NA)
  groups <- stage1_covariate_groups(values)
  expect_equal(sum(is.na(groups)), 4)
  expect_null(stage1_covariate_groups(c(rep("a", 50), rep("b", 3))))
  expect_null(stage1_covariate_groups(c(rep("a", 5), rep("b", 5), rep(NA, 10))))
  expect_null(stage1_covariate_groups(NULL))
})

test_that("stage 1 fits each cell at each tier, from the prepared tables", {
  reg <- engine_registries()
  # The mock records no socioeconomic measure, so one is added that both
  # screen use and the outcome follow.
  edit <- function(rows) {
    set.seed(41)
    level <- sample(1:3, nrow(rows), replace = TRUE)
    rows$ses_household <- c("Low", "Medium", "High")[level]
    rows$st_game <- rows$st_game + level
    rows$mh_dep_self <- rows$mh_dep_self + 1.5 * level
    rows
  }
  prepared <- engine_prepare("9995", reg, edit)
  fits <- fit_stage1(
    prepared,
    engine_cells[1, ],
    reg$outcomes,
    reg$schema,
    tiers = 1:3
  )
  expect_equal(sort(unique(fits$tier)), 1:3)
  expect_equal(nrow(fits), 3 * dplyr::n_distinct(fits$unit))
  one <- fit_stage1(prepared, engine_cells[1, ], reg$outcomes, reg$schema)
  expect_equal(fits[fits$tier == 1, ], one)
  second <- fits[fits$tier == 2, ]
  expect_true(all(second$status == "fitted"))
  expect_true(all(second$ses_measure == "ses_household"))
  # Adjusting takes out what SES added to the slope.
  expect_true(all(second$estimate < one$estimate - 0.1))
  expect_equal(second$tier1_same_rows_estimate, one$estimate)
  expect_true(all(
    fits$status[fits$tier == 3] ==
      "no ethnicity or minority measure that varies in the unit"
  ))
})

test_that("a further tier is set beside tier 1 on its own rows", {
  slopes <- function(tier, estimate, datasets, same_rows = NA_real_) {
    tibble::tibble(
      dataset_id = rep(datasets, each = 2),
      unit = paste(rep(datasets, each = 2), 1:2),
      exposure = "st_watch",
      outcome = "o",
      score_type = "continuous",
      effect_measure = "sd_per_hour",
      tier = tier,
      estimate = estimate,
      se = 0.01,
      tier1_same_rows_estimate = same_rows,
      tier1_same_rows_se = 0.01,
      n = 90,
      n_no_covariate = 10,
      status = "fitted"
    )
  }
  # Tier 1 over all rows is 0.10 in datasets a to d and 0.30 in e and f.
  # Tier 2 is 0.06 in a to d, where tier 1 on the tier's rows is 0.12; it
  # cannot be fitted in e, f, or the second unit of d.
  first <- dplyr::bind_rows(
    slopes(1L, 0.10, c("a", "b", "c", "d")),
    slopes(1L, 0.30, c("e", "f"))
  )
  further <- dplyr::bind_rows(
    slopes(2L, 0.06, c("a", "b", "c", "d"), same_rows = 0.12),
    dplyr::mutate(
      slopes(2L, NA_real_, c("e", "f")),
      status = "no socioeconomic measure that varies in the unit"
    )
  )
  further$status[
    further$unit == "d 2"
  ] <- "fewer rows than the minimum unit size"
  further$estimate[further$unit == "a 1"] <- 0.02
  out <- compare_stage2_tiers(first, further)
  expect_equal(nrow(out), 1)
  expect_equal(out$tier, 2L)
  expect_equal(out$k_datasets, 4)
  expect_equal(out$k_units, 7)
  # Against tier 1 on the tier's rows (0.12), not on all rows (0.10) or in
  # all six datasets.
  expect_equal(out$tier1_estimate, 0.12)
  expect_equal(out$change, out$estimate - 0.12)
  expect_equal(out$change_unit_median, -0.06)
  expect_equal(out$change_unit_q25, -0.06)
  expect_equal(out$rows_lost_share, 0.1)
  # Four units in e and f, and one in d.
  expect_equal(out$k_units_lost, 5L)
  expect_equal(nrow(compare_stage2_tiers(first, further[0, ])), 0)

  # A cell the tier cannot pool is reported, with no change.
  few <- further[further$dataset_id %in% c("a", "b"), ]
  thin <- compare_stage2_tiers(first, few)
  expect_equal(thin$status, "insufficient data")
  expect_true(is.na(thin$change))
})

test_that("cells are counted from the rows that have both", {
  n <- 100
  prepared <- list(
    base = tibble::tibble(
      dataset_id = "9001",
      participant_id = c(1:50, 1:50),
      unit = rep(c("9001|1|X", "9001|2|X"), each = 50),
      ses_household = rep(c("Low", "High"), 50),
      ethnicity = NA_character_,
      urbanicity = c(rep("Urban", 50), rep(c("Urban", "Rural"), 25))
    ),
    exposures = tibble::tibble(
      st_watch = c(rep(1, 40), rep(NA, 10), rep(1, 50)),
      st_game = NA_real_,
      total_screen = 2,
      total_source = "summed",
      st_archived = 1
    ),
    outcomes = tibble::tibble(
      core_row = c(rep(NA, 10), rep(1, 90)),
      archive_row = 1,
      empty_row = NA_real_
    )
  )
  registry <- list(
    rows = tibble::tibble(
      variable_name = c("core_row", "archive_row", "empty_row"),
      use = c("core", "archive", "core")
    )
  )
  exposures <- tibble::tibble(
    variable_name = c("st_watch", "st_game", "st_archived"),
    include = c(TRUE, TRUE, FALSE)
  )
  counted <- count_cells(prepared, registry, exposures)
  cells <- counted$cells
  # Only analysed exposures and outcome rows, and only pairs with a row.
  expect_setequal(cells$exposure, c("st_watch", "total_screen"))
  expect_equal(unique(cells$outcome), "core_row")
  watch <- cells[cells$exposure == "st_watch", ]
  expect_equal(watch$n_rows, c(30, 50))
  expect_equal(watch$n_participants, c(30, 50))
  expect_equal(cells$n_rows[cells$exposure == "total_screen"], c(40, 50))
  expect_equal(unique(cells$dataset_id), "9001")
  # What each tier would use: SES in both units, no ethnicity, and
  # urbanicity only where it varies.
  expect_equal(counted$units$rows, c(50, 50))
  expect_equal(counted$units$ses_measure, rep("ses_household", 2))
  expect_true(all(is.na(counted$units$ethnicity_measure)))
  expect_equal(counted$units$urbanicity_measure, c(NA, "urbanicity"))

  # A participant with several rows in a unit is one participant.
  prepared$base$unit <- "9001|1|X"
  again <- count_cells(prepared, registry, exposures)$cells
  watch <- again[again$exposure == "st_watch", ]
  expect_equal(watch$n_rows, 80)
  expect_equal(watch$n_participants, 50)
  expect_null(count_cells(NULL, registry, exposures))
})

test_that("feasibility counts datasets, units and participants per level", {
  registry <- list(
    tree = tibble::tibble(
      node = c("fam", "n1", "n2"),
      parent = c(NA, "fam", "fam"),
      level = c("L0", "L1", "L1")
    ),
    rows = tibble::tibble(
      variable_name = c("a_self", "a_parent", "b_self", "alone"),
      node = c("n1", "n1", "n2", NA),
      informant = c("self", "parent", "self", "self"),
      direction = "higher_better",
      synthesis = c(TRUE, TRUE, TRUE, FALSE)
    )
  )
  exposures <- tibble::tibble(
    variable_name = "st_watch",
    exposure = "watch",
    day = "usual",
    headline = TRUE
  )
  cell <- function(dataset, wave, outcome, n) {
    tibble::tibble(
      dataset_id = dataset,
      unit = paste(dataset, wave, "X", sep = "|"),
      exposure = "st_watch",
      outcome = outcome,
      n_rows = n,
      n_participants = n
    )
  }
  cells <- dplyr::bind_rows(
    # Four datasets give `a_self`; d1 in two waves of the same children.
    cell("d1", 1, "a_self", 100),
    cell("d1", 2, "a_self", 80),
    cell("d2", 1, "a_self", 200),
    cell("d3", 1, "a_self", 300),
    cell("d4", 1, "a_self", 400),
    # Two give `a_parent`; one unit is too small to count.
    cell("d1", 1, "a_parent", 90),
    cell("d2", 1, "a_parent", 10),
    cell("d5", 1, "a_parent", 500),
    # One gives `b_self`, one the row reported on its own.
    cell("d1", 1, "b_self", 100),
    cell("d1", 1, "alone", 100),
    # Not signed off: not counted.
    cell("d9", 1, "a_self", 900)
  )
  ids <- unique(cells$unit)
  units <- tibble::tibble(
    unit = ids,
    country = "X",
    year = c(2000, 2001, 2005, 2010, 2012, 2015, 2020),
    measure_type = "Categorical",
    responder = c(rep("Self", 4), rep("Parent or carer", 3))
  )
  measures <- tibble::tibble(
    unit = ids,
    ses_measure = c(
      "ses_household",
      NA,
      "ses_household",
      "ses_area",
      NA,
      NA,
      NA
    ),
    ethnicity_measure = c("ethnicity", NA, NA, "minority", NA, NA, NA),
    urbanicity_measure = c("urbanicity", NA, NA, NA, NA, NA, NA)
  )
  datasets <- tibble::tibble(
    dataset_id = c("d1", "d2", "d3", "d4", "d5", "d9"),
    signed_off = c(TRUE, TRUE, TRUE, TRUE, TRUE, NA)
  )
  out <- summarise_feasibility(
    cells,
    measures,
    units,
    registry,
    exposures,
    datasets
  )
  expect_equal(out$scope$datasets_with_cells, 6)
  expect_equal(out$scope$datasets_signed_off, 5)
  level <- function(name) out$levels[out$levels$level == name, ]

  row <- level("L3")
  a_self <- row[row$outcome == "a_self", ]
  expect_equal(a_self$k_datasets, 4)
  expect_equal(a_self$k_units, 5)
  # d1's two waves hold the same children: its larger wave counts.
  expect_equal(a_self$participants, 100 + 200 + 300 + 400)
  expect_true(a_self$poolable)
  a_parent <- row[row$outcome == "a_parent", ]
  expect_equal(a_parent$k_datasets, 2)
  expect_false(a_parent$poolable)
  # The row reported on its own is counted as a row and pools nowhere.
  expect_true("alone" %in% row$outcome)

  n1 <- level("L1")[level("L1")$node == "n1", ]
  expect_equal(n1$k_datasets, 5)
  expect_equal(n1$k_rows, 2)
  expect_equal(n1$participants, 100 + 200 + 300 + 400 + 500)
  expect_equal(nrow(level("L2")), 3)
  family <- level("L0")
  expect_equal(family$k_datasets, 5)
  expect_equal(family$k_nodes_poolable, 1)
  expect_equal(family$exposure_group, "watch")

  tiers <- out$tiers[out$tiers$node == "n1", ]
  expect_equal(
    c(tiers$k_datasets_tier1, tiers$k_datasets_tier2, tiers$k_datasets_tier3),
    c(5, 3, 1)
  )
  # Informant: four datasets by the child, two by a parent.
  informant <- out$moderators[
    out$moderators$node == "n1" & out$moderators$moderator == "informant",
  ]
  expect_equal(informant$k_datasets[order(informant$level)], c(2, 4))
  expect_false(any(informant$comparable))
  years <- out$years[out$years$node == "n1", ]
  expect_equal(c(years$first_year, years$last_year), c(2000, 2015))
  expect_false(years$trend_can_run)
})
