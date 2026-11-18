# Multiple imputation (ANALYSIS_PLAN.md, Step 8): what is imputed and what
# is left alone, that an imputation can be made again from its seed, that
# it removes a bias complete cases have without weakening a slope, and
# Rubin's rules.

skip_if_not_installed("mice")
skip_if_not_installed("fixest")

imputation_registries <- function() {
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
imputation_mock <- make_mock_analysis_data(n_per_wave = 1500)
imputation_prepare <- function(id, reg, edit = identity) {
  rows <- imputation_mock$data[imputation_mock$data$dataset_id == id, ]
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
imputation_settings <- function() {
  analysis_settings()
}
fit_imputed <- function(prepared, imputed, cells, reg, m, ...) {
  fit_stage1_imputed(
    prepared,
    imputed,
    review_imputation(prepared, imputed, reg$outcomes, reg$exposures),
    seq_len(m),
    cells,
    reg$outcomes,
    reg$schema,
    ...
  )
}
impute_all <- function(prepared, reg, m = 3L) {
  bind_imputations(lapply(seq_len(m), function(index) {
    impute_dataset(prepared, reg$outcomes, reg$exposures, index)
  }))
}

test_that("Rubin's rules match a worked example and mice", {
  estimate <- c(0.10, 0.14, 0.09, 0.12)
  se <- c(0.030, 0.032, 0.029, 0.031)
  out <- pool_rubin(estimate, se, rep(200, 4))
  within <- mean(se^2)
  between <- stats::var(estimate)
  expect_equal(out$estimate, mean(estimate))
  expect_equal(out$se, sqrt(within + 1.25 * between))
  expect_equal(out$riv, 1.25 * between / within)
  expect_equal(out$m, 4L)
  reference <- mice::pool.scalar(estimate, se^2, n = 202, k = 2)
  expect_equal(out$se^2, reference$t)
  expect_equal(out$df, reference$df)
  expect_equal(out$riv, reference$r)
  expect_equal(out$fmi, reference$fmi)
  # The pooled degrees of freedom never exceed a complete-data fit's.
  expect_lt(out$df, 200)

  # Imputations that agree add nothing; one imputation is passed through.
  same <- pool_rubin(rep(0.1, 3), rep(0.03, 3), rep(50, 3))
  expect_equal(c(same$se, same$df, same$riv, same$fmi), c(0.03, 50, 0, 0))
  one <- pool_rubin(0.1, 0.03, 50)
  expect_equal(c(one$estimate, one$se, one$df), c(0.1, 0.03, 50))
  expect_error(pool_rubin(c(0.1, NA), c(0.03, 0.03), c(50, 50)), "needs")
})

test_that("only sporadic gaps in covariates are imputed, unit by unit", {
  reg <- imputation_registries()
  edit <- function(rows) {
    set.seed(71)
    n <- nrow(rows)
    rows$st_socmedia <- NA_real_
    rows$sex[sample(n, 100)] <- NA
    rows$sex[1:3] <- "Other"
    rows$mh_dep_self[sample(n, 200)] <- NA
    rows$urbanicity <- sample(c("Urban", "Rural"), n, replace = TRUE)
    rows$urbanicity[sample(n, 200)] <- NA
    # Known for a third of the rows: not sporadic, so left alone.
    rows$ses_household <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
    rows$ses_household[sample(n, 1000)] <- NA
    # Never asked of the nine year olds, and asked of everyone else.
    rows$parental_education <- sample(
      c("Secondary school or less", "Bachelor degree or higher"),
      n,
      replace = TRUE
    )
    rows$parental_education[floor(rows$age_years) == 9] <- NA
    rows$parental_education[sample(which(floor(rows$age_years) > 9), 60)] <- NA
    rows
  }
  prepared <- imputation_prepare("9995", reg, edit)
  first <- impute_dataset(prepared, reg$outcomes, reg$exposures, 1L)
  again <- impute_dataset(prepared, reg$outcomes, reg$exposures, 1L)
  second <- impute_dataset(prepared, reg$outcomes, reg$exposures, 2L)
  # The same number gives the same imputation, and another number another.
  expect_equal(again, first)
  expect_equal(second$cells$row, first$cells$row)
  expect_false(all(second$cells$value == first$cells$value))

  # Screen use and outcomes are not imputed here.
  expect_setequal(
    first$cells$variable,
    c("sex", "urbanicity", "parental_education")
  )
  cells <- function(name) first$cells[first$cells$variable == name, ]
  expect_equal(nrow(cells("sex")), sum(is.na(prepared$base$sex)))
  # A child recorded as another sex keeps the record and is no donor.
  expect_true(all(cells("sex")$value %in% c("Female", "Male")))
  expect_equal(sum(prepared$base$sex %in% "Other"), 3)
  expect_true(all(cells("urbanicity")$value %in% c("Urban", "Rural")))
  notes <- first$units
  note <- function(name) notes[notes$variable == name, ]
  expect_equal(
    note("ses_household")$status,
    "too little observed in the unit to impute"
  )
  # The nine year olds were never asked, so they are left; the others'
  # gaps are filled.
  nine <- floor(prepared$base$age_years) == 9
  education <- cells("parental_education")
  expect_false(any(nine[education$row]))
  expect_equal(nrow(education), 60)
  expect_equal(note("parental_education")$n_age_group, sum(nine))
  expect_equal(note("parental_education")$status, "imputed")

  filled <- apply_imputation(prepared, first, 1L)
  expect_equal(sum(is.na(filled$base$sex)), 0)
  expect_equal(sum(is.na(filled$base$urbanicity)), 0)
  expect_equal(sum(is.na(filled$base$ses_household)), 1000)
  expect_equal(sum(is.na(filled$base$parental_education)), sum(nine))
  expect_equal(filled$exposures, prepared$exposures)
  expect_equal(filled$outcomes, prepared$outcomes)
  both <- bind_imputations(list(first, second, NULL))
  expect_equal(apply_imputation(prepared, both, 1L), filled)
  expect_equal(bind_imputations(first)$cells, first$cells)
  expect_error(apply_imputation(filled, first, 1L), "already has a value")

  # The checks pass, and fail when an imputed value is tampered with.
  review <- review_imputation(prepared, both, reg$outcomes, reg$exposures)
  expect_setequal(review$levels$source, c("observed", "imputed"))
  expect_equal(unique(review$units$imputations), 2L)
  tamper <- function(change, message) {
    tampered <- both
    tampered$cells <- change(tampered$cells)
    expect_error(
      review_imputation(prepared, tampered, reg$outcomes, reg$exposures),
      message
    )
  }
  tamper(
    function(cells) {
      cells$value[cells$variable == "urbanicity"][1] <- "Suburban"
      cells
    },
    "category its unit did not observe"
  )
  tamper(
    function(cells) {
      cells$row[1] <- which(!is.na(prepared$base[[cells$variable[1]]]))[1]
      cells
    },
    "was imputed over"
  )
  tamper(
    function(cells) {
      at <- which(cells$variable == "parental_education")[1]
      cells$row[at] <- which(nine)[1]
      cells
    },
    "age group that observed too little"
  )
  # Cells lost from an imputation, or a whole imputation lost, no longer
  # match what the imputations recorded filling.
  tamper(
    function(cells) cells[-1, ],
    "not the ones the imputations recorded"
  )
  tamper(
    function(cells) cells[cells$imputation != 2, ],
    "not the ones the imputations recorded"
  )
  # Two imputations may legitimately be the same, so that is not an error.
  same <- both
  same$cells$value[same$cells$imputation == 2] <-
    same$cells$value[same$cells$imputation == 1]
  expect_no_error(
    review_imputation(prepared, same, reg$outcomes, reg$exposures)
  )
  # A model is fitted only with the review in hand, and only on
  # imputations that were made.
  one_cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  expect_error(
    fit_stage1_imputed(
      prepared,
      both,
      NULL,
      1:2,
      one_cell,
      reg$outcomes,
      reg$schema
    ),
    "needs the dataset's own `review_imputation\\(\\)`"
  )
  expect_error(
    fit_stage1_imputed(
      prepared,
      both,
      review,
      1:3,
      one_cell,
      reg$outcomes,
      reg$schema
    ),
    "imputation 3 is missing"
  )
  # Another dataset's review will not do.
  elsewhere <- review
  elsewhere$dataset_id <- "9993"
  expect_error(
    fit_stage1_imputed(
      prepared,
      both,
      elsewhere,
      1:2,
      one_cell,
      reg$outcomes,
      reg$schema
    ),
    "needs the dataset's own"
  )
  # No imputation at all is not the same as nothing to fill.
  expect_error(
    fit_stage1_imputed(
      prepared,
      bind_imputations(list()),
      review,
      1:2,
      one_cell,
      reg$outcomes,
      reg$schema
    ),
    "imputation 1, 2 is missing"
  )
  expect_equal(both$made, 1:2)
  fitted <- fit_stage1_imputed(
    prepared,
    both,
    review,
    1:2,
    one_cell,
    reg$outcomes,
    reg$schema,
    tiers = 1:2
  )
  # Each adjusted slope says what share of its rows had the covariate
  # imputed; here no socioeconomic measure passes, so the tier is not
  # fitted and has none.
  expect_true(all(is.na(fitted$covariate_imputed_share[fitted$tier == 1])))
  expect_error(
    impute_dataset(prepared, reg$outcomes, reg$exposures, 1L, list()),
    "needs a `seed`"
  )
})

test_that("screen use is imputed for an outcome's own model, where sporadic", {
  set.seed(83)
  n <- 2000
  data <- data.frame(
    age = stats::runif(n, 10, 14),
    sex = sample(c("Female", "Male"), n, replace = TRUE),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_,
    stringsAsFactors = FALSE
  )
  for (name in bp_stage1_covariates()) {
    data[[name]] <- NA_character_
  }
  data$x <- sample(c(0.5, 1.5, 2.5, 4), n, replace = TRUE)
  data$y <- 0.2 * data$x + stats::rnorm(n)
  whole <- data
  gaps <- sample(n, 500)
  data$x[gaps] <- NA
  settings <- imputation_settings()
  filled <- impute_cell_exposure(data, 11L, settings)
  expect_equal(attr(filled, "imputed"), 500L)
  expect_false(anyNA(filled$x))
  expect_equal(filled$x[-gaps], data$x[-gaps])
  # Drawn from the values the rows observed, and the same from one seed.
  expect_true(all(filled$x[gaps] %in% c(0.5, 1.5, 2.5, 4)))
  expect_equal(impute_cell_exposure(data, 11L, settings), filled)
  expect_false(identical(impute_cell_exposure(data, 12L, settings)$x, filled$x))
  # The imputed rows keep the relation to the outcome.
  slope <- function(rows) unname(stats::coef(stats::lm(y ~ x, rows))["x"])
  expect_equal(slope(filled[gaps, ]), slope(whole), tolerance = 0.06, scale = 1)

  # A covariate that repeats another does not stop the imputation.
  repeated <- data
  repeated$ethnicity <- sample(c("A", "B", "C"), n, replace = TRUE)
  repeated$minority <- ifelse(repeated$ethnicity == "A", "No", "Yes")
  expect_equal(
    attr(impute_cell_exposure(repeated, 11L, settings), "imputed"),
    500L
  )

  # Never asked of the ten year olds: they are left without it.
  by_age <- whole
  ten <- floor(by_age$age) == 10
  by_age$x[ten] <- NA
  by_age$x[sample(which(!ten), 100)] <- NA
  left <- impute_cell_exposure(by_age, 11L, settings)
  expect_equal(attr(left, "imputed"), 100L)
  expect_true(all(is.na(left$x[ten])))
  expect_false(anyNA(left$x[!ten]))

  # Known for too few of the rows: nothing is imputed.
  sparse <- whole
  sparse$x[sample(n, 1200)] <- NA
  expect_equal(attr(impute_cell_exposure(sparse, 11L, settings), "imputed"), 0L)
  expect_equal(impute_cell_exposure(sparse, 11L, settings)$x, sparse$x)
  expect_equal(attr(impute_cell_exposure(whole, 11L, settings), "imputed"), 0L)

  # With the rest of the child's screen use, the two are imputed together.
  two <- whole
  two$other <- pmax(3 - two$x + stats::rnorm(n, 0, 0.5), 0)
  two$x[gaps] <- NA
  two$other[gaps] <- NA
  joint <- impute_cell_exposure(two, 11L, settings)
  expect_false(anyNA(joint$x) || anyNA(joint$other))
  expect_lt(stats::cor(joint$x[gaps], joint$other[gaps]), -0.3)
})

test_that("what a model adjusts for is in its exposure's imputation", {
  settings <- imputation_settings()
  n <- 3000
  frame <- function() {
    data <- data.frame(
      age = stats::runif(n, 10, 14),
      sex = sample(c("Female", "Male"), n, replace = TRUE),
      weight = NA_real_,
      cluster = NA_character_,
      family = NA_character_,
      stringsAsFactors = FALSE
    )
    for (name in bp_stage1_covariates()) {
      data[[name]] <- NA_character_
    }
    data
  }
  # The mean slope over ten imputations, each fitted as the model is.
  imputed_slope <- function(data, tier = 1L, extra = character()) {
    mean(vapply(
      1:10,
      function(seed) {
        filled <- impute_cell_exposure(data, seed, settings)
        filled <- filled[!is.na(filled$x), ]
        fit_stage1_unit(filled, "linear", settings, tier, extra)$estimate
      },
      numeric(1)
    ))
  }

  # A covariate that confounds, with a few gaps of its own: it stays in
  # the exposure's model, so the adjusted slope is the one with nothing
  # missing and not the unadjusted one.
  set.seed(97)
  data <- frame()
  level <- sample(1:3, n, replace = TRUE)
  data$ses_household <- c("Low", "Medium", "High")[level]
  data$x <- pmax(stats::rnorm(n, 1 + level), 0)
  data$y <- -0.10 * data$x - 0.6 * level + stats::rnorm(n)
  data$ses_household[sample(n, 150)] <- NA
  whole <- fit_stage1_unit(data, "linear", settings, 2L)$estimate
  unadjusted <- fit_stage1_unit(data, "linear", settings, 1L)$estimate
  expect_gt(abs(unadjusted - whole), 0.2)
  gaps <- data
  gaps$x[sample(n, 750)] <- NA
  expect_lt(abs(imputed_slope(gaps, tier = 2L) / whole - 1), 0.08)

  # The rest of the child's screen use, known for every row: the exposure
  # is drawn given it, so the slope adjusted for it is kept.
  set.seed(101)
  data <- frame()
  shared <- stats::rgamma(n, 2, 1)
  data$x <- shared + stats::rgamma(n, 1, 1)
  data$other <- 0.8 * shared + stats::rgamma(n, 1, 1)
  data$y <- -0.10 * data$x - 0.10 * data$other + stats::rnorm(n)
  whole <- fit_stage1_unit(data, "linear", settings, 1L, "other")$estimate
  gaps <- data
  gaps$x[sample(n, 750)] <- NA
  expect_lt(abs(imputed_slope(gaps, extra = "other") / whole - 1), 0.08)
  # A row whose rest of screen use is missing and cannot be imputed is
  # left alone: the model drops it.
  never <- gaps
  ten <- floor(never$age) == 10
  never$other[ten] <- NA
  filled <- impute_cell_exposure(never, 3L, settings)
  expect_equal(filled$x[ten], never$x[ten])
  expect_false(anyNA(filled$x[!ten]))

  # One age group cannot be imputed and the rest of screen use has gaps
  # too: the other ages are still imputed, and nothing fails.
  mixed <- data
  ten <- floor(mixed$age) == 10
  mixed$x[ten & seq_len(n) %% 2 == 0] <- NA
  mixed$x[sample(which(!ten), 300)] <- NA
  mixed$other[sample(which(!ten), 200)] <- NA
  filled <- impute_cell_exposure(mixed, 7L, settings)
  expect_null(attr(filled, "failed"))
  expect_false(anyNA(filled$x[!ten]) || anyNA(filled$other[!ten]))
  expect_equal(filled$x[ten], mixed$x[ten])
  # The rest of screen use alone has gaps: it is imputed given the exposure.
  rest_only <- data
  rest_only$other[sample(n, 300)] <- NA
  filled <- impute_cell_exposure(rest_only, 7L, settings)
  expect_false(anyNA(filled$other))
  expect_equal(attr(filled, "imputed"), 0L)

  # A binary outcome: the slope keeps most of the precision of complete
  # cases, which it would not if a few donors supplied most of the draws.
  set.seed(103)
  data <- frame()
  data$x <- sample(c(0.5, 1.5, 2.5, 3.5, 5), n, replace = TRUE)
  data$y <- stats::rbinom(n, 1, stats::plogis(-2.3 + 0.25 * data$x))
  gaps <- data
  gaps$x[sample(n, 750)] <- NA
  filled <- impute_cell_exposure(gaps, 5L, settings)
  drawn <- table(filled$x[is.na(gaps$x)])
  expect_length(drawn, 5)
  expect_lt(max(drawn) / sum(drawn), 0.4)
})

test_that("an imputed exposure never makes a model fittable", {
  # The exposure is imputed for linear models only (DR-0019); the sizes a
  # model needs are checked here as if every kind were imputed.
  expect_equal(imputation_settings()$impute_exposure_models, "linear")
  settings <- imputation_settings()
  settings$impute_exposure_models <- c("linear", "logistic", "ordinal")
  rows <- data.frame(x = c(rep(1, 50), rep(NA, 50)), y = rep(0:1, 50))
  expect_true(stage1_may_impute(rows, "logistic", settings))
  # Nine of one class among the rows that have the exposure.
  rare <- data.frame(
    x = c(rep(1, 59), rep(NA, 41)),
    y = c(rep(1, 9), rep(0, 50), rep(1, 41))
  )
  expect_false(stage1_may_impute(rare, "logistic", settings))
  levels <- data.frame(
    x = c(rep(1, 60), rep(NA, 40)),
    y = c(rep(1, 30), rep(2, 26), rep(3, 4), rep(3, 40))
  )
  expect_false(stage1_may_impute(levels, "ordinal", settings))
  # A linear model needs the minimum unit size among the rows that have
  # the exposure and that the model would keep: here 31 have it, and three
  # of those are dropped for a sex the model does not use.
  frame <- function(n) {
    data <- data.frame(
      age = seq(10, 12, length.out = n),
      sex = rep(c("Female", "Male"), length.out = n),
      weight = NA_real_,
      cluster = NA_character_,
      family = NA_character_,
      y = seq_len(n) %% 7,
      stringsAsFactors = FALSE
    )
    for (name in bp_stage1_covariates()) {
      data[[name]] <- NA_character_
    }
    data
  }
  small <- frame(44)
  small$x <- c(rep(c(1, 2, 3), length.out = 31), rep(NA, 13))
  expect_true(stage1_may_impute(small, "linear", settings))
  small$sex[1:3] <- "Other"
  expect_false(stage1_may_impute(small, "linear", settings))
  # A model adjusted for the rest of screen use needs that too on the
  # rows counted, since it is imputed with the exposure.
  rest <- frame(44)
  rest$x <- c(rep(c(1, 2, 3), length.out = 34), rep(NA, 10))
  rest$other <- c(rep(NA, 10), rep(1, 34))
  expect_false(stage1_may_impute(rest, "linear", settings))
  rest$other <- 1
  expect_true(stage1_may_impute(rest, "linear", settings))

  # The settings say which kinds of model have their exposure imputed.
  linear_only <- settings
  linear_only$impute_exposure_models <- "linear"
  expect_false(stage1_may_impute(rows, "logistic", linear_only))
  enough <- frame(44)
  enough$x <- c(rep(c(1, 2, 3), length.out = 31), rep(NA, 13))
  expect_true(stage1_may_impute(enough, "linear", linear_only))
})

test_that("imputation removes the bias of complete cases", {
  reg <- imputation_registries()
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  # Gaming is missing more often for children with high scores, so the
  # children left are a selected group and their slope is too small.
  edit <- function(rows) {
    set.seed(73)
    rows$st_socmedia <- NA_real_
    chance <- stats::plogis(-1.7 + 1.5 * as.numeric(scale(rows$mh_dep_self)))
    rows$st_game[stats::runif(nrow(rows)) < chance] <- NA
    rows
  }
  whole <- fit_stage1(
    imputation_prepare("9995", reg, function(rows) {
      rows$st_socmedia <- NA_real_
      rows
    }),
    cell,
    reg$outcomes,
    reg$schema
  )
  prepared <- imputation_prepare("9995", reg, edit)
  share <- mean(is.na(prepared$exposures$st_game))
  expect_gt(share, 0.15)
  expect_lt(share, 0.3)
  complete <- fit_stage1(prepared, cell, reg$outcomes, reg$schema)
  imputed <- impute_all(prepared, reg, m = 10L)
  pooled <- fit_imputed(prepared, imputed, cell, reg, 10L)
  expect_equal(pooled$status, "fitted")
  expect_equal(pooled$m, 10L)
  expect_equal(pooled$n, whole$n)
  expect_equal(pooled$n_exposure_imputed, whole$n - complete$n)
  expect_gt(pooled$fmi, 0.05)
  expect_lt(pooled$fmi, 0.9)
  # Complete cases miss the slope of the whole sample; imputation is
  # closer, and its interval covers it.
  expect_gt(abs(complete$estimate - whole$estimate), 0.02)
  expect_lt(
    abs(pooled$estimate - whole$estimate),
    abs(complete$estimate - whole$estimate)
  )
  expect_lt(abs(pooled$estimate - whole$estimate), 2 * pooled$se)
  expect_gt(pooled$se, whole$se)
  expect_false(pooled$design_varied)
  # The same again from the same seed.
  expect_equal(fit_imputed(prepared, imputed, cell, reg, 10L), pooled)

  # Too much of the exposure missing: it is not imputed, and the model is
  # the complete-case one.
  sparse <- imputation_prepare("9995", reg, function(rows) {
    set.seed(75)
    rows$st_socmedia <- NA_real_
    rows$st_game[sample(nrow(rows), 0.5 * nrow(rows))] <- NA
    rows
  })
  plain <- fit_stage1(sparse, cell, reg$outcomes, reg$schema)
  kept <- fit_imputed(sparse, impute_all(sparse, reg, 2L), cell, reg, 2L)
  expect_equal(kept$n_exposure_imputed, 0)
  expect_equal(kept$estimate, plain$estimate)
  expect_equal(kept$fmi, 0)
})

test_that("a slope is not weakened where its outcome differs from its family", {
  reg <- imputation_registries()
  # Three outcomes of one family: gaming is related to the first, and not
  # to the other two, which the family's score is mostly made of.
  edit <- function(rows) {
    set.seed(89)
    n <- nrow(rows)
    rows$st_socmedia <- NA_real_
    rows$mh_dep_self <- 0.25 * rows$st_game + stats::rnorm(n)
    rows$mh_distress_self <- stats::rnorm(n)
    rows$mh_stress_self <- stats::rnorm(n)
    rows
  }
  holes <- function(rows) {
    rows <- edit(rows)
    rows$st_game[sample(nrow(rows), 0.28 * nrow(rows))] <- NA
    rows
  }
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  whole <- fit_stage1(
    imputation_prepare("9995", reg, edit),
    cell,
    reg$outcomes,
    reg$schema
  )
  prepared <- imputation_prepare("9995", reg, holes)
  pooled <- fit_imputed(
    prepared,
    impute_all(prepared, reg, 10L),
    cell,
    reg,
    10L
  )
  expect_equal(pooled$n_exposure_imputed, round(0.28 * 1500))
  # Within a twentieth of the slope with nothing missing.
  expect_lt(abs(pooled$estimate / whole$estimate - 1), 0.05)
})

test_that("imputed covariates bring a tier's rows back", {
  reg <- imputation_registries()
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  edit <- function(rows) {
    set.seed(41)
    rows$st_socmedia <- NA_real_
    level <- sample(1:3, nrow(rows), replace = TRUE)
    rows$ses_household <- c("Low", "Medium", "High")[level]
    rows$st_game <- rows$st_game + level
    rows$mh_dep_self <- rows$mh_dep_self + 1.5 * level
    rows$ses_household[sample(nrow(rows), 300)] <- NA
    rows
  }
  prepared <- imputation_prepare("9995", reg, edit)
  complete <- fit_stage1(prepared, cell, reg$outcomes, reg$schema, tiers = 2L)
  expect_equal(
    complete$status,
    "no socioeconomic measure that varies in the unit"
  )
  pooled <- fit_imputed(
    prepared,
    impute_all(prepared, reg, 5L),
    cell,
    reg,
    5L,
    tiers = 1:2
  )
  second <- pooled[pooled$tier == 2, ]
  expect_equal(second$status, "fitted")
  expect_equal(second$ses_measure, "ses_household")
  expect_equal(second$n, nrow(prepared$base))
  # A fifth of the rows had the measure imputed, and the slope says so.
  expect_equal(second$covariate_imputed_share, 300 / nrow(prepared$base))
  expect_true(is.na(pooled$covariate_imputed_share[pooled$tier == 1]))
  # Adjusting still takes out what SES added to the slope, and the tier 1
  # slope on the same rows is combined over the imputations too.
  first <- pooled[pooled$tier == 1, ]
  expect_lt(second$estimate, first$estimate - 0.1)
  expect_equal(second$tier1_same_rows_estimate, first$estimate)
})

test_that("a child's sex is the same at every wave after imputation", {
  reg <- imputation_registries()
  edit <- function(rows) {
    people <- unique(rows$participant_id)
    # Unknown at both waves for some children, and at one wave for others.
    rows$sex[rows$participant_id %in% people[1:120]] <- NA
    rows$sex[rows$participant_id %in% people[121:200] & rows$wave == "2"] <- NA
    rows
  }
  prepared <- imputation_prepare("9992", reg, edit)
  imputed <- impute_all(prepared, reg, m = 2L)
  for (index in 1:2) {
    filled <- apply_imputation(prepared, imputed, index)
    expect_false(anyNA(filled$base$sex))
    per_child <- tapply(
      filled$base$sex,
      filled$base$participant_id,
      dplyr::n_distinct
    )
    expect_true(all(per_child == 1))
  }
  expect_silent(
    review_imputation(prepared, imputed, reg$outcomes, reg$exposures)
  )
  # Records that look odd but are legitimate do not stop the build: a sex
  # known at one wave and missing at the other, so every imputation is the
  # same; another recorded sex at one wave; and an ethnicity recorded
  # differently at two waves.
  legitimate <- function(edit) {
    data <- imputation_prepare("9992", reg, edit)
    drawn <- impute_all(data, reg, m = 2L)
    expect_no_error(
      review_imputation(data, drawn, reg$outcomes, reg$exposures)
    )
    drawn
  }
  people <- unique(as.character(
    imputation_mock$data$participant_id[
      imputation_mock$data$dataset_id == "9992"
    ]
  ))
  legitimate(function(rows) {
    rows$sex[rows$participant_id %in% people[1:200] & rows$wave == "2"] <- NA
    rows
  })
  other <- legitimate(function(rows) {
    odd <- rows$participant_id %in% people[1:5]
    rows$sex[odd & rows$wave == "1"] <- "Other"
    rows$sex[odd & rows$wave == "2"] <- NA
    rows$sex[rows$participant_id %in% people[6:100] & rows$wave == "2"] <- NA
    rows
  })
  expect_true(all(other$cells$value %in% c("Female", "Male")))
  legitimate(function(rows) {
    set.seed(107)
    rows$ethnicity <- sample(c("White", "Asian"), nrow(rows), replace = TRUE)
    rows$ethnicity[sample(nrow(rows), 200)] <- NA
    rows
  })

  # An imputed sex that contradicts the child's own record is caught.
  one_wave <- imputation_prepare("9992", reg, function(rows) {
    rows$sex[rows$participant_id %in% people[1:200] & rows$wave == "2"] <- NA
    rows
  })
  wrong <- impute_all(one_wave, reg, m = 2L)
  wrong$cells$value[1:50] <- ifelse(
    wrong$cells$value[1:50] == "Female",
    "Male",
    "Female"
  )
  expect_error(
    review_imputation(one_wave, wrong, reg$outcomes, reg$exposures),
    "against the child's own record"
  )

  # Minority status is left alone where the unit records ethnicity.
  both <- imputation_prepare("9992", reg, function(rows) {
    set.seed(79)
    n <- nrow(rows)
    rows$ethnicity <- sample(c("White", "Asian", "Black"), n, replace = TRUE)
    rows$minority <- ifelse(
      rows$ethnicity == "White",
      "Non-minority",
      "Minority"
    )
    rows$minority[sample(n, 200)] <- NA
    rows$ethnicity[sample(n, 150)] <- NA
    rows
  })
  one <- impute_dataset(both, reg$outcomes, reg$exposures, 1L)
  # Nor is it a predictor there, known for every row or not.
  whole <- both
  whole$base$minority[is.na(whole$base$minority)] <- "Minority"
  expect_no_error(impute_dataset(whole, reg$outcomes, reg$exposures, 1L))
  expect_true("ethnicity" %in% one$cells$variable)
  expect_false("minority" %in% one$cells$variable)
  expect_equal(
    unique(one$units$status[one$units$variable == "minority"]),
    "not needed: the unit records ethnicity"
  )

  # In a panel the child's other wave is among the predictors, and a
  # family's score turns each outcome so that higher is better.
  frame <- imputation_frame(prepared, reg$outcomes, reg$exposures)
  expect_true("aux_other_st_game" %in% names(frame$data))
  expect_false(anyNA(frame$data$aux_other_st_game))
  # Depression (higher is worse) and the diagnosis row both enter, turned.
  expect_lt(
    stats::cor(frame$data$aux_mental_health, prepared$outcomes_z$mh_dep_self),
    -0.6
  )
  # The seed of an imputation is never missing.
  expect_false(is.na(imputation_seed(1, "a", 2)))
})
