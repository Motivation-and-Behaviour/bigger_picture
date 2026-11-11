test_that("the mock analysis data use dataschema rows and values", {
  mock <- make_mock_analysis_data(n_per_wave = 100)
  schema <- read_dataschema(file.path(repo_root, bp_dataschema_path()))

  expect_true(all(names(mock$data) %in% schema$variable_name))
  allowed <- stats::setNames(schema$allowed_values, schema$variable_name)
  for (column in names(mock$data)) {
    levels <- allowed[[column]]
    if (is.na(levels) || !nzchar(levels)) {
      next
    }
    observed <- unique(as.character(mock$data[[column]]))
    observed <- observed[!is.na(observed)]
    expect_true(
      all(observed %in% strsplit(levels, "|", fixed = TRUE)[[1]]),
      info = column
    )
  }
})

test_that("the mock datasets have the intended shapes", {
  mock <- make_mock_analysis_data(n_per_wave = 100)
  data <- mock$data
  waves_per_person <- tapply(
    data$wave,
    paste(data$dataset_id, data$participant_id),
    function(w) length(unique(w))
  )
  dataset_of <- sub(" .*", "", names(waves_per_person))
  design <- stats::setNames(mock$designs$design, mock$designs$dataset_id)

  expect_true(all(waves_per_person[design[dataset_of] == "panel"] == 2))
  expect_true(all(waves_per_person[design[dataset_of] == "repeated"] == 1))
  single <- data$dataset_id %in% names(design)[design == "single"]
  expect_true(all(is.na(data$wave[single])))
  expect_equal(
    sort(unique(as.character(data$country[data$dataset_id == "9996"]))),
    c("Ireland", "Scotland")
  )
})

test_that("the mock data are reproducible from the seed", {
  expect_identical(
    make_mock_analysis_data(n_per_wave = 50)$data,
    make_mock_analysis_data(n_per_wave = 50)$data
  )
})

test_that("per-dataset fits recover the known effects", {
  mock <- make_mock_analysis_data()
  data <- mock$data

  # Each dataset's estimate is close to its own effect, and the
  # inverse-variance pool is close to the pooled truth and clearly non-zero;
  # one dataset alone is too noisy to show the effect exists.
  slope_check <- function(outcome, fit_one) {
    truth <- mock$truth[mock$truth$outcome == outcome, ]
    est <- t(vapply(
      seq_len(nrow(truth)),
      function(i) {
        fit_one(data[data$dataset_id == truth$dataset_id[i], ])
      },
      c(estimate = 0, se = 0)
    ))
    expect_true(
      all(abs(est[, "estimate"] - truth$effect) < 4 * est[, "se"]),
      label = paste(outcome, "per dataset")
    )
    w <- 1 / est[, "se"]^2
    pooled <- sum(w * est[, "estimate"]) / sum(w)
    pooled_se <- sqrt(1 / sum(w))
    expect_lt(abs(pooled - sum(w * truth$effect) / sum(w)), 4 * pooled_se)
    expect_gt(pooled / pooled_se, 3)
  }
  coef_se <- function(fit, term) {
    s <- summary(fit)$coefficients
    c(estimate = s[term, 1], se = s[term, 2])
  }

  slope_check("mh_dep_self", function(unit) {
    coef_se(
      stats::lm(mh_dep_self ~ st_game + age_years, data = unit),
      "st_game"
    )
  })
  slope_check("beh_alcohol_self", function(unit) {
    unit$y <- unit$beh_alcohol_self == "TRUE"
    coef_se(
      stats::glm(y ~ st_socmedia, family = stats::binomial, data = unit),
      "st_socmedia"
    )
  })
  slope_check("mh_anx_diagnosed", function(unit) {
    unit$y <- unit$mh_anx_diagnosed == "Currently diagnosed"
    coef_se(
      stats::glm(y ~ st_game, family = stats::binomial, data = unit),
      "st_game"
    )
  })

  skip_if_not_installed("MASS")
  slope_check("wb_selfratedhealth_self", function(unit) {
    # analysis_data's factor levels are in byte order, so an ordinal model
    # takes its level order from the schema's allowed_values instead.
    unit$y <- factor(
      as.character(unit$wb_selfratedhealth_self),
      levels = c("Excellent", "Very good", "Good", "Fair", "Poor"),
      ordered = TRUE
    )
    fit <- MASS::polr(y ~ st_watch, data = unit, Hess = TRUE)
    s <- summary(fit)$coefficients
    c(estimate = s["st_watch", 1], se = s["st_watch", 2])
  })
})
