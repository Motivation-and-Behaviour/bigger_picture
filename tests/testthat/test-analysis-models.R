# The model list and what the confirmatory tiers need beside the engine
# (ANALYSIS_PLAN.md, Step 7 task 10 and Step 9 tasks 3 to 5): the list and
# its cells, the tiered false discovery rate, equivalence tests, the
# adjustment for other screen use and the longitudinal strand.

models_registries <- function() {
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

test_that("tiered q-values match hand-worked examples", {
  # Tier 1: five families, K = 5.
  tier1 <- adjust_tiered_fdr(
    c(0.01, 0.02, 0.03, 0.04, 0.20),
    rep("families", 5)
  )
  expect_equal(tier1$q, c(0.05, 0.05, 0.05, 0.05, 0.20))
  expect_equal(tier1$k_total, rep(5L, 5))

  # Three of the five can be estimated: K stays 5.
  # 0.01 * 5 / 1 = 0.05; 0.03 * 5 / 2 = 0.075; 0.04 * 5 / 3 = 0.0667, which
  # also caps the one before it.
  three <- adjust_tiered_fdr(c(0.01, 0.04, 0.03), rep("families", 3))
  expect_equal(three$q, c(0.05, 0.2 / 3, 0.2 / 3))

  # Tier 2 in one family with two content classes: K = 5 + 2 = 7, and the
  # tiny Tier 1 p-values do not lower its q-values.
  # 0.015 * 7 / 1 = 0.105; 0.04 * 7 / 2 = 0.14.
  classes <- "families > classes:wellbeing"
  specific <- paste0(classes, " > specific:wellbeing:st_watch")
  p <- c(1e-6, 1e-7, 0.015, 0.04, 0.001, 0.01, 0.2, NA)
  path <- c(rep("families", 2), rep(classes, 2), rep(specific, 4))
  out <- adjust_tiered_fdr(p, path)
  expect_equal(out$q[3:4], c(0.105, 0.14))
  expect_equal(out$k_total[3:4], c(7L, 7L))
  # Three specific outcomes under one class: K = 5 + 2 + 3 = 10. The test
  # that could not be run is not counted and gets no q-value.
  # 0.001 * 10 / 1 = 0.01; 0.01 * 10 / 2 = 0.05; 0.2 * 10 / 3 = 0.667.
  expect_equal(out$q[5:8], c(0.01, 0.05, 2 / 3, NA))
  expect_equal(out$k_total[5:8], c(10L, 10L, 10L, NA))

  # A layer with no test adds nothing: K = 5 + 0 + 1.
  alone <- adjust_tiered_fdr(0.02, specific)
  expect_equal(alone$k_total, 6L)
  expect_equal(alone$q, 0.12)
  # A q-value never exceeds 1.
  expect_equal(adjust_tiered_fdr(0.9, specific)$q, 1)

  expect_error(
    adjust_tiered_fdr(rep(0.1, 6), rep("families", 6)),
    "fixed count"
  )
  expect_error(adjust_tiered_fdr(0.1, NA_character_), "needs a path")
  expect_error(adjust_tiered_fdr(1.2, "families"), "\\[0, 1\\]")
})

test_that("a test's path follows its tier, level, family and exposure", {
  path <- function(tier, level, exposure = "st_watch", primary = TRUE) {
    tiered_fdr_path(tier, level, "wellbeing", exposure, primary, "a_model")
  }
  expect_equal(path(1, "L0", "total_screen"), "families")
  expect_equal(
    path(1, "L1", "total_screen"),
    "families > specific:wellbeing:total_screen"
  )
  classes <- "families > classes:wellbeing"
  expect_equal(path(2, "L0"), classes)
  specific <- paste0(classes, " > specific:wellbeing:st_watch")
  expect_equal(path(2, "L1"), specific)
  expect_equal(path(2, "L2"), paste0(specific, " > informants"))
  expect_equal(path(2, "L3"), paste0(specific, " > rows"))
  # A secondary model sits under the primary test at its level.
  expect_equal(path(2, "L0", primary = FALSE), paste0(classes, " > a_model"))
  thresholds <- paste0(classes, " > thresholds:wellbeing")
  expect_equal(path(3, "L0"), thresholds)
  moderation <- paste0(thresholds, " > moderation:wellbeing")
  expect_equal(path(4, "L1"), moderation)
  expect_equal(path(5, "L0"), paste0(moderation, " > exploratory:wellbeing"))
  expect_true(is.na(tiered_fdr_path(2, "L3", NA, "st_watch", TRUE, "m")))
})

test_that("an equivalence test is two one-sided tests against the margin", {
  margin <- 0.10 / 3
  out <- test_equivalence(
    c(0, 0.05, -0.02, NA, 0.01),
    c(0.01, 0.01, 0.01, 0.01, NA),
    c(20, 20, 20, 20, 20),
    margin
  )
  by_hand <- function(estimate) {
    max(
      stats::pt((estimate + margin) / 0.01, 20, lower.tail = FALSE),
      stats::pt((estimate - margin) / 0.01, 20)
    )
  }
  expect_equal(out$p_equivalence[1:3], vapply(c(0, 0.05, -0.02), by_hand, 1))
  # Inside the bounds with room to spare; outside them; inside but too
  # close to the lower bound to say.
  expect_equal(out$equivalent, c(TRUE, FALSE, FALSE, NA, NA))
  expect_gt(out$p_equivalence[2], 0.9)
  # The same as the 90% interval lying inside the bounds.
  half <- stats::qt(0.95, 20) * 0.01
  expect_true(abs(0) + half < margin)
  expect_false(abs(-0.02) + half < margin)
  expect_error(test_equivalence(0, 1, 5, -1), "positive")
})

test_that("the model list is valid, and expands against the registries", {
  reg <- models_registries()
  spec <- read_model_spec(file.path(repo_root, bp_models_path()))
  expect_gt(nrow(spec), 0)
  # Every sensitivity analysis the plan lists is a row, built or not.
  expect_true(all(c("tier1_total", "tier2_content") %in% spec$model_name))
  primary <- spec[spec$model_name %in% c("tier1_total", "tier2_content"), ]
  expect_equal(primary$analysis_class, rep("confirmatory", 2))
  expect_true(all(is.na(primary$variant)))

  hypotheses <- read_hypotheses(
    file.path(repo_root, bp_hypotheses_path()),
    reg$outcomes,
    reg$exposures
  )
  # The preregistration's predictions, family by family in the order
  # learning, cognition, mental health, wellbeing, behaviour.
  predicted <- function(exposure) {
    families <- c(
      "learning",
      "cognition",
      "mental_health",
      "wellbeing",
      "behaviour"
    )
    own <- hypotheses[hypotheses$exposure == exposure, ]
    own$prediction[match(families, own$family)]
  }
  expect_equal(nrow(hypotheses), 25)
  expect_equal(predicted("total_screen"), rep("negative", 5))
  expect_equal(predicted("st_watch"), rep("negative", 5))
  expect_equal(
    predicted("st_educational"),
    c("positive", "positive", "none", "none", "none")
  )
  expect_equal(
    predicted("st_game"),
    c("none", "none", "negative", "negative", "negative")
  )
  expect_equal(
    predicted("st_socmedia"),
    c("none", "none", "negative", "negative", "none")
  )

  development <- read_development_cells(
    file.path(repo_root, bp_development_cells_path()),
    reg$outcomes,
    reg$exposures
  )
  before <- expand_model_cells(spec, reg$outcomes, reg$exposures, development)
  fitted <- dplyr::distinct(before[c("exposure", "outcome")])
  # Before the freeze nothing outside the development cells is fitted, and
  # every development cell is in some model.
  expect_equal(
    nrow(dplyr::anti_join(fitted, development, by = names(fitted))),
    0
  )
  expect_equal(
    nrow(dplyr::anti_join(development, fitted, by = names(fitted))),
    0
  )
  expect_false(bp_total_exposure() %in% before$exposure)

  after <- expand_model_cells(
    spec,
    reg$outcomes,
    reg$exposures,
    development,
    frozen = TRUE
  )
  total <- after[after$model_name == "tier1_total", ]
  expect_equal(unique(total$exposure), bp_total_exposure())
  content <- after[after$model_name == "tier2_content", ]
  expect_setequal(
    unique(content$exposure),
    c("st_watch", "st_game", "st_socmedia", "st_educational")
  )
  rows <- reg$outcomes$rows
  pooled <- rows$variable_name[
    rows$role %in% "outcome" & rows$use %in% "core" & rows$synthesis %in% TRUE
  ]
  expect_setequal(unique(total$outcome), pooled)
  # The primary models share no cell, so a pooled row has one model.
  both <- after[after$model_name %in% c("tier1_total", "tier2_content"), ]
  expect_false(anyDuplicated(both[c("exposure", "outcome")]) > 0)
  # The cross-sectional stage 1 fits the cells of its own models only.
  stage1 <- after[after$model_name %in% bp_stage1_models(), ]
  expect_false(anyDuplicated(stage1[c("exposure", "outcome")]) > 0)
  school <- reg$exposures$variable_name[reg$exposures$context %in% "school"]
  expect_false(any(stage1$exposure %in% school))
  expect_false(any(
    rows$role[match(stage1$outcome, rows$variable_name)] == "negative_control"
  ))
  controls <- after[after$model_name == "negative_controls", ]
  expect_true(all(
    rows$role[match(controls$outcome, rows$variable_name)] == "negative_control"
  ))
})

test_that("a broken model list is refused", {
  good <- readLines(file.path(repo_root, bp_models_path()))
  write <- function(lines) {
    path <- withr::local_tempfile(
      fileext = ".csv",
      .local_envir = parent.frame()
    )
    writeLines(lines, path)
    path
  }
  expect_error(read_model_spec(write(c(good, good[2]))), "repeat")
  expect_error(
    read_model_spec(write(sub("confirmatory", "primary", good[1:2]))),
    "analysis_class"
  )
  expect_error(
    read_model_spec(write(sub(
      ",two_stage,",
      ",magic,",
      good[1:2],
      fixed = TRUE
    ))),
    "engine"
  )
  # An exploratory model cannot sit in a confirmatory tier.
  loose <- sub(",confirmatory,", ",exploratory,", good[1:2], fixed = TRUE)
  expect_error(read_model_spec(write(loose)), "tier 5")
})

skip_if_not_installed("fixest")
skip_if_not_installed("metafor")
skip_if_not_installed("clubSandwich")

models_mock <- make_mock_analysis_data(n_per_wave = 600)
models_prepare <- function(id, reg, edit = identity) {
  rows <- models_mock$data[models_mock$data$dataset_id == id, ]
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
models_truth <- function(id) {
  truth <- models_mock$truth
  truth$effect[truth$dataset_id == id & truth$outcome == "mh_dep_self"]
}
no_breaks <- function(reg) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(paste(bp_series_breaks_columns(), collapse = ","), path)
  read_series_breaks(path, reg$exposures)
}

test_that("adjusting for other screen use removes what it adds to a slope", {
  reg <- models_registries()
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  # Children who watch more also game more, and watching has an effect of
  # its own, so the unadjusted slope on gaming is too large.
  edit <- function(rows) {
    rows$st_socmedia <- NA_real_
    rows$st_game <- rows$st_game + 0.5 * rows$st_watch
    rows$mh_dep_self <- rows$mh_dep_self + 0.3 * rows$st_watch
    rows
  }
  prepared <- models_prepare("9995", reg, edit)
  plain <- fit_stage1(prepared, cell, reg$outcomes, reg$schema)
  adjusted <- fit_stage1(
    prepared,
    cell,
    reg$outcomes,
    reg$schema,
    adjust_other = TRUE
  )
  expect_equal(adjusted$status, "fitted")
  expect_match(adjusted$covariates, "other")
  expect_equal(adjusted$other_from, "summed: st_watch|st_game")
  expect_equal(adjusted$n_no_other, 0L)
  expect_equal(adjusted$tier1_same_rows_estimate, plain$estimate)
  # The outcome is standardised, so the slopes are compared in its SD.
  spread <- stats::sd(
    edit(models_mock$data[models_mock$data$dataset_id == "9995", ])$mh_dep_self
  )
  expect_gt(plain$estimate, adjusted$estimate + 0.05)
  expect_equal(
    adjusted$estimate,
    models_truth("9995") / spread,
    tolerance = 0.5
  )
  expect_lt(abs(adjusted$estimate - models_truth("9995") / spread), 0.06)
  expect_true(is.na(plain$n_no_other))

  # A unit that records no other screen use says so, and the derived total
  # has no rest to adjust for.
  alone <- models_prepare("9995", reg, function(rows) {
    rows$st_watch <- NA_real_
    rows$st_socmedia <- NA_real_
    rows
  })
  none <- fit_stage1(alone, cell, reg$outcomes, reg$schema, adjust_other = TRUE)
  expect_equal(none$status, "no row has other")
  total <- fit_stage1(
    prepared,
    tibble::tibble(exposure = "total_screen", outcome = "mh_dep_self"),
    reg$outcomes,
    reg$schema,
    adjust_other = TRUE
  )
  expect_equal(total$status, "no row has other")
})

test_that("the longitudinal models separate change from standing differences", {
  reg <- models_registries()
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  # Each child has a standing trait that raises both gaming and the score,
  # at both waves; gaming also has the mock's own effect at each wave.
  edit <- function(rows) {
    set.seed(61)
    people <- unique(rows$participant_id)
    trait <- stats::rnorm(length(people))[match(rows$participant_id, people)]
    rows$st_game <- rows$st_game + pmax(trait + 1, 0)
    rows$mh_dep_self <- rows$mh_dep_self + 1.2 * trait
    rows
  }
  prepared <- models_prepare("9992", reg, edit)
  fits <- fit_stage1_longitudinal(
    prepared,
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  expect_equal(fits$model, c("lagged", "matched", "within"))
  expect_true(all(fits$status == "fitted"))
  expect_equal(fits$wave_from, rep("1", 3))
  expect_equal(fits$wave_to, rep("2", 3))
  expect_equal(fits$lag_years, rep(2, 3))
  expect_equal(fits$n_pairs, rep(600L, 3))
  slope <- function(model) fits$estimate[fits$model == model]
  # The cross-sectional slope carries the trait; the change within a child
  # does not, and recovers the mock's effect in the earlier wave's SD.
  rows <- edit(models_mock$data[models_mock$data$dataset_id == "9992", ])
  spread <- stats::sd(rows$mh_dep_self[rows$wave == "1"])
  expect_gt(slope("matched"), 0.25)
  expect_lt(abs(slope("within") - models_truth("9992") / spread), 0.05)
  # The matched model is the cross-sectional model of the earlier wave.
  first <- fit_stage1(prepared, cell, reg$outcomes, reg$schema)
  expect_equal(slope("matched"), first$estimate[first$wave == "1"])
  # Screen use at the first wave says little about the second wave's score
  # once the first wave's score is known.
  expect_lt(abs(slope("lagged")), slope("matched") / 2)
  expect_match(fits$covariates[fits$model == "lagged"], "y0")

  # A change to the screen item between the waves stops the within-child
  # model and leaves the other two.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(bp_series_breaks_columns(), collapse = ","),
      "9992,game,,2017,,New response bands"
    ),
    path
  )
  changed <- fit_stage1_longitudinal(
    prepared,
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    read_series_breaks(path, reg$exposures)
  )
  expect_equal(
    changed$status,
    c("fitted", "fitted", "screen item changed between the waves")
  )

  # A panel in two countries gives a pair per country, each on its own
  # children.
  split <- function(rows) {
    rows <- edit(rows)
    people <- unique(rows$participant_id)
    half <- people[seq_along(people) %% 2 == 0]
    rows$country <- ifelse(rows$participant_id %in% half, "Wales", "England")
    rows
  }
  two <- fit_stage1_longitudinal(
    models_prepare("9992", reg, split),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  expect_equal(nrow(two), 6)
  expect_equal(dplyr::n_distinct(two$unit), 2)
  expect_equal(two$n_pairs, rep(300L, 6))
  expect_true(all(grepl("England|Wales", two$unit)))

  # Repeated cross-sections and single occasions follow nobody.
  for (id in c("9993", "9995")) {
    expect_null(fit_stage1_longitudinal(
      models_prepare(id, reg),
      cell,
      reg$outcomes,
      reg$exposures,
      reg$schema,
      no_breaks(reg)
    ))
  }
})

test_that("the gap between waves follows ages where they are fine", {
  fine <- seq(10, 12, length.out = 40)
  coarse <- rep(c(13.5, 16.5), 20)
  # Interviews nine months apart that cross a new year.
  expect_equal(longitudinal_lag(rep(1, 40), rep(0.75, 40), fine), 0.75)
  # Coarse ages: the collection years are used.
  expect_equal(longitudinal_lag(rep(2, 40), rep(0, 40), coarse), 2)
  # Years not recorded, or the same at both waves: ages are used.
  expect_equal(longitudinal_lag(rep(NA, 40), rep(1.5, 40), coarse), 1.5)
  expect_equal(longitudinal_lag(rep(0, 40), rep(1, 40), coarse), 1)
  # Neither gives a gap: none is recorded, not a gap of zero.
  expect_true(is.na(longitudinal_lag(rep(NA, 40), rep(0, 40), coarse)))
})

test_that("an outcome's instrument column is found by its stem", {
  base <- tibble::tibble(wb_selfesteem_measure = "a", wb_self_measure = "b")
  expect_equal(
    longitudinal_measure_column("wb_selfesteem_self", base),
    "wb_selfesteem_measure"
  )
  expect_equal(longitudinal_measure_column("mh_dep_self", base), "")
})

test_that("results are labelled, tested and adjusted in one table", {
  reg <- models_registries()
  spec <- read_model_spec(file.path(repo_root, bp_models_path()))
  hypotheses <- read_hypotheses(
    file.path(repo_root, bp_hypotheses_path()),
    reg$outcomes,
    reg$exposures
  )
  row <- function(model, exposure, level, family, estimate, se, df = 20) {
    tibble::tibble(
      model_name = model,
      exposure = exposure,
      level = level,
      family = family,
      node = if (level == "L0") NA_character_ else "a_node",
      estimate = estimate,
      se = se,
      df = df,
      ci_low = estimate - 2 * se,
      ci_high = estimate + 2 * se,
      status = "pooled"
    )
  }
  tree <- dplyr::bind_rows(
    row("tier1_total", "total_screen", "L0", "wellbeing", -0.05, 0.01),
    row("tier2_content", "st_watch", "L0", "wellbeing", -0.04, 0.01),
    row("tier2_content", "st_game", "L0", "learning", 0.002, 0.005),
    row("tier2_content", "st_watch", "L1", "wellbeing", -0.03, 0.02),
    row("tier2_content", "st_watch", "L2", "wellbeing", -0.03, 0.02),
    row("tier2_content_adjusted", "st_watch", "L0", "wellbeing", -0.02, 0.01),
    row("tier2_content_adjusted", "st_watch", "L2", "wellbeing", -0.02, 0.01),
    row("tier2_content", "st_game", "L0", "cognition", NA, NA)
  )
  out <- assemble_results(tree, NULL, spec, hypotheses, reg$outcomes)
  # The adjusted model is not given L2 in the model list, so that row goes.
  expect_equal(nrow(out), 7)
  expect_true(all(out$development))
  p <- 2 * stats::pt(-5, 20)
  expect_equal(out$p[1], p)
  expect_equal(out$fdr_path[1], "families")
  expect_equal(out$q[1], p * 5)
  expect_equal(out$k_total[1], 5L)
  # One content class tested in wellbeing: K = 5 + 1.
  expect_equal(out$k_total[2], 6L)
  expect_equal(out$q[2], 2 * stats::pt(-4, 20) * 6)
  # The adjusted model adds its own test below that: K = 5 + 1 + 1.
  adjusted <- out[out$model_name == "tier2_content_adjusted", ]
  expect_equal(adjusted$k_total, 7L)
  expect_equal(adjusted$finding, "prereg_secondary")
  expect_equal(out$finding[c(1, 4, 5)], c("finding", "finding", "exploratory"))
  expect_equal(out$contrast_estimate[1], -0.15)

  expect_equal(out$hypothesis[1:3], c("H1", "H2b", "H2c"))
  expect_equal(out$prediction[1:3], c("negative", "negative", "none"))
  # Gaming and learning is predicted to be negligible: the estimate is
  # within the smallest effect of interest, and its q-value uses K = 5 + 1.
  game <- out[3, ]
  expect_true(game$equivalent)
  # An estimate with too few studies to say is not called equivalent.
  thin <- tree
  thin$status[3] <- "too few studies to say"
  thin <- assemble_results(thin, NULL, spec, hypotheses, reg$outcomes)
  expect_true(is.na(thin$equivalent[3]))
  expect_equal(thin$p_equivalence[3], game$p_equivalence)
  expect_equal(game$q_equivalence, min(game$p_equivalence * 6, 1))
  expect_true(is.na(out$q_equivalence[1]))
  # A family that could not be pooled has no p-value and is not counted.
  last <- out[out$family == "cognition", ]
  expect_true(is.na(last$p) && is.na(last$q) && is.na(last$k_total))
  expect_true(is.na(adjusted$hypothesis))

  expect_error(
    assemble_results(
      row("no_such_model", "st_watch", "L0", "wellbeing", 0, 1),
      NULL,
      spec,
      hypotheses,
      reg$outcomes
    ),
    "not in the model list"
  )
})
