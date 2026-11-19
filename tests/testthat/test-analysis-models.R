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

  # A test that is not ranked still counts in K, for its own group and the
  # one below, and has no q-value: with it ranked the other two would take
  # 0.02 * 8 / 2 and 0.04 * 8 / 3.
  thin <- adjust_tiered_fdr(
    c(0.001, 0.02, 0.04, 0.03),
    c(rep(classes, 3), specific),
    ranked = c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_equal(thin$q, c(NA, 0.02 * 8 / 1, 0.04 * 8 / 2, 0.03 * 9 / 1))
  expect_equal(thin$k_total, c(8L, 8L, 8L, 9L))

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
    own <- hypotheses[
      hypotheses$exposure == exposure & hypotheses$test == "slope",
    ]
    own$prediction[match(families, own$family)]
  }
  expect_equal(sum(hypotheses$test == "slope"), 25)
  # The other registered hypotheses: a threshold for every exposure and
  # family (H3), no moderation by location (H4), and moderation by sex
  # and by socioeconomic status where the preregistration predicts it.
  by_test <- function(test) hypotheses[hypotheses$test == test, ]
  expect_equal(nrow(by_test("threshold")), 25)
  expect_equal(unique(by_test("moderation_location")$prediction), "none")
  sex_rows <- by_test("moderation_sex")
  expect_equal(unique(sex_rows$exposure), "st_socmedia")
  expect_setequal(sex_rows$family, c("mental_health", "wellbeing"))
  expect_setequal(
    by_test("moderation_ses")$family,
    c("learning", "mental_health", "behaviour")
  )
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
  # The registered moderation hypotheses name their exposures and families.
  families_of <- function(model) {
    own <- after[after$model_name == model, ]
    unique(rows$prereg_family[match(own$outcome, rows$variable_name)])
  }
  sex <- after[after$model_name == "moderation_sex", ]
  expect_equal(unique(sex$exposure), "st_socmedia")
  expect_setequal(
    families_of("moderation_sex"),
    c("mental_health", "wellbeing")
  )
  expect_setequal(
    families_of("moderation_ses"),
    c("learning", "mental_health", "behaviour")
  )
  expect_length(
    intersect(
      families_of("moderation_ses"),
      families_of("moderation_ses_other")
    ),
    0
  )
  # Every registered moderation hypothesis is a cell of the confirmatory
  # model that tests it, and that model tests nothing else: the exposures
  # and families of the two are the same.
  for (model in unique(grep("^moderation_", hypotheses$test, value = TRUE))) {
    registered <- hypotheses[hypotheses$test == model, ]
    own <- after[after$model_name == model, ]
    tested <- unique(tibble::tibble(
      exposure = own$exposure,
      family = rows$prereg_family[match(own$outcome, rows$variable_name)]
    ))
    key <- function(table) sort(paste(table$exposure, table$family))
    expect_equal(key(tested), key(registered), info = model)
    expect_equal(
      spec$analysis_class[spec$model_name == model],
      "confirmatory",
      info = model
    )
  }
  # H4 is overall screen use at school; the other school rows are
  # exploratory.
  location <- after[after$model_name == "moderation_location", ]
  expect_equal(unique(location$exposure), "st_total_school")
  expect_false(
    "st_total_school" %in%
      after$exposure[after$model_name == "moderation_location_other"]
  )
  # A registered test is never also an exploratory one.
  overlap <- function(model) {
    own <- after[after$model_name == model, c("exposure", "outcome")]
    rest <- after[
      after$model_name == paste0(model, "_other"),
      c("exposure", "outcome")
    ]
    nrow(dplyr::inner_join(own, rest, by = c("exposure", "outcome")))
  }
  for (model in c(
    "moderation_sex",
    "moderation_ses",
    "moderation_location",
    "thresholds_by_sex",
    "thresholds_by_ses"
  )) {
    expect_gt(sum(after$model_name == paste0(model, "_other")), 0)
    expect_equal(overlap(model), 0, info = model)
  }
  expect_true(all(spec$tier[spec$analysis_class == "confirmatory"] <= 4))
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

  # So does an outcome recorded as asked differently at one of the waves;
  # at both waves it would be the same item again.
  reworded <- prepared
  reworded$series <- tibble::tibble(
    variable_name = "mh_dep_self",
    wave = "2",
    reason = "Reworded"
  )
  asked <- fit_stage1_longitudinal(
    reworded,
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  expect_equal(
    asked$status,
    c("fitted", "fitted", "outcome asked differently at one of the waves")
  )
  reworded$series <- tibble::tibble(
    variable_name = c("mh_dep_self", "mh_dep_self", "beh_ext_sdq_parent"),
    wave = c("1", "2", "1"),
    reason = "Reworded"
  )
  expect_true(all(
    fit_stage1_longitudinal(
      reworded,
      cell,
      reg$outcomes,
      reg$exposures,
      reg$schema,
      no_breaks(reg)
    )$status ==
      "fitted"
  ))

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

  # Three waves, with no gaming item at the second. The lagged and matched
  # models pair the first wave with the second, which has the outcome; the
  # within-child model pairs it with the third, the next that has both.
  # Nothing starts from the second wave, which has no screen use.
  three_waves <- function(rows) {
    rows <- edit(rows)
    third <- rows[rows$wave == "2", ]
    third$wave <- "3"
    third$age_years <- third$age_years + 2
    third$data_year <- third$data_year + 2
    set.seed(67)
    third$st_game <- third$st_game + stats::rnorm(nrow(third), 0, 0.5)
    third$mh_dep_self <- third$mh_dep_self + stats::rnorm(nrow(third), 0, 0.5)
    rows$st_game[rows$wave == "2"] <- NA
    rbind(rows, third)
  }
  spanning <- fit_stage1_longitudinal(
    models_prepare("9992", reg, three_waves),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  to_wave <- function(model) spanning$wave_to[spanning$model == model]
  expect_equal(to_wave("lagged"), "2")
  expect_equal(to_wave("matched"), "2")
  expect_equal(to_wave("within"), "3")
  expect_equal(unique(spanning$wave_from), "1")
  expect_equal(spanning$lag_years[spanning$model == "within"], 4)
  expect_true(all(spanning$status == "fitted"))

  # A subsample followed up at the second wave and everyone else at the
  # third: each child goes to their own next wave, so the unit has two
  # pairs and nobody is dropped. A group too small to be a pair is counted.
  subsample <- function(rows) {
    rows <- three_waves(rows)
    rows$st_game[rows$wave == "2"] <- rows$st_game[rows$wave == "3"]
    people <- unique(rows$participant_id)
    rows$mh_dep_self[
      rows$wave == "2" & !rows$participant_id %in% people[1:200]
    ] <- NA
    rows$mh_dep_self[
      rows$wave == "3" & rows$participant_id %in% people[1:200]
    ] <- NA
    rows[!(rows$wave != "1" & rows$participant_id %in% people[581:600]), ]
  }
  parts <- fit_stage1_longitudinal(
    models_prepare("9992", reg, subsample),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  first <- parts[parts$wave_from == "1" & parts$model == "lagged", ]
  expect_equal(first$wave_to, c("2", "3"))
  expect_equal(first$n_pairs, c(200L, 380L))
  expect_equal(first$n_earlier, c(600L, 600L))
  expect_equal(first$n_left, c(0L, 0L))
  expect_equal(dplyr::n_distinct(first$unit), 2)
  few <- function(rows) {
    rows <- subsample(rows)
    people <- unique(rows$participant_id)
    rows$mh_dep_self[
      rows$wave == "2" & !rows$participant_id %in% people[1:40]
    ] <- NA
    # Twenty of the forty have no gaming at the first wave, so only twenty
    # of the unit's children go to the second wave.
    rows$st_game[rows$wave == "1" & rows$participant_id %in% people[21:40]] <-
      NA
    rows
  }
  small <- fit_stage1_longitudinal(
    models_prepare("9992", reg, few),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  small <- small[small$wave_from == "1" & small$model == "lagged", ]
  expect_equal(small$wave_to, "3")
  expect_equal(small$n_left, 20L)

  # A child whose screen use is reported by someone else at the later wave
  # is left out of the within-child model only.
  reporter <- function(rows) {
    rows <- edit(rows)
    people <- unique(rows$participant_id)
    rows$st_responder <- ifelse(
      rows$wave == "2" & rows$participant_id %in% people[1:150],
      "Self",
      "Parent"
    )
    rows
  }
  moved <- fit_stage1_longitudinal(
    models_prepare("9992", reg, reporter),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  expect_equal(moved$n_switched[moved$model == "within"], 150L)
  expect_equal(moved$n[moved$model == "within"], 450L)
  expect_equal(moved$n[moved$model == "lagged"], 600L)

  # Who is kept between waves is described without any outcome.
  kept <- describe_panel_pairs(
    models_prepare("9992", reg, function(rows) {
      rows <- rows[!(rows$wave == "2" & rows$st_game > 2), ]
      rows
    }),
    "st_game"
  )
  expect_equal(kept$n_earlier, 600L)
  expect_lt(kept$retained, 0.9)
  # Those who gamed most at the second wave left, which the first wave's
  # gaming does not predict in the mock.
  expect_lt(abs(kept$difference_sd), 0.2)
  expect_null(describe_panel_pairs(models_prepare("9995", reg), "st_game"))

  # A unit with no later wave that enough of its children reach says so.
  ended <- function(rows) {
    rows <- edit(rows)
    rows$mh_dep_self[rows$wave == "2"] <- NA
    rows
  }
  none <- fit_stage1_longitudinal(
    models_prepare("9992", reg, ended),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  expect_equal(
    unique(none$status),
    "no later wave with enough of the unit's children"
  )
  expect_equal(nrow(none), 3)

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

test_that("the longitudinal slopes are pooled model by model, with their gaps", {
  reg <- models_registries()
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  one <- fit_stage1_longitudinal(
    models_prepare("9992", reg),
    cell,
    reg$outcomes,
    reg$exposures,
    reg$schema,
    no_breaks(reg)
  )
  # Six panels made from the one: each with its own slopes and gap, and
  # the last with a second pair of waves nine years apart.
  set.seed(71)
  copy <- function(id, gap, pair = "a") {
    rows <- one
    rows$dataset_id <- id
    rows$unit <- paste(id, pair)
    rows$estimate <- rows$estimate + stats::rnorm(nrow(rows), 0, 0.02)
    rows$lag_years <- gap
    rows
  }
  gaps <- c(1, 1, 2, 4, 6, 1)
  estimates <- dplyr::bind_rows(
    lapply(seq_along(gaps), function(i) copy(paste0("p", i), gaps[i])),
    list(copy("p6", 9, "b"))
  )
  pooled <- pool_longitudinal(estimates, cell, reg$outcomes)
  expect_setequal(
    pooled$tree$model_name,
    paste0("longitudinal_", c("lagged", "matched", "within"))
  )
  expect_setequal(pooled$cells$model_name, pooled$tree$model_name)
  for (model in c("lagged", "matched", "within")) {
    rows <- estimates[estimates$model == model, ]
    name <- paste0("longitudinal_", model)
    # Each model is pooled on its own slopes, as the cross-sectional ones
    # are.
    alone <- pool_tree(rows, reg$outcomes, NULL)
    own <- pooled$tree[pooled$tree$model_name == name, ]
    expect_equal(own$estimate, alone$estimate, info = model)
    expect_equal(own$se, alone$se, info = model)
    expect_equal(
      pooled$cells$estimate[pooled$cells$model_name == name],
      pool_stage2_cells(rows, cell)$estimate,
      info = model
    )
    # The gap is the median over datasets of each dataset's median, so
    # the panel with two pairs (1 and 9 years, median 5) counts once:
    # the median of 1, 1, 2, 4, 6 and 5.
    family <- own[own$level == "L0", ]
    expect_equal(family$k_datasets, 6L, info = model)
    expect_equal(family$lag_years, 3, info = model)
    expect_equal(family$lag_years_min, 1, info = model)
    expect_equal(family$lag_years_max, 6, info = model)
    expect_equal(own$lag_years[own$level == "L1"], 3, info = model)
    expect_true(all(is.na(own$lag_years[own$level == "L2"])), info = model)
  }
  empty <- pool_longitudinal(estimates[0, ], cell, reg$outcomes)
  expect_equal(nrow(empty$tree), 0)
})

test_that("the gap between waves follows ages where they can show it", {
  fine <- seq(10, 12, length.out = 40)
  coarse <- rep(c(13.5, 16.5), 20)
  gap <- function(...) longitudinal_lag(...)$years
  # Interviews nine months apart that cross a new year.
  expect_equal(gap(rep(1, 40), rep(0.75, 40), fine), 0.75)
  expect_false(longitudinal_lag(rep(1, 40), rep(0.75, 40), fine)$differ)
  # Coarse ages: the collection years are used.
  expect_equal(gap(rep(2, 40), rep(0, 40), coarse), 2)
  # Age recorded as age at entry plus the time since: every child moves
  # by the same part of a year, which the calendar years overstate.
  entry <- rep(c(6.5, 7), 20)
  steady <- longitudinal_lag(rep(1, 40), rep(0.25, 40), entry)
  expect_equal(steady$years, 0.25)
  expect_true(steady$differ)
  # Whole-year ages that all move by a year say nothing the years do not.
  expect_equal(gap(rep(2, 40), rep(1, 40), rep(c(10, 11), 20)), 2)
  # Years not recorded, or the same at both waves: ages are used.
  expect_equal(gap(rep(NA, 40), rep(1.5, 40), coarse), 1.5)
  expect_equal(gap(rep(0, 40), rep(1, 40), coarse), 1)
  # Neither gives a gap: none is recorded, not a gap of zero.
  expect_true(is.na(gap(rep(NA, 40), rep(0, 40), coarse)))
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
  # An estimate with too few studies to say is not called equivalent or a
  # finding, and is counted but not ranked: it has no q-value, and K is
  # what it was.
  thin <- tree
  thin$status[3] <- "too few studies to say"
  thin <- assemble_results(thin, NULL, spec, hypotheses, reg$outcomes)
  expect_true(is.na(thin$equivalent[3]))
  expect_equal(thin$p_equivalence[3], game$p_equivalence)
  expect_equal(thin$finding[3], "too few studies to say")
  # A row that could not be pooled at all is not a finding either.
  expect_equal(out$finding[out$family == "cognition"], "not estimated")
  expect_true(is.na(thin$q[3]) && is.na(thin$q_equivalence[3]))
  expect_equal(thin$k_total[3], game$k_total)
  expect_equal(thin$q[-3], out$q[-3])
  expect_equal(game$q_equivalence, min(game$p_equivalence * 6, 1))
  expect_true(is.na(out$q_equivalence[1]))
  # A family whose specific outcomes disagree is treated the same way: it
  # is labelled so, counted, not ranked, and never called equivalent.
  split <- tree
  split$headline <- TRUE
  split$headline[3] <- FALSE
  split <- assemble_results(split, NULL, spec, hypotheses, reg$outcomes)
  expect_equal(split$finding[3], "specific outcomes disagree")
  expect_true(is.na(split$q[3]) && is.na(split$equivalent[3]))
  expect_equal(split$k_total[3], game$k_total)
  expect_equal(split$q[-3], out$q[-3])
  # A moderator test comes with its own p-value: it joins the family's
  # exploratory layer, is counted and adjusted there, has no equivalence
  # test, and leaves the layers above it as they were.
  tests <- tibble::tibble(
    model_name = c("moderation_study_level", "year_trend"),
    exposure = "st_watch",
    level = c("L0", "L1"),
    family = "wellbeing",
    node = c(NA, "self_esteem"),
    moderator = c("informant", "data_year"),
    term = c("levels equal", "between datasets"),
    effect_measure = c("sd_per_hour_difference", "sd_per_hour_per_decade"),
    estimate = c(NA, -0.05),
    df = c(8, 10),
    p_test = c(0.03, NA),
    status = c("pooled", "too few datasets or years")
  )
  with_tests <- assemble_results(
    tree,
    NULL,
    spec,
    hypotheses,
    reg$outcomes,
    tests = tests
  )
  expect_equal(nrow(with_tests), nrow(out) + 2)
  moderated <- with_tests[
    with_tests$model_name == "moderation_study_level",
  ]
  expect_equal(moderated$p, 0.03)
  expect_equal(moderated$moderator, "informant")
  expect_equal(moderated$finding, "exploratory")
  expect_equal(moderated$tier, 5L)
  expect_false(is.na(moderated$q))
  expect_gt(moderated$k_total, 5L)
  expect_true(is.na(moderated$p_equivalence))
  trend <- with_tests[with_tests$model_name == "year_trend", ]
  expect_equal(trend$finding, "not estimated")
  expect_true(is.na(trend$p) && is.na(trend$k_total))
  expect_equal(with_tests$q[seq_len(nrow(out))], out$q)
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
