# Multiple imputation (ANALYSIS_PLAN.md, Step 8): what is imputed and what
# is left alone, that an imputation can be made again from its seed, that
# it removes a bias complete cases have, and Rubin's rules.

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

test_that("only sporadic gaps in screen use and covariates are imputed", {
  reg <- imputation_registries()
  edit <- function(rows) {
    set.seed(71)
    n <- nrow(rows)
    rows$st_socmedia <- NA_real_
    rows$st_game[sample(n, 300)] <- NA
    rows$sex[sample(n, 100)] <- NA
    rows$mh_dep_self[sample(n, 200)] <- NA
    # Known for a third of the rows: not sporadic, so left alone.
    rows$ses_household <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
    rows$ses_household[sample(n, 1000)] <- NA
    rows
  }
  prepared <- imputation_prepare("9995", reg, edit)
  first <- impute_dataset(prepared, reg$outcomes, reg$exposures, 1L)
  again <- impute_dataset(prepared, reg$outcomes, reg$exposures, 1L)
  second <- impute_dataset(prepared, reg$outcomes, reg$exposures, 2L)
  # The same number gives the same imputation, and another number another.
  expect_equal(again, first)
  expect_equal(second$numeric$row, first$numeric$row)
  expect_false(all(second$numeric$value == first$numeric$value))

  expect_setequal(first$numeric$variable, "st_game")
  expect_setequal(first$categorical$variable, "sex")
  game <- first$numeric
  expect_equal(nrow(game), 300)
  expect_true(all(is.na(prepared$exposures$st_game[game$row])))
  # Predictive mean matching draws values the unit observed.
  expect_true(all(game$value %in% prepared$exposures$st_game))
  expect_true(all(first$categorical$value %in% c("Female", "Male")))
  notes <- first$units
  expect_equal(
    notes$status[notes$variable == "ses_household"],
    "too little observed in the unit to impute"
  )
  expect_equal(notes$n_imputed[notes$variable == "st_game"], 300L)
  # The summed total is not imputed in its own right.
  expect_false(bp_total_exposure() %in% first$numeric$variable)

  # Applied: the gaps are filled, the total is the sum again, the rest of
  # the child's screen use follows, and outcomes are untouched.
  filled <- apply_imputation(prepared, first, 1L)
  expect_false(anyNA(filled$exposures$st_game))
  expect_false(anyNA(filled$exposures$total_screen))
  expect_equal(
    filled$exposures$total_screen[game$row],
    filled$exposures$st_watch[game$row] + game$value
  )
  expect_equal(filled$exposures$other_game, filled$exposures$st_watch)
  expect_equal(filled$outcomes, prepared$outcomes)
  expect_equal(sum(is.na(filled$base$sex)), 0)
  expect_equal(sum(is.na(filled$base$ses_household)), 1000)
  # The other imputation's cells are not applied with this one.
  both <- bind_imputations(list(first, second, NULL))
  expect_equal(apply_imputation(prepared, both, 1L), filled)
  expect_error(apply_imputation(filled, first, 1L), "already has a value")

  # The checks pass, and fail when an imputed value is tampered with.
  review <- review_imputation(prepared, both, reg$outcomes, reg$exposures)
  expect_setequal(review$numeric$source, c("observed", "imputed"))
  expect_equal(unique(review$units$imputations), 2L)
  expect_equal(review$numeric$n[review$numeric$source == "imputed"], 600)
  tampered <- both
  tampered$numeric$value[1] <- 23.9
  expect_error(
    review_imputation(prepared, tampered, reg$outcomes, reg$exposures),
    "outside the values its unit observed"
  )
  tampered <- both
  tampered$numeric$row[1] <- which(!is.na(prepared$exposures$st_game))[1]
  expect_error(
    review_imputation(prepared, tampered, reg$outcomes, reg$exposures),
    "observed value was imputed over"
  )
  tampered <- both
  tampered$numeric$value[tampered$numeric$imputation == 2] <-
    tampered$numeric$value[tampered$numeric$imputation == 1]
  expect_error(
    review_imputation(prepared, tampered, reg$outcomes, reg$exposures),
    "same draw"
  )
  expect_error(
    impute_dataset(prepared, reg$outcomes, reg$exposures, 1L, list()),
    "needs a `seed`"
  )
})

test_that("imputation removes the bias of complete cases", {
  reg <- imputation_registries()
  cell <- tibble::tibble(exposure = "st_game", outcome = "mh_dep_self")
  # Gaming is missing more often for children with high scores, so the
  # children left are a selected group and their slope is too small.
  edit <- function(rows) {
    set.seed(73)
    rows$st_socmedia <- NA_real_
    chance <- stats::plogis(-1.2 + 1.5 * as.numeric(scale(rows$mh_dep_self)))
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
  expect_gt(share, 0.2)
  expect_lt(share, 0.5)
  complete <- fit_stage1(prepared, cell, reg$outcomes, reg$schema)
  imputed <- impute_all(prepared, reg, m = 10L)
  pooled <- fit_stage1_imputed(
    prepared,
    imputed,
    cell,
    reg$outcomes,
    reg$schema
  )
  expect_equal(pooled$status, "fitted")
  expect_equal(pooled$m, 10L)
  expect_equal(pooled$n, whole$n)
  expect_gt(pooled$fmi, 0.05)
  expect_lt(pooled$fmi, 0.9)
  # Complete cases miss the slope of the whole sample; imputation is
  # closer, and its interval covers it.
  expect_gt(abs(complete$estimate - whole$estimate), 0.02)
  expect_lt(
    abs(pooled$estimate - whole$estimate),
    abs(complete$estimate - whole$estimate) / 2
  )
  expect_lt(abs(pooled$estimate - whole$estimate), 2 * pooled$se)
  expect_gt(pooled$se, whole$se)
  expect_false(pooled$design_varied)

  # With nothing imputed the dataset is fitted once, as it is.
  none <- fit_stage1_imputed(
    prepared,
    bind_imputations(list()),
    cell,
    reg$outcomes,
    reg$schema
  )
  expect_equal(none$m, 0L)
  expect_equal(none$estimate, complete$estimate)
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
  expect_true("ethnicity" %in% one$categorical$variable)
  expect_false("minority" %in% one$categorical$variable)
  expect_equal(
    unique(one$units$status[one$units$variable == "minority"]),
    "not needed: the unit records ethnicity"
  )

  # In a panel the child's other wave is among the predictors.
  frame <- imputation_frame(prepared, reg$outcomes, reg$exposures)
  expect_true("aux_other_st_game" %in% names(frame$data))
  expect_true("aux_mental_health" %in% names(frame$data))
  expect_false(anyNA(frame$data$aux_other_st_game))
})
