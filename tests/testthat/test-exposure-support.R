# The rule on rows where the exposure varies (DR-0022).

skip_unless_engine_needed()
skip_if_not_installed("fixest")

# One unit of 400 children. Screen use is zero except for `users` of them;
# the binary outcome has 60 events, `events_among_users` of them among the
# children with any use.
support_unit <- function(users, events_among_users = 0, seed = 5) {
  withr::local_seed(seed)
  n <- 400
  x <- rep(0, n)
  x[seq_len(users)] <- sample(c(5 / 14, 1 / 3, 1, 2), users, replace = TRUE)
  event <- rep(0, n)
  event[seq_len(events_among_users)] <- 1
  event[seq(n, by = -1, length.out = 60 - events_among_users)] <- 1
  tibble::tibble(
    x = x,
    y = stats::rnorm(n) + 0.2 * x,
    event = event,
    age = stats::runif(n, 8, 12),
    sex = sample(c("Female", "Male"), n, replace = TRUE),
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_
  )
}

test_that("exposure_varies() compares values as numbers", {
  # A weekly average such as 5/14 does not survive a round trip through
  # text, which once made every row count as varying.
  x <- c(rep(5 / 14, 40), rep(1 / 3, 10), 2, 0)
  expect_equal(sum(exposure_varies(x)), 12)
  expect_equal(sum(exposure_varies(c(rep(1 / 3, 3), 0.1 + 0.2))), 1)
  # Ties go to the lowest value.
  expect_equal(
    exposure_varies(c(2, 0, 2, 0, 1)),
    c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  expect_equal(exposure_varies(c(3, 3, 3)), c(FALSE, FALSE, FALSE))
})

test_that("a slope needs 30 rows where the exposure varies", {
  enough <- fit_stage1_unit(support_unit(30), "linear")
  expect_equal(enough$status, "fitted")
  short <- fit_stage1_unit(support_unit(29), "linear")
  expect_equal(short$status, "too few rows where the exposure varies")
  expect_true(is.na(short$estimate))
  expect_equal(short$n, 400)
})

test_that("a binary outcome needs 10 of each class where the exposure varies", {
  unit <- function(events) {
    data <- support_unit(60, events)
    data$y <- data$event
    data
  }
  expect_equal(fit_stage1_unit(unit(10), "logistic")$status, "fitted")
  # Nine events among the 60 children with any use, 60 events in all.
  expect_equal(
    fit_stage1_unit(unit(9), "logistic")$status,
    "too few of one class where the exposure varies"
  )
  # None at all, which a logistic model cannot estimate.
  expect_equal(
    fit_stage1_unit(unit(0), "logistic")$status,
    "too few of one class where the exposure varies"
  )
  # The 30-row minimum comes first.
  short <- support_unit(20, 15)
  short$y <- short$event
  expect_equal(
    fit_stage1_unit(short, "logistic")$status,
    "too few rows where the exposure varies"
  )
})

test_that("the rule is judged on recorded values, not imputed ones", {
  data <- support_unit(40)
  # Twenty of the forty children with any use had it imputed.
  data$x_recorded <- !seq_len(nrow(data)) %in% 1:20
  expect_equal(
    fit_stage1_unit(data, "linear")$status,
    "too few rows where the exposure varies"
  )
  data$x_recorded <- TRUE
  expect_equal(fit_stage1_unit(data, "linear")$status, "fitted")
})

test_that("the support rule reuses the minimum unit and class sizes", {
  settings <- analysis_settings()
  expect_equal(settings$support_min_rows, settings$min_unit_rows)
  expect_equal(settings$support_min_class, 10L)
  expect_true(all(
    c("support_min_rows", "support_min_class") %in% bp_fit_settings()
  ))
})

# Models of moderation (DR-0021).

moderation_unit <- function(n = 4000, difference = 0.3, seed = 9) {
  withr::local_seed(seed)
  sex <- sample(c("Female", "Male"), n, replace = TRUE)
  ses <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
  x <- stats::runif(n, 0, 6)
  tibble::tibble(
    x = x,
    # The slope is -0.1 for boys and -0.1 - `difference` for girls; and
    # -0.1 in the middle group, `difference` lower in the lowest and
    # higher in the highest.
    y_sex = stats::rnorm(n) - 0.1 * x - difference * x * (sex == "Female"),
    y_ses = stats::rnorm(n) -
      0.1 * x +
      difference * x * unname(c(Low = -1, Medium = 0, High = 1)[ses]),
    age = stats::runif(n, 8, 12),
    sex = sex,
    ses_household = ses,
    parental_education = NA_character_,
    ses_area = NA_character_,
    ethnicity = NA_character_,
    minority = NA_character_,
    urbanicity = NA_character_,
    weight = NA_real_,
    cluster = NA_character_,
    family = NA_character_
  )
}

test_that("moderation by sex estimates the girls' slope less the boys'", {
  data <- moderation_unit()
  data$y <- data$y_sex
  fit <- fit_stage1_unit(data, "linear", moderator = "sex")
  expect_equal(fit$status, "fitted")
  expect_equal(fit$estimate, -0.3, tolerance = 0.05)
  expect_equal(fit$slope_second, -0.1, tolerance = 0.05)
  expect_equal(fit$slope_first - fit$slope_second, fit$estimate)
  expect_equal(fit$moderator, "sex")
  # The same answer as two separate fits put together.
  girls <- fit_stage1_unit(data[data$sex == "Female", ], "linear")
  boys <- fit_stage1_unit(data[data$sex == "Male", ], "linear")
  expect_equal(fit$estimate, girls$estimate - boys$estimate, tolerance = 0.01)

  # A child recorded as another sex is left out of this model.
  other <- data
  other$sex[1:50] <- "Other"
  left_out <- fit_stage1_unit(other, "linear", moderator = "sex")
  expect_equal(left_out$n, 3950)
  expect_equal(left_out$n_available, 4000)
  expect_equal(left_out$n_no_group, 50)

  # Each group needs the minimum number of rows, and of rows that vary.
  few_girls <- data[data$sex == "Male" | cumsum(data$sex == "Female") <= 20, ]
  expect_equal(
    fit_stage1_unit(few_girls, "linear", moderator = "sex")$status,
    "in a compared group: fewer rows than the minimum unit size"
  )
  flat <- data
  girls_rows <- which(flat$sex == "Female")
  flat$x[girls_rows[-(1:10)]] <- 0
  expect_equal(
    fit_stage1_unit(flat, "linear", moderator = "sex")$status,
    "in a compared group: too few rows where the exposure varies"
  )
  # A group is judged on its own commonest value: here the boys nearly all
  # record one hour, which is not the unit's commonest value, so only 20
  # of them vary although none is at the unit's commonest.
  boys_rows <- which(data$sex == "Male")
  heaped <- data
  heaped$x[data$sex == "Female"] <- 0
  heaped$x[which(data$sex == "Female")[1:200]] <- 2
  heaped$x[boys_rows] <- 1
  heaped$x[boys_rows[1:20]] <- 3
  expect_equal(
    fit_stage1_unit(heaped, "linear", moderator = "sex")$status,
    "in a compared group: too few rows where the exposure varies"
  )
  # And on its own classes of a binary outcome: the boys have four events.
  binary <- data
  binary$y <- as.numeric(stats::runif(nrow(binary)) < 0.3)
  binary$y[boys_rows] <- 0
  binary$y[boys_rows[1:4]] <- 1
  expect_equal(
    fit_stage1_unit(binary, "logistic", moderator = "sex")$status,
    "in a compared group: fewer than 10 rows in one class"
  )
  one_sex <- data[data$sex == "Male", ]
  expect_equal(
    fit_stage1_unit(one_sex, "linear", moderator = "sex")$status,
    "girls and boys are not both recorded in the unit"
  )
})

test_that("moderation by SES estimates the lowest group's slope less the highest's", {
  data <- moderation_unit()
  data$y <- data$y_ses
  fit <- fit_stage1_unit(data, "linear", moderator = "ses")
  expect_equal(fit$status, "fitted")
  # Lowest -0.4, highest 0.2.
  expect_equal(fit$estimate, -0.6, tolerance = 0.05)
  expect_equal(fit$slope_first, -0.4, tolerance = 0.05)
  expect_equal(fit$slope_second, 0.2, tolerance = 0.05)
  expect_equal(fit$moderator_measure, "ses_household")
  expect_false(grepl("ses_household", fit$covariates, fixed = TRUE))

  # The outcome need not step evenly across the groups: here the middle
  # group sits well above the line between the other two and uses screens
  # more, which a score alone would take for a difference in slopes.
  uneven <- data
  middle <- uneven$ses_household == "Medium"
  uneven$x[middle] <- uneven$x[middle] + 2
  uneven$y <- stats::rnorm(nrow(uneven)) - 0.1 * uneven$x + 1.5 * middle
  expect_equal(
    fit_stage1_unit(uneven, "linear", moderator = "ses")$estimate,
    0,
    tolerance = 0.05
  )

  # Parental education is used, by its own order, where it is the measure.
  education <- data
  education$parental_education <- unname(c(
    Low = "Secondary school or less",
    Medium = "Post-secondary",
    High = "Bachelor degree or higher"
  )[education$ses_household])
  education$ses_household <- NA_character_
  by_education <- fit_stage1_unit(education, "linear", moderator = "ses")
  expect_equal(by_education$moderator_measure, "parental_education")
  expect_equal(by_education$estimate, fit$estimate)

  none <- data
  none$ses_household <- NA_character_
  expect_equal(
    fit_stage1_unit(none, "linear", moderator = "ses")$status,
    "no socioeconomic measure that varies in the unit"
  )
  # A child whose group is unknown is left out.
  unknown <- data
  unknown$ses_household[1:100] <- "Unknown"
  expect_equal(fit_stage1_unit(unknown, "linear", moderator = "ses")$n, 3900)
})

test_that("a model without a moderator is unchanged by the option", {
  data <- moderation_unit()
  data$y <- data$y_sex
  plain <- fit_stage1_unit(data, "linear")
  expect_false(any(c("moderator", "slope_first") %in% names(plain)))
  expect_equal(plain$status, "fitted")
})

test_that("the exposure of a moderated model is imputed with the group in its model", {
  data <- moderation_unit(n = 3000, difference = 0.4)
  data$y <- data$y_sex
  data$moderator <- as.numeric(data$sex == "Female")
  withr::local_seed(3)
  gone <- stats::runif(nrow(data)) < 0.25
  full <- fit_stage1_unit(data, "linear", moderator = "sex")$estimate
  data$x[gone] <- NA
  settings <- analysis_settings()
  estimate <- function(moderated) {
    mean(vapply(
      1:10,
      function(index) {
        filled <- impute_cell_exposure(data, index, settings, moderated)
        expect_equal(attr(filled, "imputed"), sum(gone))
        fit_stage1_unit(filled, "linear", moderator = "sex")$estimate
      },
      numeric(1)
    ))
  }
  # With the group in the exposure's model the difference is recovered;
  # without it, it shrinks towards zero.
  expect_lt(abs(estimate(TRUE) - full), 0.03)
  expect_gt(estimate(FALSE) - full, 0.04)
})

test_that("imputations are fitted in groups and none may go missing", {
  expect_equal(split_imputations(1:12, 5), list(1:5, 6:10, 11:12))
  expect_equal(split_imputations(1:3, 5), list(1:3))
  expect_length(split_imputations(seq_len(50)), 50 / bp_imputation_group_size())

  fits <- tibble::tibble(
    unit = "u",
    exposure = "st_watch",
    outcome = "y",
    tier = 1L,
    imputation = 1:4,
    status = "fitted",
    estimate = c(0.1, 0.2, 0.1, 0.2),
    se = 0.05,
    df = 100,
    covariates = "age",
    ses_measure = NA_character_,
    ethnicity_measure = NA_character_,
    covariate_imputed_share = 0,
    n = 100,
    n_available = 100,
    n_no_weight = 0,
    n_no_sex = 0,
    n_no_covariate = 0,
    n_no_other = NA,
    n_other_inconsistent = NA,
    n_exposure_imputed = 0,
    tier1_same_rows_estimate = NA_real_,
    tier1_same_rows_se = NA_real_
  )
  whole <- combine_stage1_imputations(fits, 1:4)
  in_groups <- combine_stage1_imputations(
    list(fits[1:2, ], NULL, fits[3:4, ]),
    1:4
  )
  expect_equal(in_groups, whole)
  expect_equal(whole$estimate, 0.15)
  expect_equal(whole$m, 4)
  # A group that did not arrive is an error, not a slope fitted in too few,
  # and so is one that arrives twice.
  expect_error(combine_stage1_imputations(fits[1:2, ], 1:4), "not the 4")
  expect_error(
    combine_stage1_imputations(list(fits, fits[1:2, ]), 1:4),
    "twice"
  )

  # A model of moderation: the two groups' slopes are averaged over the
  # imputations, so their difference is still the estimate.
  moderated <- fits
  moderated$moderator <- "sex"
  moderated$moderator_measure <- "sex"
  moderated$n_no_group <- 2
  moderated$slope_second <- c(-0.2, -0.1, -0.2, -0.1)
  moderated$slope_first <- moderated$slope_second + moderated$estimate
  pooled <- combine_stage1_imputations(moderated, 1:4)
  expect_equal(pooled$slope_first - pooled$slope_second, pooled$estimate)
  expect_equal(pooled$slope_second, -0.15)
  expect_equal(pooled$n_no_group, 2)
  expect_null(combine_stage1_imputations(list(NULL, NULL), 1:4))
})

test_that("label_moderation() places a pooled difference by exposure and family", {
  registry <- list(
    rows = tibble::tibble(
      variable_name = c("dep", "happy", "read"),
      prereg_family = c("mental_health", "wellbeing", "learning")
    )
  )
  model_cells <- tibble::tibble(
    model_name = c(
      "moderation_sex",
      "moderation_sex",
      "moderation_sex_other",
      "moderation_sex_other"
    ),
    exposure = c("st_socmedia", "st_socmedia", "st_socmedia", "st_watch"),
    outcome = c("dep", "happy", "read", "dep")
  )
  tree <- tibble::tibble(
    exposure = c("st_socmedia", "st_socmedia", "st_watch", "st_game"),
    family = c("mental_health", "learning", "mental_health", "learning"),
    level = "L0",
    estimate = 1:4
  )
  labelled <- label_moderation(
    tree,
    model_cells,
    registry,
    "moderation_sex",
    "sex"
  )
  # The registered families of social media, the rest of social media and
  # another exposure; a pair in neither model's cells is dropped.
  expect_equal(
    labelled$model_name,
    c("moderation_sex", "moderation_sex_other", "moderation_sex_other")
  )
  expect_equal(labelled$estimate, 1:3)
  expect_equal(unique(labelled$moderator), "sex")
  expect_equal(
    nrow(label_moderation(
      tree[0, ],
      model_cells,
      registry,
      "moderation_sex",
      "sex"
    )),
    0
  )
})
