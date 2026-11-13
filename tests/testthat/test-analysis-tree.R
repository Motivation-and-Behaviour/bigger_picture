# Pooling up the outcome tree (ANALYSIS_PLAN.md, Step 7 task 1): known-answer
# tests on slopes built by hand, because a wrong pooling rule would not show
# on real data.

skip_if_not_installed("metafor")
skip_if_not_installed("clubSandwich")

# `k` datasets of two units each, one slope per unit, around `mean`.
tree_slopes <- function(k, mean, outcome, node, seed, tau = 0.05) {
  set.seed(seed)
  dataset <- sprintf("d%02d", seq_len(k))
  out <- tibble::tibble(
    dataset_id = rep(dataset, each = 2),
    unit = paste(rep(dataset, each = 2), rep(1:2, k), sep = "|"),
    outcome = outcome,
    node = node,
    se = stats::runif(2 * k, 0.03, 0.08)
  )
  out$estimate <- mean +
    rep(stats::rnorm(k, 0, tau), each = 2) +
    stats::rnorm(2 * k, 0, out$se)
  out
}

test_that("slopes are put on one scale and one direction", {
  registry <- list(
    tree = tibble::tibble(
      node = c("fam", "n1", "n2"),
      parent = c(NA, "fam", "fam"),
      level = c("L0", "L1", "L1")
    ),
    rows = tibble::tibble(
      variable_name = c("good", "bad", "bin", "alone", "nodir"),
      node = c("n1", "n1", "n2", NA, "n2"),
      informant = c("self", "parent", "self", "self", "self"),
      direction = c(
        "higher_better",
        "higher_worse",
        "higher_worse",
        "higher_worse",
        "none"
      ),
      synthesis = c(TRUE, TRUE, TRUE, FALSE, TRUE)
    )
  )
  stage1 <- tibble::tibble(
    outcome = c("good", "bad", "bin", "alone", "nodir", "good"),
    estimate = c(0.1, 0.2, 0.9, 0.3, 0.3, NA),
    se = c(0.01, 0.02, 0.18, 0.03, 0.03, NA),
    effect_measure = c(
      "sd_per_hour",
      "sd_per_hour",
      "log_odds_per_hour",
      "sd_per_hour",
      "sd_per_hour",
      NA
    ),
    status = c(rep("fitted", 5), "no variation in the exposure")
  )
  effects <- tree_effects(stage1, registry)
  # A row reported on its own, a row with no direction and an unfitted slope
  # do not pool.
  expect_equal(effects$outcome, c("good", "bad", "bin"))
  expect_equal(effects$family, rep("fam", 3))
  expect_equal(effects$estimate, c(0.1, -0.2, -0.9 / (pi / sqrt(3))))
  expect_equal(effects$se, c(0.01, 0.02, 0.18 / (pi / sqrt(3))))
  expect_equal(effects$natural_estimate, c(0.1, 0.2, 0.9))
  expect_equal(effects$flipped, c(FALSE, TRUE, TRUE))
})

test_that("slopes sharing a unit covary, and others do not", {
  effects <- tibble::tibble(
    unit = c("u1", "u1", "u1", "u2"),
    outcome = c("a", "b", "c", "a"),
    se = c(0.1, 0.2, 0.3, 0.4),
    flipped = c(FALSE, TRUE, FALSE, FALSE)
  )
  correlations <- tibble::tibble(
    unit = "u1",
    outcome_a = c("b", "a"),
    outcome_b = c("a", "c"),
    r = c(0.5, 0.25)
  )
  built <- tree_covariance(effects, correlations)
  expect_equal(diag(built$V), effects$se^2)
  # One of the pair changed sign, so the correlation does too.
  expect_equal(built$V[1, 2], -0.5 * 0.1 * 0.2)
  expect_equal(built$V[1, 3], 0.25 * 0.1 * 0.3)
  # No estimate for b and c: the default, counted.
  expect_equal(built$V[2, 3], 0.6 * 0.2 * 0.3)
  expect_equal(built$assumed, 1L)
  expect_equal(built$V[4, 1:3], c(0, 0, 0))
  expect_true(isSymmetric(built$V))
})

test_that("a group of one row per unit pools as stage 2 does", {
  slopes <- tree_slopes(8, 0.1, "a", "n1", seed = 1)
  slopes$status <- "fitted"
  slopes$n <- 100
  group <- pool_tree_group(slopes)$row
  stage2 <- pool_stage2(slopes)
  expect_equal(group$estimate, stage2$estimate)
  expect_equal(group$se, stage2$se)
  expect_equal(group$df, stage2$df)
  expect_equal(group$tau_dataset, stage2$tau_dataset)
  expect_equal(group$k_rows, 1)
  expect_equal(group$pairs_assumed, 0L)
})

test_that("a group needs the minimum number of datasets", {
  slopes <- tree_slopes(2, 0.1, "a", "n1", seed = 1)
  group <- pool_tree_group(slopes)
  expect_equal(group$row$status, "insufficient data")
  expect_null(group$M)
})

test_that("two rows of one unit are not counted as two units", {
  # The same slopes entered as a second, perfectly correlated row add no
  # information: the estimate and its standard error stay the same.
  slopes <- tree_slopes(8, 0.1, "a", "n1", seed = 2)
  copy <- slopes
  copy$outcome <- "b"
  both <- dplyr::bind_rows(slopes, copy)
  both <- both[order(both$unit), ]
  correlations <- tibble::tibble(
    unit = slopes$unit,
    outcome_a = "a",
    outcome_b = "b",
    r = 0.999999
  )
  alone <- pool_tree_group(slopes)$row
  doubled <- pool_tree_group(both, correlations)$row
  expect_equal(doubled$estimate, alone$estimate, tolerance = 1e-4)
  expect_equal(doubled$se, alone$se, tolerance = 1e-3)
  expect_equal(doubled$k_rows, 2)
})

test_that("a family is the plain average of its specific outcomes", {
  # A large, precise specific outcome and a small, noisy one count equally.
  big <- tree_slopes(12, 0.20, "a", "n1", seed = 3)
  big$se <- big$se / 4
  small <- tree_slopes(4, 0.00, "b", "n2", seed = 4)
  effects <- dplyr::bind_rows(big, small)
  groups <- list(n1 = pool_tree_group(big), n2 = pool_tree_group(small))
  family <- pool_tree_family(effects, groups)
  expect_equal(family$k_nodes, 2)
  expect_equal(
    family$estimate,
    mean(c(groups$n1$row$estimate, groups$n2$row$estimate))
  )
  expect_equal(
    c(family$node_low, family$node_high),
    range(groups$n1$row$estimate, groups$n2$row$estimate)
  )
  # The two are about 0.2 apart: more than the smallest effect of interest.
  expect_match(family$warning, "differ by more than the smallest effect")
  # The precise member adds almost nothing to the average's uncertainty,
  # which is about half the noisy member's.
  expect_equal(family$se, groups$n2$row$se / 2, tolerance = 0.1)
})

test_that("a perfectly correlated pair in two specific outcomes counts once", {
  # The same score sits in two nodes: the family then knows no more than
  # one node does, so its estimate, standard error, degrees of freedom and
  # interval are that node's.
  one <- tree_slopes(8, 0.1, "a", "n1", seed = 5)
  two <- one
  two$outcome <- "a_again"
  two$node <- "n2"
  groups <- list(n1 = pool_tree_group(one), n2 = pool_tree_group(two))
  family <- pool_tree_family(dplyr::bind_rows(one, two), groups)
  node <- groups$n1$row
  expect_equal(family$estimate, node$estimate)
  expect_equal(family$se, node$se)
  expect_equal(family$df, node$df)
  expect_equal(family$ci_low, node$ci_low)
  expect_equal(family$ci_high, node$ci_high)
  expect_true(is.na(family$warning))

  # Two unrelated specific outcomes of the same size are more precise
  # together than either alone.
  other <- tree_slopes(8, 0.1, "b", "n2", seed = 6)
  other$dataset_id <- paste0(other$dataset_id, "x")
  other$unit <- paste0(other$unit, "x")
  apart <- pool_tree_family(
    dplyr::bind_rows(one, other),
    list(n1 = groups$n1, n2 = pool_tree_group(other))
  )
  expect_lt(apart$se, 0.85 * node$se)
})

test_that("a family warns when its specific outcomes disagree in sign", {
  up <- tree_slopes(8, 0.01, "a", "n1", seed = 7, tau = 0.001)
  down <- tree_slopes(8, -0.01, "b", "n2", seed = 8, tau = 0.001)
  up$se <- up$se / 20
  down$se <- down$se / 20
  set.seed(9)
  up$estimate <- 0.01 + stats::rnorm(16, 0, up$se)
  down$estimate <- -0.01 + stats::rnorm(16, 0, down$se)
  family <- pool_tree_family(
    dplyr::bind_rows(up, down),
    list(n1 = pool_tree_group(up), n2 = pool_tree_group(down))
  )
  expect_equal(family$warning, "specific outcomes disagree in sign")
  expect_lt(abs(family$estimate), 0.005)
})

test_that("a family needs two pooled specific outcomes", {
  one <- tree_slopes(8, 0.1, "a", "n1", seed = 5)
  few <- tree_slopes(2, 0.1, "b", "n2", seed = 6)
  family <- pool_tree_family(
    dplyr::bind_rows(one, few),
    list(n1 = pool_tree_group(one), n2 = pool_tree_group(few))
  )
  expect_equal(family$status, "fewer than two specific outcomes pooled")
  expect_equal(family$k_nodes, 1)
})
