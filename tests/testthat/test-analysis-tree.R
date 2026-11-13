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
  # An unfitted slope is not returned; a row reported on its own and a row
  # with no direction are, marked as not pooling and why.
  expect_equal(effects$outcome, c("good", "bad", "bin", "alone", "nodir"))
  expect_equal(effects$pools, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(
    effects$left_out,
    c(NA, NA, NA, "reported on its own", "no direction")
  )
  effects <- effects[effects$pools, ]
  expect_equal(effects$family, rep("fam", 3))
  expect_equal(effects$estimate, c(0.1, -0.2, -0.9 / (pi / sqrt(3))))
  expect_equal(effects$se, c(0.01, 0.02, 0.18 / (pi / sqrt(3))))
  expect_equal(effects$natural_estimate, c(0.1, 0.2, 0.9))
  expect_equal(effects$flipped, c(FALSE, TRUE, TRUE))
  registry$rows$informant[1] <- NA
  expect_equal(tree_effects(stage1, registry)$informant[1], "not recorded")
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
  expect_equal(built$repaired, 0L)

  # A pair too thin to estimate arrives with no correlation: it takes the
  # default, which is for aligned slopes, scaled by the pair's overlap, and
  # is counted, whichever of the two was flipped.
  thin <- tibble::tibble(
    unit = "u1",
    outcome_a = "a",
    outcome_b = "b",
    r = NA_real_,
    overlap = 0.5
  )
  built <- tree_covariance(effects[1:2, ], thin)
  expect_equal(built$V[1, 2], 0.6 * 0.5 * 0.1 * 0.2)
  expect_equal(built$assumed, 1L)
})

test_that("correlations that cannot all hold are shrunk, and counted", {
  effects <- tibble::tibble(
    unit = "u1",
    outcome = c("a", "b", "c"),
    se = c(0.1, 0.1, 0.1)
  )
  correlations <- tibble::tibble(
    unit = "u1",
    outcome_a = c("a", "a", "b"),
    outcome_b = c("b", "c", "c"),
    r = c(0.95, 0.95, -0.5)
  )
  built <- tree_covariance(effects, correlations)
  expect_equal(built$repaired, 1L)
  expect_gt(min(eigen(built$V, only.values = TRUE)$values), 0)
  # Shrunk by one factor, so the pattern is kept.
  expect_equal(built$V[1, 2], built$V[1, 3])
  expect_equal(built$V[2, 3] / built$V[1, 2], -0.5 / 0.95)

  # The same row twice in a unit is a perfect correlation, made usable.
  twice <- tibble::tibble(unit = "u1", outcome = c("a", "a"), se = c(0.1, 0.1))
  built <- tree_covariance(twice)
  expect_equal(built$repaired, 1L)
  expect_gt(min(eigen(built$V, only.values = TRUE)$values), 0)
  expect_equal(built$V[1, 2], 0.01, tolerance = 1e-4)
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
  # The two are about 0.2 apart, more than the smallest effect of interest,
  # and a test rejects that they are equal: not for the headline.
  expect_lt(family$nodes_differ_p, 0.05)
  expect_match(family$warning, "differ by more than the smallest effect")
  expect_false(family$headline)
  expect_true(is.na(family$nodes_left_out))
  # The sensitivity average weights each by its datasets: 12 and 4.
  expect_equal(
    family$estimate_by_datasets,
    (12 * groups$n1$row$estimate + 4 * groups$n2$row$estimate) / 16
  )
  expect_lt(family$se_by_datasets, family$se)
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
  expect_true(family$headline)

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
  # Each is clearly on its own side of zero, though they are closer than
  # the smallest effect of interest.
  expect_equal(family$warning, "specific outcomes on opposite sides of zero")
  expect_false(family$headline)
  expect_lt(abs(family$estimate), 0.005)
})

test_that("equal specific outcomes are not warned for differing by chance", {
  # Ten specific outcomes with one true effect, several of them small: their
  # estimates span more than the smallest effect of interest in most draws,
  # and the family is still not warned in most.
  warned <- vapply(
    1:30,
    function(seed) {
      parts <- lapply(1:10, function(i) {
        k <- c(3, 4, 5, 6, 8, 10, 12, 12, 14, 16)[i]
        part <- tree_slopes(
          k,
          -0.05,
          paste0("o", i),
          paste0("n", i),
          100 * seed + i
        )
        # Each specific outcome is measured by its own draw of 35 datasets.
        renamed <- sprintf("d%02d", sort(sample(35, k)))
        names(renamed) <- unique(part$dataset_id)
        part$unit <- sub("^d[0-9]+", "", part$unit)
        part$dataset_id <- unname(renamed[part$dataset_id])
        part$unit <- paste0(part$dataset_id, part$unit)
        part
      })
      groups <- lapply(parts, pool_tree_group)
      names(groups) <- paste0("n", 1:10)
      family <- pool_tree_family(dplyr::bind_rows(parts), groups)
      c(
        wide = family$node_high - family$node_low > 0.10 / 3,
        warned = !is.na(family$warning)
      )
    },
    logical(2)
  )
  expect_gt(mean(warned["wide", ]), 0.5)
  expect_lt(mean(warned["warned", ]), 0.2)
})

test_that("a family with one pooled specific outcome is that outcome", {
  one <- tree_slopes(8, 0.1, "a", "n1", seed = 5)
  few <- tree_slopes(2, 0.1, "b", "n2", seed = 6)
  groups <- list(n1 = pool_tree_group(one), n2 = pool_tree_group(few))
  family <- pool_tree_family(dplyr::bind_rows(one, few), groups)
  expect_equal(family$k_nodes, 1)
  expect_equal(family$estimate, groups$n1$row$estimate)
  expect_equal(family$se, groups$n1$row$se)
  expect_equal(family$status, "pooled")
  # Resting on one specific outcome is said, and is no reason to keep the
  # family from the headline: only disagreement is.
  expect_true(is.na(family$warning))
  expect_equal(family$note, "one specific outcome only")
  expect_true(family$headline)
  expect_equal(family$estimate_by_datasets, family$estimate)
  # The one that could not be pooled is named, with the reason.
  expect_equal(family$nodes_left_out, "n2 (insufficient data)")

  none <- pool_tree_family(few, list(n2 = groups$n2))
  expect_equal(none$status, "no specific outcome pooled")
  expect_true(is.na(none$estimate))
})

test_that("a family refuses rows in another order than its groups'", {
  one <- tree_slopes(8, 0.1, "a", "n1", seed = 5)
  two <- tree_slopes(8, 0.0, "b", "n2", seed = 6)
  groups <- list(n1 = pool_tree_group(one), n2 = pool_tree_group(two))
  shuffled <- dplyr::bind_rows(one[rev(seq_len(nrow(one))), ], two)
  expect_error(
    pool_tree_family(shuffled, groups),
    "not the rows, in order"
  )
  # The order of the groups themselves does not matter.
  expect_equal(
    pool_tree_family(dplyr::bind_rows(one, two), groups)$estimate,
    pool_tree_family(dplyr::bind_rows(one, two), rev(groups))$estimate
  )
})

test_that("the tree reports each level, odds ratios in their own direction", {
  registry <- list(
    tree = tibble::tibble(
      node = c("fam", "n1", "n2"),
      parent = c(NA, "fam", "fam"),
      level = c("L0", "L1", "L1")
    ),
    rows = tibble::tibble(
      variable_name = c("score_self", "score_parent", "bad_event", "alone"),
      node = c("n1", "n1", "n2", NA),
      informant = c("self", "parent", NA, "self"),
      direction = c(
        "higher_better",
        "higher_better",
        "higher_worse",
        "higher_worse"
      ),
      synthesis = c(TRUE, TRUE, TRUE, FALSE)
    )
  )
  slopes <- function(outcome, mean, seed, measure) {
    out <- tree_slopes(8, mean, outcome, NA, seed)
    out$node <- NULL
    out$exposure <- "st_watch"
    out$effect_measure <- measure
    out$status <- "fitted"
    out
  }
  stage1 <- dplyr::bind_rows(
    slopes("score_self", -0.05, 31, "sd_per_hour"),
    slopes("score_parent", -0.05, 32, "sd_per_hour"),
    slopes("bad_event", 0.09, 33, "log_odds_per_hour"),
    slopes("alone", 0.2, 34, "sd_per_hour")
  )
  correlations <- tibble::tibble(
    unit = unique(stage1$unit),
    exposure = "st_watch",
    outcome_a = "score_self",
    outcome_b = "score_parent",
    r = 0.4,
    overlap = 1
  )
  # Another exposure's correlations are not used for this one.
  other <- correlations
  other$exposure <- "st_game"
  other$r <- -0.9
  tree <- pool_tree(stage1, registry, dplyr::bind_rows(other, correlations))
  expect_equal(
    paste(tree$level, tree$node, tree$informant),
    c(
      "L2 n1 parent",
      "L2 n1 self",
      "L2 n2 not recorded",
      "L1 n1 NA",
      "L1 n2 NA",
      "L0 NA NA"
    )
  )
  expect_equal(attr(tree, "left_out")$outcome, "alone")
  expect_equal(attr(tree, "left_out")$slopes, 16)

  # The specific outcome uses the correlations given for its exposure.
  n1 <- tree[tree$level == "L1" & tree$node == "n1", ]
  both <- stage1[stage1$outcome %in% c("score_self", "score_parent"), ]
  both$flipped <- FALSE
  both <- both[order(both$unit), ]
  direct <- pool_tree_group(both, correlations)$row
  expect_equal(n1$estimate, direct$estimate)
  expect_equal(n1$se, direct$se)
  expect_equal(n1$pairs_assumed, 0L)
  expect_false(is.na(n1$tau_row))

  # The event is worse when higher: its aligned slope is negative, and its
  # odds ratio is the event's own, above one, as stage 2 reports the row.
  event <- tree[tree$level == "L2" & tree$node == "n2", ]
  event_rows <- stage1[stage1$outcome == "bad_event", ]
  event_rows$n <- 100
  row <- pool_stage2(event_rows)
  expect_lt(event$estimate, 0)
  expect_equal(event$odds_ratio, exp(row$estimate))
  expect_equal(event$odds_ratio_low, exp(row$ci_low))
  expect_equal(event$odds_ratio_high, exp(row$ci_high))
  expect_true(all(is.na(tree$odds_ratio[tree$node %in% "n1"])))

  l0 <- tree[tree$level == "L0", ]
  expect_equal(
    l0$estimate,
    mean(tree$estimate[tree$level == "L1"])
  )
})

# A prepared dataset of one unit with two outcome rows of one specific
# outcome, whose scores correlate at `rho` once screen use is taken out.
tree_prepared <- function(n, rho, seed, slope = 0.1) {
  set.seed(seed)
  x <- stats::rgamma(n, shape = 2)
  shared <- stats::rnorm(n)
  a <- slope * x + sqrt(rho) * shared + sqrt(1 - rho) * stats::rnorm(n)
  b <- slope * x + sqrt(rho) * shared + sqrt(1 - rho) * stats::rnorm(n)
  list(
    base = tibble::tibble(
      dataset_id = "9001",
      unit = "9001|1|Nowhere",
      age_years = stats::runif(n, 8, 12),
      sex = sample(c("Female", "Male"), n, replace = TRUE),
      survey_weight_norm = NA_real_
    ),
    exposures = tibble::tibble(st_watch = x),
    outcomes = tibble::tibble(row_a = a, row_b = b),
    outcomes_z = tibble::tibble(row_a = a, row_b = b)
  )
}
tree_pair_registry <- list(
  rows = tibble::tibble(variable_name = c("row_a", "row_b"), node = "n1")
)
tree_pair_cells <- tibble::tibble(
  exposure = "st_watch",
  outcome = c("row_a", "row_b")
)

test_that("the correlation between two slopes is recovered from the rows", {
  prepared <- tree_prepared(4000, rho = 0.5, seed = 21)
  full <- stage1_correlations(prepared, tree_pair_cells, tree_pair_registry)
  expect_equal(nrow(full), 1)
  expect_equal(full$basis, "estimated")
  expect_equal(full$n_shared, 4000)
  expect_equal(full$overlap, 1)
  expect_equal(full$r, 0.5, tolerance = 0.06)

  # Half the children lack the second score: the slopes share less.
  half <- prepared
  half$outcomes_z$row_b[1:2000] <- NA
  part <- stage1_correlations(half, tree_pair_cells, tree_pair_registry)
  expect_equal(part$n_shared, 2000)
  expect_equal(part$overlap, 2000 / sqrt(4000 * 2000))
  expect_equal(part$r, 0.5 / sqrt(2), tolerance = 0.08)

  # No child has both: independent slopes.
  apart <- half
  apart$outcomes_z$row_a[2001:4000] <- NA
  none <- stage1_correlations(apart, tree_pair_cells, tree_pair_registry)
  expect_equal(none$r, 0)
  expect_equal(none$basis, "no shared rows")

  # Too few children have both to estimate it: left for the default.
  few <- half
  few$outcomes_z$row_a[-(2001:2040)] <- NA
  few$outcomes_z$row_a[2021:2040] <- NA
  few$outcomes_z$row_a[1:40] <- prepared$outcomes_z$row_a[1:40]
  thin <- stage1_correlations(few, tree_pair_cells, tree_pair_registry)
  expect_equal(thin$basis, "too few shared rows")
  expect_equal(thin$n_shared, 20)
  expect_true(is.na(thin$r))
  expect_equal(thin$overlap, 20 / sqrt(60 * 2000))

  # A score that does not vary cannot be fitted, so it has no pair.
  flat <- prepared
  flat$outcomes_z$row_b <- 1
  expect_null(stage1_correlations(flat, tree_pair_cells, tree_pair_registry))
})

test_that("the correlation uses the rows and terms stage 1 would", {
  # Age and sex move both scores. A few children of unknown sex or age must
  # not take the term out of the model: they are left out, as in stage 1.
  set.seed(22)
  prepared <- tree_prepared(3000, rho = 0.3, seed = 23)
  female <- prepared$base$sex == "Female"
  shift <- 0.8 * female + 0.5 * (prepared$base$age_years - 10)
  prepared$outcomes_z$row_a <- prepared$outcomes_z$row_a + shift
  prepared$outcomes_z$row_b <- prepared$outcomes_z$row_b + shift
  clean <- stage1_correlations(prepared, tree_pair_cells, tree_pair_registry)
  expect_equal(clean$r, 0.3, tolerance = 0.15)
  prepared$base$sex[1:5] <- "Other"
  prepared$base$age_years[6:10] <- NA
  dirty <- stage1_correlations(prepared, tree_pair_cells, tree_pair_registry)
  expect_equal(dirty$n_shared, 2990)
  expect_equal(dirty$r, clean$r, tolerance = 0.02)
})

# The two rows' slopes over many samples, against the correlation estimated
# within each sample.
tree_slope_draws <- function(reps, make) {
  draws <- lapply(seq_len(reps), function(seed) {
    prepared <- make(seed)
    x <- prepared$exposures$st_watch
    age <- prepared$base$age_years
    sex <- prepared$base$sex
    slope <- function(y) {
      stats::coef(stats::lm(y ~ x + splines::ns(age, df = 2) + sex))[["x"]]
    }
    estimated <- stage1_correlations(
      prepared,
      tree_pair_cells,
      tree_pair_registry
    )
    c(
      a = slope(prepared$outcomes_z$row_a),
      b = slope(prepared$outcomes_z$row_b),
      r = estimated$r
    )
  })
  do.call(rbind, draws)
}

test_that("the estimate matches how two slopes vary together over samples", {
  # Half the children lack the second score.
  draws <- tree_slope_draws(400, function(seed) {
    prepared <- tree_prepared(300, rho = 0.6, seed = seed)
    prepared$outcomes_z$row_b[1:150] <- NA
    prepared
  })
  expect_equal(
    mean(draws[, "r"]),
    stats::cor(draws[, "a"], draws[, "b"]),
    tolerance = 0.15
  )
  expect_equal(mean(draws[, "r"]), 0.6 / sqrt(2), tolerance = 0.05)
})

test_that("the estimate allows for clusters that share screen use and error", {
  # Thirty schools of twenty: screen use and both scores each have a school
  # part, so the slopes err together far more than the children's scores do.
  draws <- tree_slope_draws(300, function(seed) {
    prepared <- tree_prepared(600, rho = 0.2, seed = seed)
    school <- rep(seq_len(30), each = 20)
    set.seed(1000 + seed)
    prepared$base$survey_cluster <- as.character(school)
    prepared$exposures$st_watch <- prepared$exposures$st_watch +
      stats::rnorm(30, 0, 1.5)[school]
    shared <- stats::rnorm(30, 0, 1)[school] *
      (prepared$exposures$st_watch - mean(prepared$exposures$st_watch))
    prepared$outcomes_z$row_a <- prepared$outcomes_z$row_a + 0.3 * shared
    prepared$outcomes_z$row_b <- prepared$outcomes_z$row_b + 0.3 * shared
    prepared
  })
  truth <- stats::cor(draws[, "a"], draws[, "b"])
  expect_gt(truth, 0.5)
  expect_equal(mean(draws[, "r"]), truth, tolerance = 0.15)
})

test_that("the tree's settings hold everything its functions read", {
  # The pipeline hands these functions `bp_tree_settings()` only, so a
  # setting read but not listed would be NULL there.
  files <- file.path(
    repo_root,
    "R/analysis",
    c(
      "tree_effects.R",
      "tree_covariance.R",
      "pool_tree_group.R",
      "pool_tree_family.R",
      "pool_tree.R",
      "stage1_correlations.R"
    )
  )
  code <- unlist(lapply(files, readLines))
  read <- unique(unlist(regmatches(
    code,
    gregexpr("(?<=settings\\$)[a-z_0-9]+", code, perl = TRUE)
  )))
  expect_setequal(read, bp_tree_settings())
  expect_true(all(bp_tree_settings() %in% names(analysis_settings())))
})

test_that("uncertainty alone does not keep a family from the headline", {
  # Two specific outcomes on three datasets each: too few studies to say,
  # and still in the headline, because nothing shows they disagree.
  one <- tree_slopes(3, 0.05, "a", "n1", seed = 41)
  two <- tree_slopes(3, 0.05, "b", "n2", seed = 42)
  family <- pool_tree_family(
    dplyr::bind_rows(one, two),
    list(n1 = pool_tree_group(one), n2 = pool_tree_group(two))
  )
  expect_equal(family$status, "too few studies to say")
  expect_equal(family$k_nodes_thin, 2)
  expect_true(is.na(family$warning))
  expect_true(family$headline)
})
