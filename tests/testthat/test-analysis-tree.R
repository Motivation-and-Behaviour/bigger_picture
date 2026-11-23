# Pooling up the outcome tree (ANALYSIS_PLAN.md, Step 7 task 1): known-answer
# tests on slopes built by hand, because a wrong pooling rule would not show
# on real data.

skip_unless_engine_needed()
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
  # one node does. Its estimate is that node's, and its standard error is
  # no smaller: a family of several specific outcomes takes the jackknife
  # form of the robust variance, which is a little larger.
  one <- tree_slopes(8, 0.1, "a", "n1", seed = 5)
  two <- one
  two$outcome <- "a_again"
  two$node <- "n2"
  groups <- list(n1 = pool_tree_group(one), n2 = pool_tree_group(two))
  family <- pool_tree_family(dplyr::bind_rows(one, two), groups)
  node <- groups$n1$row
  expect_equal(family$estimate, node$estimate)
  expect_gte(family$se, node$se)
  expect_lt(family$se, 1.2 * node$se)
  expect_lte(family$ci_low, node$ci_low)
  expect_gte(family$ci_high, node$ci_high)
  # Too few studies to say is judged on the degrees of freedom the
  # family's own interval uses.
  between <- analysis_settings()
  between$min_robust_df <- family$df + 0.01
  expect_equal(
    pool_tree_family(dplyr::bind_rows(one, two), groups, between)$status,
    "too few studies to say"
  )
  between$min_robust_df <- family$df - 0.01
  expect_equal(
    pool_tree_family(dplyr::bind_rows(one, two), groups, between)$status,
    "pooled"
  )
  # A family of one specific outcome is that outcome exactly.
  alone <- pool_tree_family(one, groups["n1"])
  expect_equal(alone$se, node$se)
  expect_equal(alone$df, node$df)
  expect_equal(alone$ci_low, node$ci_low)
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
      "stage1_correlations.R",
      "fit_tree_model.R",
      "tree_moderators.R",
      "moderate_tree_group.R",
      "moderate_tree.R",
      # The results table is assembled with the tree's settings, and rates
      # each effect with them.
      "assemble_results.R",
      "rate_effect.R"
    )
  )
  code <- unlist(lapply(files, readLines))
  read <- unique(unlist(regmatches(
    code,
    gregexpr("(?<=settings\\$)[a-z_0-9]+", code, perl = TRUE)
  )))
  expect_true(all(read %in% bp_tree_settings()))
  # `stage1_correlations()` also prepares rows with `stage1_design()`.
  design <- readLines(file.path(repo_root, "R/analysis/fit_stage1.R"))
  design <- unique(unlist(regmatches(
    design,
    gregexpr("(?<=settings\\$)[a-z_0-9]+", design, perl = TRUE)
  )))
  expect_true(all(
    c("sex_known_min", "covariate_known_min") %in% bp_tree_settings()
  ))
  expect_setequal(
    c(read, "sex_known_min", "covariate_known_min"),
    bp_tree_settings()
  )
  expect_true(all(bp_tree_settings() %in% names(analysis_settings())))
  # Stage 1 and the tier comparison get `bp_model_settings()` only, and
  # stage 1 the imputation settings too where it imputes.
  expect_true(all(design %in% bp_imputed_model_settings()))
  expect_setequal(
    setdiff(design, bp_model_settings()),
    "impute_exposure_models"
  )
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

test_that("moderator labels come from the registry, the unit and the mapping", {
  effects <- tibble::tibble(
    dataset_id = c("1", "1", "2", "2", "3"),
    unit = c("u1", "u1", "u2", "u2", "u3"),
    exposure = c(
      "st_watch",
      "st_watch",
      "st_watch",
      "total_screen",
      "st_watch"
    ),
    outcome = c("o_self", "o_parent", "o_self", "o_task", "o_mixed"),
    informant = c("self", "parent", "self", "task", "mixed"),
    score_type = "continuous"
  )
  units <- tibble::tibble(
    unit = c("u1", "u2", "u3"),
    measure_type = c("Categorical", "Continuous", "Categorical"),
    responder = c("Self", "Parent or carer", NA),
    total_source = c(NA, "summed", NA)
  )
  status <- tibble::tibble(
    dataset_id = c("1", "1", "2"),
    target_variable = c("o_self", "st_watch", "o_self"),
    status = c("compatible", "partial", "proximate")
  )
  datasets <- tibble::tibble(
    dataset_id = c("1", "2"),
    risk_of_bias = c("low", NA)
  )
  out <- tree_moderators(effects, units, status, datasets)
  expect_true(all(bp_tree_moderators() %in% names(out)))
  # The child reports both, a parent the outcome only, a parent the screen
  # use only, a test against a parent's report, and a mixed informant.
  expect_equal(
    out$reporter,
    c(
      "same reporter",
      "different reporter",
      "different reporter",
      "different reporter",
      NA
    )
  )
  expect_equal(out$measure_type[c(1, 3)], c("Categorical", "Continuous"))
  # A row with no mapping of its own is one the analysis derives.
  expect_equal(
    out$outcome_status,
    c("compatible", "derived", "proximate", "derived", "derived")
  )
  expect_equal(
    out$exposure_status,
    c("partial", "partial", "derived", "derived", "derived")
  )
  # A mixed informant is not a kind of informant to compare.
  expect_equal(out$informant, c("self", "parent", "self", "task", NA))
  # The source of the total applies to the derived total only.
  expect_equal(out$total_source, c(NA, NA, NA, "summed", NA))
  expect_equal(out$risk_of_bias, c("low", "low", NA, NA, NA))
  expect_equal(out$instrument, effects$outcome)

  variables <- tibble::tibble(
    target_variable = c("a", "b", "st_watch", "st_watch"),
    status = c("compatible", "unavailable", "partial", "compatible"),
    measure = c(NA, NA, "", "diary")
  )
  listed <- list_mapping_status(variables, 7)
  expect_equal(listed$target_variable, c("a", "st_watch"))
  expect_equal(listed$status, c("compatible", "partial"))
  expect_equal(listed$dataset_id, c("7", "7"))
})

test_that("a moderator's levels are compared, with a known difference", {
  # Twelve datasets report by the child (mean 0.02) and twelve by a parent
  # (mean 0.12); four of them give both; one more gives a third level.
  self <- tree_slopes(12, 0.02, "o_self", "n1", seed = 51, tau = 0.01)
  parent <- tree_slopes(12, 0.12, "o_parent", "n1", seed = 52, tau = 0.01)
  parent$dataset_id <- sub("d", "p", parent$dataset_id)
  parent$dataset_id[1:8] <- self$dataset_id[1:8]
  parent$unit <- paste0(parent$dataset_id, sub("^.[0-9]+", "", parent$unit))
  teacher <- tree_slopes(1, 0.5, "o_teacher", "n1", seed = 53)
  teacher$dataset_id <- "t01"
  teacher$unit <- sub("d01", "t01", teacher$unit)
  effects <- dplyr::bind_rows(self, parent, teacher)
  effects$informant <- sub("o_", "", effects$outcome)
  effects <- effects[order(effects$unit), ]
  out <- moderate_tree_group(effects, "informant")
  expect_equal(out$level, c("parent", "self", "teacher"))
  expect_equal(out$k_datasets, c(12, 12, 1))
  expect_equal(out$status, rep("compared", 3))
  expect_equal(unique(out$k_levels), 2)
  expect_equal(unique(out$k_datasets_several), 4)
  expect_equal(unique(out$largest_dataset_share), 4 / 48)
  expect_equal(unique(out$difference_of), "self minus parent")
  expect_equal(unique(out$test_status), "tested")
  expect_gt(out$equal_df[1], 4)
  # The level with one dataset is counted and not estimated.
  expect_equal(out$level_status[3], "insufficient data")
  expect_true(is.na(out$estimate[3]))
  expect_equal(out$estimate[1:2], c(0.12, 0.02), tolerance = 0.03, scale = 1)
  expect_true(all(out$ci_low[1:2] < c(0.12, 0.02)))
  expect_true(all(out$ci_high[1:2] > c(0.12, 0.02)))
  # Second minus first: the child's report minus the parent's.
  expect_equal(out$difference[1], out$estimate[2] - out$estimate[1])
  expect_equal(out$difference[1], -0.10, tolerance = 0.03, scale = 1)
  expect_lt(out$difference_ci_high[1], 0)
  expect_lt(out$equal_p[1], 0.001)

  # The same mean at both levels: no difference found.
  parent$estimate <- parent$estimate - 0.10
  effects <- dplyr::bind_rows(self, parent)
  effects$informant <- sub("o_", "", effects$outcome)
  effects <- effects[order(effects$unit), ]
  same <- moderate_tree_group(effects, "informant")
  expect_gt(same$equal_p[1], 0.05)
  expect_true(same$difference_ci_low[1] < 0 && same$difference_ci_high[1] > 0)

  # One level only, or none: said, not estimated.
  one <- moderate_tree_group(
    effects[effects$informant == "self", ],
    "informant"
  )
  expect_equal(one$status, "fewer than two levels with enough datasets")
  effects$informant <- NA
  none <- moderate_tree_group(effects, "informant")
  expect_equal(nrow(none), 1)
  expect_true(is.na(none$level))
  expect_equal(none$status, "no slope has this label")
})

test_that("three levels are tested together", {
  parts <- lapply(1:3, function(i) {
    out <- tree_slopes(10, c(0, 0, 0.15)[i], paste0("o", i), "n1", 60 + i, 0.01)
    out$dataset_id <- paste0(out$dataset_id, letters[i])
    out$unit <- paste0(out$unit, letters[i])
    out$score_type <- c("binary", "continuous", "ordinal")[i]
    out
  })
  out <- moderate_tree_group(dplyr::bind_rows(parts), "score_type")
  expect_equal(unique(out$k_levels), 3)
  expect_lt(out$equal_p[1], 0.01)
  # A difference is given only between two levels.
  expect_true(all(is.na(out$difference)))
  expect_equal(out$estimate, c(0, 0, 0.15), tolerance = 0.04, scale = 1)
})

test_that("a teacher's report of both counts as the same reporter", {
  effects <- tibble::tibble(
    dataset_id = "1",
    unit = c("u1", "u2", "u3", "u4"),
    exposure = "st_watch",
    outcome = "o",
    informant = c("teacher", "self", "task", "parent"),
    score_type = "continuous"
  )
  units <- tibble::tibble(
    unit = c("u1", "u2", "u3", "u4"),
    measure_type = "Categorical",
    responder = c("Teacher", "Device or objective measure", "Self", "Unknown"),
    total_source = NA
  )
  expect_equal(
    tree_moderators(effects, units)$reporter,
    c("same reporter", "different reporter", "different reporter", NA)
  )
})

test_that("only the compared levels count towards the within-study basis", {
  # Three datasets give a third level alongside the first, but it has too
  # few datasets to be compared, so they are not within-study comparisons.
  a <- tree_slopes(6, 0.05, "o_a", "n1", seed = 71)
  b <- tree_slopes(6, 0.05, "o_b", "n1", seed = 72)
  b$dataset_id <- sub("d", "e", b$dataset_id)
  b$unit <- sub("d", "e", b$unit)
  # Sorts between the two compared levels.
  aa <- tree_slopes(2, 0.05, "o_aa", "n1", seed = 73)
  effects <- dplyr::bind_rows(a, b, aa)
  effects$instrument <- effects$outcome
  effects <- effects[order(effects$unit), ]
  out <- moderate_tree_group(effects, "instrument")
  expect_equal(out$level, c("o_a", "o_aa", "o_b"))
  expect_equal(out$level_status[2], "insufficient data")
  expect_equal(unique(out$k_datasets_several), 0)
  expect_equal(unique(out$difference_of), "o_b minus o_a")
  expect_equal(out$difference[1], out$estimate[3] - out$estimate[1])
})

test_that("a comparison resting on few studies says so on the test", {
  a <- tree_slopes(3, 0.0, "o_a", "n1", seed = 81, tau = 0.001)
  b <- tree_slopes(3, 0.2, "o_b", "n1", seed = 82, tau = 0.001)
  b$dataset_id <- sub("d", "e", b$dataset_id)
  b$unit <- sub("d", "e", b$unit)
  effects <- dplyr::bind_rows(a, b)
  effects$instrument <- effects$outcome
  out <- moderate_tree_group(effects, "instrument")
  expect_equal(unique(out$status), "compared")
  # The test has between three and four degrees of freedom: enough under
  # the minimum of three, and too few under a stricter one.
  expect_gte(out$equal_df[1], 3)
  expect_lt(out$equal_df[1], 4)
  expect_equal(unique(out$test_status), "tested")
  strict <- analysis_settings()
  strict$min_robust_df <- 4
  out <- moderate_tree_group(effects, "instrument", settings = strict)
  expect_equal(unique(out$test_status), "too few studies to say")
})

test_that("the equality test rejects equal levels about as often as it says", {
  # Two levels with one true mean, eight datasets each, two of them shared.
  p <- vapply(
    1:120,
    function(seed) {
      a <- tree_slopes(8, 0.05, "o_a", "n1", seed = 1000 + seed)
      b <- tree_slopes(8, 0.05, "o_b", "n1", seed = 2000 + seed)
      b$dataset_id <- sub("d0([3-8])", "e0\\1", b$dataset_id)
      b$unit <- sub("d0([3-8])", "e0\\1", b$unit)
      effects <- dplyr::bind_rows(a, b)
      effects$instrument <- effects$outcome
      effects <- effects[order(effects$unit), ]
      moderate_tree_group(effects, "instrument")$equal_p[1]
    },
    numeric(1)
  )
  expect_lt(mean(p < 0.05), 0.12)
  expect_gt(mean(p < 0.5), 0.3)
})

test_that("every moderator of every specific outcome is on record", {
  registry <- list(
    tree = tibble::tibble(
      node = c("fam", "n1"),
      parent = c(NA, "fam"),
      level = c("L0", "L1")
    ),
    rows = tibble::tibble(
      variable_name = c("score_self", "score_parent"),
      node = "n1",
      informant = c("self", "parent"),
      direction = "higher_better",
      score_type = "continuous",
      synthesis = TRUE
    )
  )
  slopes <- function(outcome, mean, seed) {
    out <- tree_slopes(8, mean, outcome, NA, seed)
    out$node <- NULL
    out$exposure <- "st_watch"
    out$score_type <- "continuous"
    out$effect_measure <- "sd_per_hour"
    out$status <- "fitted"
    out
  }
  stage1 <- dplyr::bind_rows(
    slopes("score_self", 0.02, 91),
    slopes("score_parent", 0.12, 92)
  )
  units <- tibble::tibble(
    unit = unique(stage1$unit),
    measure_type = "Categorical",
    responder = "Self",
    total_source = NA
  )
  out <- moderate_tree(stage1, units, registry)
  # Specific outcomes only, unless the family level is asked for.
  expect_equal(unique(out$tree_level), "L1")
  expect_setequal(
    moderate_tree(stage1, units, registry, levels = c("L1", "L0"))$tree_level,
    c("L1", "L0")
  )
  expect_setequal(out$moderator, bp_tree_moderators())
  status <- function(name) unique(out$status[out$moderator == name])
  expect_equal(status("informant"), "compared")
  expect_equal(
    status("measure_type"),
    "fewer than two levels with enough datasets"
  )
  expect_equal(status("risk_of_bias"), "no slope has this label")
  # Instrument and reporter sort these slopes exactly as informant does.
  same <- function(name) unique(out$same_as[out$moderator == name])
  expect_true(is.na(same("informant")))
  expect_equal(same("reporter"), "informant")
  expect_equal(same("instrument"), "informant")
  expect_true(is.na(same("risk_of_bias")))
})

test_that("a family's moderator test allows for its specific outcomes differing", {
  # Two specific outcomes 0.3 apart. In each, parent-reported slopes are
  # 0.1 above self-reported ones; most of the first outcome's slopes are
  # self-reported and most of the second's parent-reported, so without a
  # term for the specific outcome the levels would differ by far more.
  part <- function(mean, outcome, node, k, seed, prefix) {
    rows <- tree_slopes(k, mean, outcome, node, seed = seed, tau = 0.01)
    rows$dataset_id <- sub("d", prefix, rows$dataset_id)
    rows$unit <- sub("d", prefix, rows$unit)
    rows
  }
  effects <- dplyr::bind_rows(
    part(0.00, "a_self", "n1", 12, 71, "a"),
    part(0.10, "a_parent", "n1", 4, 72, "b"),
    part(0.30, "b_self", "n2", 4, 73, "c"),
    part(0.40, "b_parent", "n2", 12, 74, "e")
  )
  effects$informant <- sub("^._", "", effects$outcome)
  effects <- effects[order(effects$unit), ]
  crude <- moderate_tree_group(effects, "informant")
  expect_gt(abs(crude$difference[1]), 0.2)
  family <- moderate_tree_group(effects, "informant", adjust = "node")
  expect_equal(family$status[1], "compared")
  expect_equal(abs(family$difference[1]), 0.10, tolerance = 0.03, scale = 1)
  expect_lt(family$equal_p[1], 0.01)
  # The levels' own means belong to one specific outcome, so none is given.
  expect_true(all(is.na(family$estimate)))
  expect_equal(unique(family$level_status), "compared")
  # A label that sorts the slopes as the specific outcomes do is not
  # tested.
  effects$sorted <- effects$node
  apart <- moderate_tree_group(effects, "sorted", adjust = "node")
  expect_equal(
    unique(apart$status),
    "moderator not separable from the specific outcomes"
  )
  expect_true(all(is.na(apart$equal_p)))
  # With one specific outcome the term is not needed, and the result is
  # the specific outcome's own.
  one <- effects[effects$node == "n1", ]
  expect_equal(
    moderate_tree_group(one, "informant", adjust = "node")$difference,
    moderate_tree_group(one, "informant")$difference
  )
})

test_that("the trend over years is recovered for a specific outcome and a family", {
  settings <- analysis_settings()
  # Fourteen datasets over 26 years; the slope falls by 0.05 a decade
  # between datasets, and the second specific outcome sits 0.3 higher.
  build <- function(mean, outcome, node, seed) {
    rows <- tree_slopes(14, mean, outcome, node, seed = seed, tau = 0.01)
    number <- as.integer(sub("d", "", rows$dataset_id))
    rows$year <- 1996 + 2 * number
    rows$estimate <- rows$estimate - 0.05 * (rows$year - 2010) / 10
    rows$series <- rows$unit
    rows$age_median <- 12
    rows$measure_type <- "Continuous"
    rows$responder <- "Self"
    rows
  }
  first <- build(0, "a", "n1", 81)
  second <- build(0.3, "b", "n2", 82)
  node <- trend_tree_group(first, settings = settings)
  expect_equal(node$status, "fitted")
  expect_equal(node$between_per_decade, -0.05, tolerance = 0.02, scale = 1)
  expect_lt(node$between_ci_high, 0)
  expect_equal(node$k_datasets, 14L)
  # No series spans more than one year here, so there is no within part.
  expect_true(is.na(node$within_per_decade))
  expect_equal(node$k_within, 0L)
  both <- dplyr::bind_rows(first, second)
  family <- trend_tree_group(both, settings = settings, adjust = "node")
  expect_equal(family$between_per_decade, -0.05, tolerance = 0.02, scale = 1)
  expect_match(family$adjusted_for, "specific outcome")
  expect_equal(family$k_slopes, 56L)
  # Too few datasets or too short a span: said, not fitted.
  few <- trend_tree_group(first[first$year <= 2002, ], settings = settings)
  expect_equal(few$status, "too few datasets or years")
  # A within-series trend: each dataset's two units are ten years apart.
  panel <- first
  panel$series <- panel$dataset_id
  second_unit <- duplicated(panel$dataset_id)
  panel$year[second_unit] <- panel$year[second_unit] + 10
  panel$estimate[second_unit] <- panel$estimate[second_unit] + 0.08
  within <- trend_tree_group(panel, settings = settings)
  expect_equal(within$k_within, 14L)
  expect_gt(within$within_per_decade, 0)
})

test_that("moderator tests become rows the results table can count", {
  moderation <- tibble::tibble(
    exposure = "st_watch",
    tree_level = c("L1", "L1", "L1", "L1", "L0", "L0"),
    family = "wellbeing",
    node = c("n1", "n1", "n1", "n1", NA, NA),
    moderator = c(
      "informant",
      "informant",
      "reporter",
      "instrument",
      "informant",
      "informant"
    ),
    level = c("parent", "self", "same reporter", "a", "parent", "self"),
    equal_p = c(0.02, 0.02, 0.4, 0.02, 0.2, 0.2),
    equal_df = c(8, 8, 2, 8, 9, 9),
    test_status = c(
      "tested",
      "tested",
      "too few studies to say",
      "tested",
      "tested",
      "tested"
    ),
    difference = c(0.1, 0.1, NA, NA, 0.05, 0.05),
    difference_ci_low = 0,
    difference_ci_high = 0.2,
    status = "compared",
    same_as = c(NA, NA, NA, "informant", NA, NA)
  )
  trend <- tibble::tibble(
    exposure = "st_watch",
    tree_level = "L1",
    family = "wellbeing",
    node = "n1",
    between_per_decade = -0.05,
    between_ci_low = -0.09,
    between_ci_high = -0.01,
    between_df = 10,
    between_status = "estimated",
    within_per_decade = NA_real_,
    within_ci_low = NA_real_,
    within_ci_high = NA_real_,
    within_df = NA_real_,
    within_status = NA_character_,
    status = "fitted"
  )
  tests <- moderator_tests(moderation, trend)
  # One row per test: the duplicate label is gone, and the reporter test
  # is the model list's own row.
  # The within-series trend is a description and is not a test.
  expect_equal(nrow(tests), 4)
  expect_equal(
    tests$model_name,
    c(
      "moderation_study_level",
      "sens_reporter",
      "moderation_study_level_family",
      "year_trend"
    )
  )
  expect_equal(tests$level, c("L1", "L1", "L0", "L1"))
  expect_equal(tests$term[4], "between datasets")
  expect_equal(
    tests$status,
    c("pooled", "too few studies to say", "pooled", "pooled")
  )
  # The trend's p-value is the one its interval and degrees of freedom
  # give.
  se <- 0.08 / (2 * stats::qt(0.975, 10))
  expect_equal(tests$p_test[4], 2 * stats::pt(-0.05 / se, 10))
  # A trend that was not fitted has no p-value.
  unfitted <- trend
  unfitted$between_per_decade <- NA_real_
  unfitted$between_status <- NA_character_
  unfitted$status <- "too few datasets or years"
  none <- moderator_tests(NULL, unfitted)
  expect_true(is.na(none$p_test))
  expect_equal(none$status, "too few datasets or years")
  expect_equal(nrow(moderator_tests(NULL, NULL)), 0)
})
