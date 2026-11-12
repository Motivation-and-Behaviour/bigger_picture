# The data manifest, the checks and the preparation step (ANALYSIS_PLAN.md,
# Steps 2 to 4), on the mock dataset and small hand-made tables, without the
# data mount.

mock <- make_mock_analysis_data(n_per_wave = 120)
mock_dataset <- function(id) {
  rows <- mock$data[mock$data$dataset_id == id, ]
  rows[] <- lapply(rows, function(x) if (is.factor(x)) as.character(x) else x)
  rows
}
mock_info <- function(id, ...) {
  info <- tibble::tibble(
    dataset_id = id,
    baseline_wave = NA_character_,
    signed_off = NA,
    notes = NA_character_,
    included = TRUE
  )
  extra <- list(...)
  for (name in names(extra)) {
    info[[name]] <- extra[[name]]
  }
  info
}
repo_registries <- function() {
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
no_exceptions <- function(reg) {
  read_analysis_exceptions(character(), reg$schema, reg$outcomes)
}
prepare_mock <- function(id, rows = mock_dataset(id), info = mock_info(id)) {
  reg <- repo_registries()
  prepare_analysis_data(
    rows,
    info,
    reg$outcomes,
    reg$exposures,
    reg$derivations,
    reg$schema,
    no_exceptions(reg)
  )
}

test_that("every dataset is included until one is signed off", {
  datasets <- tibble::tibble(
    dataset_id = c("1", "2"),
    signed_off = c(NA, NA)
  )
  expect_true(analysis_dataset_info(datasets, "1")$included)
  expect_true(analysis_dataset_info(datasets, "3")$included)

  datasets$signed_off <- c(TRUE, NA)
  expect_true(analysis_dataset_info(datasets, "1")$included)
  expect_false(analysis_dataset_info(datasets, "2")$included)
  expect_false(analysis_dataset_info(datasets, "3")$included)
})

test_that("an excluded dataset yields nothing anywhere", {
  rows <- mock_dataset("9991")
  out <- mock_info("9991", included = FALSE)
  reg <- repo_registries()
  expect_null(manifest_dataset(rows, out))
  expect_null(summarise_grain(rows, out))
  expect_null(summarise_numeric(rows, out, reg$schema))
  expect_null(summarise_levels(rows, out, reg$schema))
  expect_null(summarise_units(rows, out))
  expect_null(summarise_unit_counts(rows, out, reg$schema))
  expect_null(prepare_mock("9991", info = out))
})

test_that("the manifest counts a dataset and its hash follows the data", {
  rows <- mock_dataset("9991")
  info <- mock_info("9991")
  first <- manifest_dataset(rows, info)
  expect_equal(first$rows, nrow(rows))
  expect_equal(first$participants, dplyr::n_distinct(rows$participant_id))
  expect_identical(manifest_dataset(rows, info)$hash, first$hash)

  rows$st_game[1] <- rows$st_game[1] + 1
  changed <- manifest_dataset(rows, info)
  expect_false(identical(changed$hash, first$hash))

  other <- manifest_dataset(mock_dataset("9992"), mock_info("9992"))
  manifest <- combine_data_manifest(other, NULL, first)
  expect_equal(manifest$dataset_id, c("9991", "9992"))
  expect_length(unique(manifest$overall_hash), 1)
  expect_false(identical(
    combine_data_manifest(other, changed)$overall_hash[1],
    manifest$overall_hash[1]
  ))
})

test_that("a recorded manifest shows what was added, removed or changed", {
  a <- manifest_dataset(mock_dataset("9991"), mock_info("9991"))
  b <- manifest_dataset(mock_dataset("9992"), mock_info("9992"))
  c <- manifest_dataset(mock_dataset("9993"), mock_info("9993"))
  dir <- withr::local_tempdir()
  path <- record_data_manifest("test-record", combine_data_manifest(a, b), dir)
  expect_true(file.exists(path))
  expect_error(record_data_manifest("Bad Label", combine_data_manifest(a), dir))

  expect_equal(
    nrow(compare_data_manifest(combine_data_manifest(a, b), path)),
    0
  )
  b_changed <- b
  b_changed$hash <- "different"
  diff <- compare_data_manifest(combine_data_manifest(b_changed, c), path)
  diff <- diff[order(diff$dataset_id), ]
  expect_equal(diff$dataset_id, c("9991", "9992", "9993"))
  expect_equal(diff$change, c("removed", "changed", "added"))
  expect_equal(
    nrow(compare_data_manifest(combine_data_manifest(a), "no-such-file.csv")),
    0
  )
})

test_that("summaries describe a dataset without row-level data", {
  reg <- repo_registries()
  rows <- mock_dataset("9996")
  info <- mock_info("9996")

  grain <- summarise_grain(rows, info)
  expect_equal(sum(grain$rows), nrow(rows))
  expect_equal(sum(grain$duplicate_keys), 0)

  numeric <- summarise_numeric(rows, info, reg$schema)
  game <- numeric[numeric$variable_name == "st_game", ]
  expect_equal(game$n, sum(!is.na(rows$st_game)))
  expect_equal(game$median, stats::median(rows$st_game))
  expect_false("st_socmedia" %in% numeric$variable_name)

  levels <- summarise_levels(rows, info, reg$schema)
  sex <- levels[levels$variable_name == "sex", ]
  expect_equal(sum(sex$n), nrow(rows))
  # Identifiers are never tabulated, however few a dataset has.
  ids <- levels[levels$variable_name %in% bp_identifier_columns(), ]
  expect_setequal(ids$variable_name, c("participant_id", "survey_cluster"))
  expect_equal(unique(ids$level), "(many values)")
  few <- summarise_levels(rows, info, reg$schema, max_levels = 1)
  expect_equal(unique(few$level[few$variable_name == "sex"]), "(many values)")

  units <- summarise_units(rows, info)
  expect_setequal(units$country, c("Ireland", "Scotland"))
  expect_equal(sum(units$rows), nrow(rows))

  coverage <- summarise_coverage(numeric, levels, grain, reg$schema)
  expect_equal(coverage$share[coverage$variable_name == "st_game"], 1)
  counts <- summarise_unit_counts(rows, info, reg$schema)
  game <- counts[counts$variable_name == "st_game", ]
  expect_equal(sum(game$n_value), nrow(rows))
  expect_setequal(game$country, c("Ireland", "Scotland"))
  missing <- summarise_missingness(counts, units)
  expect_equal(sum(missing$n_systematic), 0)
})

test_that("a variable one country never measured is systematic there", {
  reg <- repo_registries()
  rows <- mock_dataset("9996")
  info <- mock_info("9996")
  rows$mh_dep_self[rows$country == "Scotland"] <- NA
  rows$mh_dep_self[which(rows$country == "Ireland")[1:4]] <- NA
  rows$country[which(rows$country == "Ireland")[5:7]] <- "Unknown"
  units <- summarise_units(rows, info)
  expect_equal(sum(is.na(units$country)), 1)
  missing <- summarise_missingness(
    summarise_unit_counts(rows, info, reg$schema),
    units
  )
  dep <- missing[missing$variable_name == "mh_dep_self", ]
  expect_equal(dep$n_systematic, sum(rows$country == "Scotland"))
  expect_equal(dep$n_sporadic, 4)
  expect_equal(dep$n_value + dep$n_sporadic + dep$n_systematic, nrow(rows))
})

test_that("a screen-time row held only on an extra measure is still covered", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  info <- mock_info("9995")
  diary <- rows[1:30, ]
  diary$st_measure_id <- "diary"
  diary$st_educational <- 1
  rows <- dplyr::bind_rows(rows, diary)
  coverage <- summarise_coverage(
    summarise_numeric(rows, info, reg$schema),
    summarise_levels(rows, info, reg$schema),
    summarise_grain(rows, info),
    reg$schema
  )
  edu <- coverage[coverage$variable_name == "st_educational", ]
  expect_equal(edu$n_value, 0)
  expect_equal(edu$n_value_other_measures, 30)
  watch <- coverage[coverage$variable_name == "st_watch", ]
  expect_equal(watch$n_value, nrow(rows) - 30)
  expect_equal(watch$share, 1)
})

test_that("the dataset flow counts specs by status from their files", {
  dir <- withr::local_tempdir()
  write_spec <- function(id, status) {
    path <- file.path(dir, paste0(id, ".yaml"))
    yaml::write_yaml(list(dataset_id = id, status = status), path)
    path
  }
  files <- c(
    write_spec("1", "harmonised"),
    write_spec("2", "blocked"),
    write_spec("3", "in_progress")
  )
  manifest <- tibble::tibble(participants = 10, primary_rows = 20)
  flow <- summarise_dataset_flow(files, c("1", "3"), manifest)
  count <- function(stage) flow$datasets[flow$stage == stage]
  expect_equal(count("Spec written"), 3)
  expect_equal(count("Spec status: blocked"), 1)
  expect_equal(count("In the pipeline"), 2)
  expect_equal(count("Included in the analysis"), 1)
})

test_that("Unknown counts as missing, sporadic where the unit has values", {
  reg <- repo_registries()
  rows <- mock_dataset("9991")
  info <- mock_info("9991")
  rows$sex[rows$wave == "2"] <- "Unknown"
  rows$sex[which(rows$wave == "1")[1:5]] <- "Unknown"
  grain <- summarise_grain(rows, info)
  levels <- summarise_levels(rows, info, reg$schema)
  numeric <- summarise_numeric(rows, info, reg$schema)

  coverage <- summarise_coverage(numeric, levels, grain, reg$schema)
  sex <- coverage[coverage$variable_name == "sex", ]
  expect_equal(sex$n_unknown, sum(rows$sex == "Unknown"))
  expect_equal(sex$n_value, sum(rows$sex != "Unknown"))

  missing <- summarise_missingness(
    summarise_unit_counts(rows, info, reg$schema),
    summarise_units(rows, info)
  )
  sex <- missing[missing$variable_name == "sex", ]
  expect_equal(sex$n_sporadic, 5)
  expect_equal(sex$n_systematic, sum(rows$wave == "2"))
})

run_checks <- function(rows, info, invariants) {
  reg <- repo_registries()
  check_analysis_data(
    summarise_numeric(rows, info, reg$schema),
    summarise_levels(rows, info, reg$schema),
    summarise_grain(rows, info),
    check_invariants(rows, info, invariants),
    dplyr::bind_rows(
      tibble::tibble(dataset_id = character(), n_differs = integer()),
      check_day_consistency(rows, info, reg$exposures)
    ),
    invariants,
    reg$schema,
    reg$exposures
  )
}
# The mock draws a cluster per row, so it is not declared constant here.
mock_invariants <- tibble::tibble(
  variable_name = c("sex", "country"),
  rule = c("constant", "carried")
)

test_that("the checks find what they name", {
  rows <- mock_dataset("9991")
  info <- mock_info("9991")
  rows$st_game[1:3] <- 30
  rows$mh_dep_self[1:2] <- 500
  rows$age_years[4] <- 40
  rows$sex[rows$participant_id == rows$participant_id[5]] <- "Not listed"
  rows$st_socmedia <- 0
  rows <- dplyr::bind_rows(rows, rows[10, ])
  person <- rows$participant_id[20]
  rows$sex[rows$participant_id == person] <- c("Female", "Male")
  mover <- rows$participant_id[30]
  rows$country[rows$participant_id == mover] <- c("Australia", "England")
  gap <- rows$participant_id[40]
  rows$country[rows$participant_id == gap][2] <- "Unknown"

  findings <- run_checks(rows, info, mock_invariants)
  found <- function(check, variable = NULL) {
    hit <- findings$check == check
    if (!is.null(variable)) {
      hit <- hit & findings$variable_name %in% variable
    }
    sum(findings$n[hit])
  }
  severity <- function(check, variable) {
    unique(findings$severity[
      findings$check == check & findings$variable_name %in% variable
    ])
  }
  expect_equal(found("above_max", "st_game"), 3)
  expect_equal(severity("above_max", "st_game"), "review")
  expect_equal(found("above_max", "age_years"), 1)
  expect_equal(severity("above_max", "age_years"), "advisory")
  expect_equal(found("outside_allowed_values", "sex"), 2)
  expect_equal(found("screen_all_zero", "st_socmedia"), nrow(rows))
  expect_equal(found("duplicate_key"), 1)
  expect_equal(found("invariant_varies", "sex"), 1)
  expect_equal(found("invariant_varies", "country"), 0)
  expect_equal(found("carried_value_changes", "country"), 1)
  expect_equal(severity("carried_value_changes", "country"), "advisory")
  expect_equal(found("carry_gap", "country"), 1)
  expect_equal(found("carry_gap", "sex"), 0)
  expect_true(all(findings$severity %in% c("review", "advisory")))

  clean <- run_checks(mock_dataset("9992"), mock_info("9992"), mock_invariants)
  expect_equal(clean$check[clean$severity == "review"], character())
})

test_that("usual-day values are checked row by row against the week", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  info <- mock_info("9995")
  n <- nrow(rows)
  rows$st_watch_wd <- stats::runif(n, 0, 4)
  rows$st_watch_we <- stats::runif(n, 0, 6)
  rows$st_watch <- (5 * rows$st_watch_wd + 2 * rows$st_watch_we) / 7
  # Weekday and weekend asked of a subsample: the means differ, the rows agree.
  rows$st_watch_wd[101:n] <- NA
  rows$st_watch_we[101:n] <- NA
  rows$st_watch[101:n] <- 9
  agree <- check_day_consistency(rows, info, reg$exposures)
  expect_equal(sum(agree$n), 100)
  expect_equal(sum(agree$n_differs), 0)
  findings <- run_checks(rows, info, mock_invariants)
  expect_false("usual_day_mismatch" %in% findings$check)

  rows$st_watch[1:7] <- rows$st_watch[1:7] + 1
  findings <- run_checks(rows, info, mock_invariants)
  expect_equal(findings$n[findings$check == "usual_day_mismatch"], 7)
  expect_null(check_day_consistency(mock_dataset("9992"), mock_info("9992"), reg$exposures))
})

test_that("a dataset on another scale stands out from the others", {
  reg <- repo_registries()
  ids <- as.character(9991:9996)
  numeric <- dplyr::bind_rows(lapply(ids, function(id) {
    rows <- mock_dataset(id)
    if (id == "9996") {
      rows$st_game <- rows$st_game * 10
    }
    summarise_numeric(rows, mock_info(id), reg$schema)
  }))
  empty <- function(...) tibble::tibble(dataset_id = character(), ...)
  findings <- check_analysis_data(
    numeric,
    empty(variable_name = character(), level = character(), n = integer()),
    empty(duplicate_keys = integer(), missing_ids = integer()),
    empty(
      variable_name = character(),
      participants_varying = integer(),
      participants_gap = integer()
    ),
    empty(n_differs = integer()),
    mock_invariants,
    reg$schema,
    reg$exposures
  )
  scale <- findings[findings$check == "screen_scale", ]
  expect_equal(scale$dataset_id, "9996")
  expect_equal(scale$variable_name, "st_game")
})

test_that("rows under the age limit are kept and the rest are counted", {
  rows <- mock_dataset("9995")
  rows$age_years[1:4] <- c(19, 25, NA, 18.9)
  prepared <- prepare_base(rows, mock_info("9995"), names(rows))
  expect_equal(nrow(prepared$base), nrow(rows) - 3)
  expect_equal(sum(prepared$keep), nrow(rows) - 3)
  expect_true(all(prepared$base$age_years < 19))
  dropped <- prepared$dropped
  expect_equal(dropped$n[dropped$reason == "age_over_limit"], 2)
  expect_equal(dropped$n[dropped$reason == "age_missing"], 1)
})

test_that("trials keep baseline rows and control arms, and need a baseline", {
  rows <- mock_dataset("9991")
  people <- unique(rows$participant_id)
  arms <- rep(c("Control", "Intervention"), length.out = length(people))
  rows$rct_arm <- arms[match(rows$participant_id, people)]
  expect_error(
    prepare_base(rows, mock_info("9991"), names(rows)),
    "baseline_wave"
  )
  with_baseline <- prepare_base(
    rows,
    mock_info("9991", baseline_wave = "1"),
    names(rows)
  )
  base <- with_baseline$base
  expect_true(all(base$rct_arm[base$wave == "2"] == "Control"))
  expect_setequal(base$rct_arm[base$wave == "1"], c("Control", "Intervention"))
  expect_true(all(base$is_baseline[base$wave == "1"]))

  no_baseline <- prepare_base(
    rows,
    mock_info("9991", notes = "One post-intervention occasion"),
    names(rows)
  )
  expect_true(all(no_baseline$base$rct_arm == "Control"))
})

test_that("the base table recodes Unknown and derives its columns", {
  rows <- mock_dataset("9991")
  rows$sex[1:3] <- "Unknown"
  rows$wave <- ifelse(rows$wave == "1", "later", "earlier")
  rows$data_year <- ifelse(rows$wave == "later", 2020, 2015)
  rows$survey_weight[rows$wave == "later"] <-
    rows$survey_weight[rows$wave == "later"] * 1000
  base <- prepare_base(rows, mock_info("9991"), names(rows))$base

  expect_equal(sum(is.na(base$sex)), 3)
  expect_equal(unique(base$wave_order[base$wave == "earlier"]), 1L)
  expect_equal(unique(base$wave_order[base$wave == "later"]), 2L)
  expect_equal(dplyr::n_distinct(base$unit), 2)
  unit_means <- tapply(base$survey_weight_norm, base$unit, mean)
  expect_equal(unname(as.numeric(unit_means)), c(1, 1))
  expect_true(all(base$school_stage %in% analysis_settings()$school_stage_labels))
})

test_that("total screen time is the dedicated item, else a guarded sum", {
  reg <- repo_registries()
  exposures <- tibble::tibble(
    st_total = c(5, 6, NA, NA, NA, NA),
    st_watch = c(1, 1, 2, NA, 20, 1),
    st_game = c(1, 1, 3, 1, 20, 1),
    st_socmedia = c(1, 1, NA, 1, 1, NA),
    st_messaging = c(4, 4, 4, 4, 4, 4),
    st_phone = c(9, 9, 9, 9, 9, 9)
  )
  unit <- c("a", "a", "b", "b", "b", "c")
  total <- derive_total_screen(exposures, unit, reg$exposures)

  expect_equal(total$total_source, c(rep("dedicated", 2), rep("summed", 4)))
  expect_equal(total$total_screen[1:2], c(5, 6))
  # Unit b sums the content classes it has: complete cases only, capped.
  expect_equal(total$total_screen[3:5], c(NA, NA, 24))
  expect_equal(total$total_capped, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(total$total_coverage[3], 3L)
  expect_equal(total$total_rows[3], "st_watch|st_game|st_socmedia")
  # Unit c: neither the device row nor the narrower activity row is added.
  expect_equal(total$total_screen[6], 2)
  expect_equal(total$total_rows[6], "st_watch|st_game")
  # The rest of screen use: total minus own where dedicated, else the others.
  expect_equal(total$other_watch[1], 4)
  expect_equal(total$other_watch[6], 1)
  expect_false("other_messaging" %in% names(total))

  no_game <- derive_total_screen(exposures["st_watch"], unit, reg$exposures)
  expect_true(all(is.na(no_game$total_screen)))
  expect_true(all(is.na(no_game$other_watch)))
})

test_that("a row asked of part of a unit does not remove everyone's total", {
  reg <- repo_registries()
  exposures <- tibble::tibble(
    st_watch = rep(1, 20),
    st_game = rep(2, 20),
    st_socmedia = c(rep(3, 4), rep(NA, 16)),
    st_educational = c(rep(1, 16), rep(NA, 4))
  )
  unit <- rep(c("x", "y"), each = 10)
  total <- derive_total_screen(exposures, unit, reg$exposures)
  # Unit x: social media on 4 of 10 rows is left out; educational stays.
  expect_equal(unique(total$total_rows[1:10]), "st_watch|st_game|st_educational")
  expect_equal(total$total_screen[1:10], rep(4, 10))
  # Unit y: no social media at all; educational on 6 of 10 rows stays, and
  # the rows without it have no total.
  expect_equal(sum(is.na(total$total_screen[11:20])), 4)
})

test_that("exposures over 24 hours are dropped and counted", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  rows$st_watch[1:2] <- c(25, 24)
  out <- prepare_exposures(rows, rows$wave, rows$wave, reg$exposures)
  expect_true(is.na(out$exposures$st_watch[1]))
  expect_equal(out$exposures$st_watch[2], 24)
  watch <- out$audit[out$audit$variable_name == "st_watch", ]
  expect_equal(sum(watch$n_values) - sum(watch$n_used), 1)
  expect_equal(nrow(out$exposures), nrow(rows))
})

test_that("outcome scores follow the registry's score types", {
  expect_equal(score_outcome_values(c("1.5", NA), "continuous"), c(1.5, NA))
  expect_equal(
    score_outcome_values(c("TRUE", "No", "Yes", "FALSE", "Unknown"), "binary"),
    c(1, 0, 1, 0, NA)
  )
  expect_equal(
    score_outcome_values(
      c("Poor", "Excellent", "Unknown", "Other"),
      "ordinal",
      "Excellent|Good|Poor|Unknown"
    ),
    c(3, 1, NA, NA)
  )
  expect_true(all(is.na(score_outcome_values(c("a", "b"), "nominal", "a|b"))))
})

test_that("derivations fill only an unmapped target with all its parts", {
  rules <- tibble::tibble(
    target = c("whole", "whole", "other"),
    sources = c("a|b", "a|b", "a|missing"),
    method = c("sum", "weighted_mean", "sum"),
    weights = c(NA, "3|1", NA),
    dataset_id = c(NA, "7", NA)
  )
  scores <- tibble::tibble(a = c(1, 2, NA), b = c(3, 4, 5))

  general <- apply_derivations(scores, rules, "1")
  expect_equal(general$scores$whole, c(4, 6, NA))
  expect_equal(general$derived$target, "whole")
  expect_false("other" %in% names(general$scores))

  specific <- apply_derivations(scores, rules, "7")
  expect_equal(specific$scores$whole, c(1.5, 2.5, NA))

  scores$whole <- c(9, NA, NA)
  mapped <- apply_derivations(scores, rules, "1")
  expect_equal(mapped$scores$whole, c(9, NA, NA))
  expect_equal(nrow(mapped$derived), 0)
})

test_that("a repeated score is dropped in favour of the generic row", {
  set.seed(1)
  x <- round(stats::rnorm(40), 2)
  scores <- tibble::tibble(
    mh_dep_self = x,
    mh_dep_smfq_self = x,
    mh_anx_self = round(stats::rnorm(40), 2),
    flat_a = 1,
    flat_b = 1
  )
  scores$mh_dep_smfq_self[1:5] <- NA
  wave <- rep(c("1", "2"), each = 20)
  nodes <- c(
    mh_dep_self = "depression",
    mh_dep_smfq_self = "depression",
    mh_anx_self = "anxiety"
  )
  repeats <- find_repeated_scores(scores, wave, nodes)
  expect_equal(repeats$specific, rep("mh_dep_smfq_self", 2))
  expect_equal(repeats$keep, rep("mh_dep_self", 2))
  expect_equal(repeats$drop, c(TRUE, TRUE))

  scores$mh_dep_smfq_self[30] <- scores$mh_dep_smfq_self[30] + 1
  expect_equal(find_repeated_scores(scores, wave, nodes)$wave, "1")

  # The row named for its informant is the generic one, whatever its length.
  scores <- tibble::tibble(cog_wm_digitspan = x, cog_wm_task = x)
  same <- c(cog_wm_digitspan = "executive_function", cog_wm_task = "executive_function")
  kept <- find_repeated_scores(scores, wave, same)
  expect_equal(unique(kept$keep), "cog_wm_task")

  # Across nodes the repeat is reported and nothing is dropped.
  apart <- c(cog_wm_digitspan = "memory", cog_wm_task = "executive_function")
  expect_false(any(find_repeated_scores(scores, wave, apart)$drop))
  expect_false(any(find_repeated_scores(scores, wave)$drop))
})

test_that("two rare diagnoses are not called one score", {
  dx <- c(rep(1, 38), 2, 2)
  scores <- tibble::tibble(a_diagnosed = dx, b_diagnosed = dx)
  nodes <- c(a_diagnosed = "n", b_diagnosed = "n")
  expect_equal(nrow(find_repeated_scores(scores, rep("1", 40), nodes)), 0)
  dx[1:8] <- 2
  scores <- tibble::tibble(a_diagnosed = dx, b_diagnosed = dx)
  expect_equal(nrow(find_repeated_scores(scores, rep("1", 40), nodes)), 1)
})

test_that("standardising is within group and needs spread", {
  values <- c(1, 2, 3, 10, 20, 30, 5, NA, 7, 7)
  groups <- c("a", "a", "a", "b", "b", "b", "c", "c", "d", "d")
  z <- standardise_within(values, groups)
  expect_equal(z[1:3], c(-1, 0, 1))
  expect_equal(z[4:6], c(-1, 0, 1))
  expect_true(all(is.na(z[7:10])))
  two_keys <- standardise_within(values[1:6], list(groups[1:6], rep("x", 6)))
  expect_equal(two_keys, z[1:6])
  expect_true(all(is.na(standardise_within(values, groups, min_n = 4))))
})

test_that("prepared tables share one row order and lose no row silently", {
  for (id in c("9991", "9994", "9996")) {
    rows <- mock_dataset(id)
    prepared <- prepare_mock(id)
    n <- nrow(prepared$base)
    expect_equal(n + sum(prepared$dropped$n), nrow(rows))
    expect_equal(nrow(prepared$exposures), n)
    expect_equal(nrow(prepared$outcomes), n)
    expect_equal(nrow(prepared$outcomes_z), n)
    expect_false(anyDuplicated(prepared$base[c("participant_id", "wave")]) > 0)

    audit <- prepared$row_audit
    expect_true(all(audit$dataset_id == id))
    measured <- c(
      "mh_dep_self",
      "wb_selfratedhealth_self",
      "mh_anx_diagnosed",
      "wb_bodyimage",
      "st_watch",
      "st_game"
    )
    expect_true(all(measured %in% audit$variable_name))
    body <- audit[audit$variable_name == "wb_bodyimage", ]
    expect_equal(body$status, "descriptive_only")
    expect_false("wb_bodyimage" %in% names(prepared$outcomes))
  }
})

test_that("prepared scores are the mock's values, standardised within unit", {
  rows <- mock_dataset("9996")
  prepared <- prepare_mock("9996")
  base <- prepared$base
  expect_equal(prepared$outcomes$mh_dep_self, rows$mh_dep_self)
  expect_equal(prepared$exposures$st_game, rows$st_game)
  expect_equal(prepared$exposures$total_source, rep("summed", nrow(base)))
  expect_equal(prepared$exposures$total_screen, rows$st_watch + rows$st_game)

  z <- prepared$outcomes_z$mh_dep_self
  by_unit <- tapply(z, base$unit, function(x) c(mean(x), stats::sd(x)))
  expect_length(by_unit, 2)
  for (unit in by_unit) {
    expect_equal(unit, c(0, 1))
  }
  expect_false("beh_alcohol_self" %in% names(prepared$outcomes))
  expect_setequal(unique(prepared$outcomes$mh_anx_diagnosed), c(1, 2, 3))
  expect_false("mh_anx_diagnosed" %in% names(prepared$outcomes_z))
})

test_that("exceptions are validated and applied", {
  reg <- repo_registries()
  dir <- withr::local_tempdir()
  write <- function(lines) {
    path <- file.path(dir, "analysis_exceptions.csv")
    writeLines(c("variable_name,wave,action,value,reason", lines), path)
    path
  }
  read <- function(lines) {
    read_analysis_exceptions(write(lines), reg$schema, reg$outcomes)
  }
  expect_equal(nrow(no_exceptions(reg)), 0)
  expect_error(read("not_a_row,,exclude,,why"), "not dataschema rows")
  expect_error(read("mh_dep_self,,delete,,why"), "unknown action")
  expect_error(read("mh_dep_self,,exclude,,"), "needs a reason")
  expect_error(read("mh_dep_self,,node,,why"), "need a value")
  expect_error(read("mh_dep_self,,node,no_such_node,why"), "not nodes")

  exceptions <- read(c(
    "mh_dep_self,2,exclude,,Different form at wave 2",
    "mh_dep_self,,standardise_by,dataset|wave|country|sex,Scored by sex"
  ))
  rows <- mock_dataset("9991")
  prepared <- prepare_analysis_data(
    rows,
    mock_info("9991"),
    reg$outcomes,
    reg$exposures,
    reg$derivations,
    reg$schema,
    exceptions
  )
  base <- prepared$base
  expect_true(all(is.na(prepared$outcomes$mh_dep_self[base$wave == "2"])))
  expect_false(anyNA(prepared$outcomes$mh_dep_self[base$wave == "1"]))
  audit <- prepared$row_audit[prepared$row_audit$variable_name == "mh_dep_self", ]
  expect_equal(audit$status[audit$wave == "2"], "excluded")
  expect_equal(audit$status[audit$wave == "1"], "used")
  z <- prepared$outcomes_z$mh_dep_self[base$wave == "1"]
  girls <- base$sex[base$wave == "1"] == "Female"
  expect_equal(mean(z[girls]), 0)
})

test_that("every committed exceptions file passes its checks", {
  reg <- repo_registries()
  files <- list.files(
    file.path(repo_root, bp_dataset_specs_dir()),
    pattern = "^analysis_exceptions\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  for (file in files) {
    expect_no_error(read_analysis_exceptions(file, reg$schema, reg$outcomes))
  }
})

test_that("a prepared dataset is summarised and flagged", {
  reg <- repo_registries()
  rows <- mock_dataset("9991")
  rows$mh_dep_cesd_self <- rows$mh_dep_self * 2 + stats::rnorm(nrow(rows), 0, 0.01)
  rows$age_years[seq_len(nrow(rows) / 2)] <- 30
  prepared <- prepare_mock("9991", rows = rows)

  summary <- summarise_prepared(prepared)
  expect_equal(summary$rows_kept, nrow(prepared$base))
  expect_equal(summary$dropped_age_over_limit, nrow(rows) / 2)
  expect_equal(summary$total_summed, nrow(prepared$base))

  variables <- tibble::tibble(
    target_variable = c("mh_dep_self", "wb_lifesat_self", "wb_selfesteem_self"),
    status = c("partial", "incompatible", "incompatible"),
    notes = c(
      "As released.",
      "Derive at analysis from the items.",
      "See the instrument row, which the analysis can derive at analysis."
    )
  )
  flags <- flag_prepared_dataset(prepared, variables, reg$outcomes)
  expect_true("rows_dropped" %in% flags$flag)
  expect_true("near_identical" %in% flags$flag)
  expect_equal(
    flags$variable_name[flags$flag == "derive_at_analysis"],
    "wb_lifesat_self"
  )
  expect_equal(flags$level[flags$flag == "partial_rows"], "info")
  expect_null(flag_prepared_dataset(NULL, variables, reg$outcomes))
})

test_that("the analysis section builds a target list from a dataset plan", {
  skip_if_not_installed("tarchetypes")
  plan <- tibble::tibble(
    dataset_key = c("BPIPD_9991", "BPIPD_9992"),
    dataset_id = c("9991", "9992")
  )
  withr::local_dir(repo_root)
  section <- analysis_targets(plan)
  names <- unlist(lapply(
    unlist(section),
    function(target) target$settings$name
  ))
  expect_true(all(
    c(
      "data_manifest",
      "dataset_summary_BPIPD_9991",
      "prepared_BPIPD_9991",
      "prepared_review_BPIPD_9992",
      "violations",
      "coverage"
    ) %in%
      names
  ))
  expect_false(anyDuplicated(names) > 0)
  expect_length(analysis_targets(plan[0, ]), 6)
})

test_that("an excluded row stays excluded when a rule could derive it", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  n <- nrow(rows)
  rows$beh_conduct_sdq_parent <- round(stats::runif(n, 0, 10))
  rows$beh_hyper_sdq_parent <- round(stats::runif(n, 0, 10))
  dir <- withr::local_tempdir()
  path <- file.path(dir, "analysis_exceptions.csv")
  writeLines(
    c(
      "variable_name,wave,action,value,reason",
      "beh_ext_sdq_parent,,exclude,,Not comparable here"
    ),
    path
  )
  prepare <- function(exceptions) {
    prepare_analysis_data(
      rows,
      mock_info("9995"),
      reg$outcomes,
      reg$exposures,
      reg$derivations,
      reg$schema,
      exceptions
    )
  }
  derived <- prepare(no_exceptions(reg))
  expect_equal(
    derived$outcomes$beh_ext_sdq_parent,
    rows$beh_conduct_sdq_parent + rows$beh_hyper_sdq_parent
  )
  excluded <- prepare(read_analysis_exceptions(path, reg$schema, reg$outcomes))
  expect_false("beh_ext_sdq_parent" %in% names(excluded$outcomes))
})

test_that("bounds drop values unless an exception keeps them", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  rows$cog_earlynumber_bas_enc <- 150
  rows$cog_earlynumber_bas_enc[1:50] <- 50
  dropped <- prepare_mock("9995", rows = rows)
  expect_equal(sum(!is.na(dropped$outcomes$cog_earlynumber_bas_enc)), 50)
  audit <- dropped$row_audit
  audit <- audit[audit$variable_name == "cog_earlynumber_bas_enc", ]
  expect_equal(sum(audit$n_values) - sum(audit$n_used), nrow(rows) - 50)

  path <- file.path(
    repo_root,
    bp_harmonisation_dataset_dir("1025"),
    "analysis_exceptions.csv"
  )
  kept <- prepare_analysis_data(
    rows,
    mock_info("9995"),
    reg$outcomes,
    reg$exposures,
    reg$derivations,
    reg$schema,
    read_analysis_exceptions(path, reg$schema, reg$outcomes)
  )
  expect_false(anyNA(kept$outcomes$cog_earlynumber_bas_enc))
})

test_that("a part is dropped only where its whole covers it", {
  reg <- repo_registries()
  rows <- mock_dataset("9993")
  n <- nrow(rows)
  rows$beh_atten_self <- stats::rnorm(n)
  rows$beh_hyper_self <- stats::rnorm(n)
  rows$beh_adhdsymp_self <- rows$beh_atten_self + rows$beh_hyper_self
  covered <- prepare_mock("9993", rows = rows)
  expect_false("beh_atten_self" %in% names(covered$outcomes))
  expect_true("beh_adhdsymp_self" %in% names(covered$outcomes))

  rows$beh_adhdsymp_self[rows$wave == "1"][1:60] <- NA
  partial <- prepare_mock("9993", rows = rows)
  wave_1 <- partial$base$wave == "1"
  expect_false(anyNA(partial$outcomes$beh_atten_self[wave_1]))
  expect_true(all(is.na(partial$outcomes$beh_atten_self[!wave_1])))
})

test_that("extra measure blocks follow the row rules", {
  rows <- mock_dataset("9995")
  rows$age_years[1:10] <- 30
  diary <- rows[1:40, ]
  diary$st_measure_id <- "diary"
  diary$st_watch[11] <- 30
  recall <- rows[41:50, ]
  recall$st_measure_id <- "recall"
  prepared <- prepare_mock("9995", rows = dplyr::bind_rows(rows, diary, recall))
  measures <- prepared$measures
  expect_equal(nrow(measures), 40)
  expect_true(all(measures$participant_id %in% prepared$base$participant_id))
  expect_equal(sum(is.na(measures$st_watch)), 1)
  expect_equal(sum(!measures$concurrent), 10)
  expect_equal(nrow(prepared$base), nrow(rows) - 10)
})

test_that("a baseline wave that is not in the data is an error", {
  rows <- mock_dataset("9991")
  expect_error(
    prepare_base(rows, mock_info("9991", baseline_wave = "0"), names(rows)),
    "not a wave in the data"
  )
})

test_that("exceptions must name analysed outcomes, and a node moves a row", {
  reg <- repo_registries()
  dir <- withr::local_tempdir()
  read <- function(line) {
    path <- file.path(dir, "analysis_exceptions.csv")
    writeLines(c("variable_name,wave,action,value,reason", line), path)
    read_analysis_exceptions(path, reg$schema, reg$outcomes)
  }
  prepare <- function(exceptions) {
    prepare_analysis_data(
      mock_dataset("9995"),
      mock_info("9995"),
      reg$outcomes,
      reg$exposures,
      reg$derivations,
      reg$schema,
      exceptions
    )
  }
  expect_error(prepare(read("st_watch,,exclude,,why")), "not analysed outcomes")
  expect_error(read("mh_dep_self,1,node,anxiety,why"), "every wave")
  moved <- prepare(read("mh_dep_self,,node,anxiety,Anxiety scale here"))
  expect_equal(moved$nodes$variable_name, "mh_dep_self")
  expect_equal(moved$nodes$node, "anxiety")
})

test_that("one summary target and one review target hold a dataset's parts", {
  reg <- repo_registries()
  rows <- mock_dataset("9991")
  info <- mock_info("9991")
  parts <- summarise_dataset(
    rows,
    info,
    reg$schema,
    mock_invariants,
    reg$exposures
  )
  expect_named(
    parts,
    c(
      "manifest",
      "grain",
      "numeric_summary",
      "level_summary",
      "unit_summary",
      "unit_counts",
      "invariant_check",
      "day_check"
    )
  )
  expect_equal(parts$manifest, manifest_dataset(rows, info))
  other <- summarise_dataset(
    mock_dataset("9992"),
    mock_info("9992"),
    reg$schema,
    mock_invariants,
    reg$exposures
  )
  excluded <- summarise_dataset(
    rows,
    mock_info("9991", included = FALSE),
    reg$schema,
    mock_invariants,
    reg$exposures
  )
  grain <- bind_part(list(parts, excluded, other), "grain")
  expect_setequal(grain$dataset_id, c("9991", "9992"))
  manifest <- combine_data_manifest(
    bind_part(list(other, excluded, parts), "manifest")
  )
  expect_equal(manifest$dataset_id, c("9991", "9992"))

  variables <- tibble::tibble(
    target_variable = "mh_dep_self",
    status = "compatible",
    notes = NA_character_
  )
  review <- review_prepared(prepare_mock("9991"), variables, reg$outcomes)
  expect_named(review, c("summary", "flags", "row_audit", "dropped"))
  expect_equal(review$summary$dataset_id, "9991")
  expect_null(review_prepared(NULL, variables, reg$outcomes)$summary)
})

test_that("empty columns are dropped, and the result does not depend on it", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  padded <- rows
  padded$st_tablet <- NA_real_
  padded$mh_anx_self <- NA_real_
  padded$urbanicity <- NA_character_
  trimmed <- drop_empty_columns(padded)
  expect_false(any(c("st_tablet", "mh_anx_self", "urbanicity") %in% names(trimmed)))
  # `wave` is all missing in a single-wave dataset and is kept.
  expect_true(all(is.na(rows$wave)))
  expect_true("wave" %in% names(trimmed))
  expect_null(drop_empty_columns(NULL))

  info <- mock_info("9995")
  a <- summarise_dataset(rows, info, reg$schema, mock_invariants, reg$exposures)
  b <- summarise_dataset(padded, info, reg$schema, mock_invariants, reg$exposures)
  expect_equal(a[names(a) != "manifest"], b[names(b) != "manifest"])
  expect_false(identical(a$manifest$hash, b$manifest$hash))
  expect_equal(
    prepare_mock("9995", rows = padded)[c("base", "exposures", "outcomes")],
    prepare_mock("9995", rows = rows)[c("base", "exposures", "outcomes")]
  )
})

test_that("a declared wave order replaces the computed one", {
  rows <- mock_dataset("9991")
  rows$data_year <- 2020
  computed <- prepare_base(rows, mock_info("9991"), names(rows))$base
  expect_equal(unique(computed$wave_order[computed$wave == "1"]), 1L)
  declared <- prepare_base(
    rows,
    mock_info("9991", wave_order = "2|1"),
    names(rows)
  )$base
  expect_equal(unique(declared$wave_order[declared$wave == "2"]), 1L)
  expect_equal(unique(declared$wave_order[declared$wave == "1"]), 2L)
  expect_error(
    prepare_base(rows, mock_info("9991", wave_order = "1"), names(rows)),
    "does not list wave"
  )
})

test_that("a generic row standing for a short form is dropped for the whole", {
  rows <- mock_dataset("9993")
  n <- nrow(rows)
  short <- round(stats::runif(n, 0, 24))
  rows$mh_distress_k6_self <- short
  rows$mh_distress_self <- short
  rows$mh_distress_k10_self <- short + round(stats::runif(n, 10, 16))
  prepared <- prepare_mock("9993", rows = rows)
  expect_true("mh_distress_k10_self" %in% names(prepared$outcomes))
  expect_false("mh_distress_k6_self" %in% names(prepared$outcomes))
  expect_false("mh_distress_self" %in% names(prepared$outcomes))
  audit <- prepared$row_audit
  expect_equal(
    unique(audit$status[audit$variable_name == "mh_distress_self"]),
    "dropped_part"
  )
})
