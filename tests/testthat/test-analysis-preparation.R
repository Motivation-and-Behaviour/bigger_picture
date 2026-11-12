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

  levels <- summarise_levels(rows, info, reg$schema, max_levels = 50)
  sex <- levels[levels$variable_name == "sex", ]
  expect_equal(sum(sex$n), nrow(rows))
  ids <- levels[levels$variable_name == "participant_id", ]
  expect_equal(ids$level, "(many values)")

  units <- summarise_units(rows, info)
  expect_setequal(units$country, c("Ireland", "Scotland"))
  expect_equal(sum(units$rows), nrow(rows))

  coverage <- summarise_coverage(numeric, levels, grain, reg$schema)
  expect_equal(coverage$share[coverage$variable_name == "st_game"], 1)
  missing <- summarise_missingness(numeric, levels, grain)
  expect_equal(sum(missing$n_systematic), 0)
})

test_that("Unknown counts as missing, sporadic where the wave has values", {
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

  missing <- summarise_missingness(numeric, levels, grain)
  sex <- missing[missing$variable_name == "sex", ]
  expect_equal(sex$n_sporadic, 5)
  expect_equal(sex$n_systematic, sum(rows$wave == "2"))
})

test_that("the checks find what they name", {
  reg <- repo_registries()
  rows <- mock_dataset("9991")
  info <- mock_info("9991")
  rows$st_game[1:3] <- 30
  rows <- dplyr::bind_rows(rows, rows[10, ])
  person <- rows$participant_id[20]
  rows$sex[rows$participant_id == person] <- c("Female", "Male")
  invariants <- tibble::tibble(variable_name = c("sex", "country"))

  findings <- check_analysis_data(
    summarise_numeric(rows, info, reg$schema),
    summarise_levels(rows, info, reg$schema),
    summarise_grain(rows, info),
    check_invariants(rows, info, invariants),
    reg$schema,
    reg$exposures
  )
  found <- function(check, variable = NULL) {
    hit <- findings$check == check
    if (!is.null(variable)) {
      hit <- hit & findings$variable_name %in% variable
    }
    sum(findings$n[hit])
  }
  expect_equal(found("above_max", "st_game"), 3)
  expect_equal(found("duplicate_key"), 1)
  expect_equal(found("invariant_varies", "sex"), 1)
  expect_equal(found("invariant_varies", "country"), 0)
  expect_true(all(findings$severity %in% c("review", "advisory")))

  clean <- mock_dataset("9992")
  none <- check_analysis_data(
    summarise_numeric(clean, mock_info("9992"), reg$schema),
    summarise_levels(clean, mock_info("9992"), reg$schema),
    summarise_grain(clean, mock_info("9992")),
    check_invariants(clean, mock_info("9992"), invariants),
    reg$schema,
    reg$exposures
  )
  expect_equal(nrow(none[none$severity == "review", ]), 0)
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
    st_phone = c(9, 9, 9, 9, 9, 9)
  )
  wave <- c("a", "a", "b", "b", "b", "c")
  total <- derive_total_screen(exposures, wave, reg$exposures)

  expect_equal(total$total_source, c(rep("dedicated", 2), rep("summed", 4)))
  expect_equal(total$total_screen[1:2], c(5, 6))
  # Wave b sums watching, gaming and social media, complete cases only.
  expect_equal(total$total_screen[3:5], c(NA, NA, 24))
  expect_equal(total$total_capped, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(total$total_coverage[3], 3L)
  expect_equal(total$total_rows[3], "st_watch|st_game|st_socmedia")
  # Wave c has only watching and gaming; the device row is never added.
  expect_equal(total$total_screen[6], 2)
  expect_equal(total$other_watch[1], 2)
  expect_equal(total$other_watch[6], 1)

  no_game <- derive_total_screen(exposures["st_watch"], wave, reg$exposures)
  expect_true(all(is.na(no_game$total_screen)))
})

test_that("exposures over 24 hours are dropped and counted", {
  reg <- repo_registries()
  rows <- mock_dataset("9995")
  rows$st_watch[1:2] <- c(25, 24)
  out <- prepare_exposures(rows, rows$wave, reg$exposures)
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

test_that("standardising is within group and needs spread", {
  values <- c(1, 2, 3, 10, 20, 30, 5, NA, 7, 7)
  groups <- c("a", "a", "a", "b", "b", "b", "c", "c", "d", "d")
  z <- standardise_within(values, groups)
  expect_equal(z[1:3], c(-1, 0, 1))
  expect_equal(z[4:6], c(-1, 0, 1))
  expect_true(all(is.na(z[7:10])))
  two_keys <- standardise_within(values[1:6], list(groups[1:6], rep("x", 6)))
  expect_equal(two_keys, z[1:6])
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
    target_variable = c("mh_dep_self", "wb_lifesat_self"),
    notes = c("As released.", "Derive at analysis from the items.")
  )
  flags <- flag_prepared_dataset(prepared, variables, reg$outcomes)
  expect_true("rows_dropped" %in% flags$flag)
  expect_true("near_identical" %in% flags$flag)
  expect_equal(
    flags$variable_name[flags$flag == "derive_at_analysis"],
    "wb_lifesat_self"
  )
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
    c("data_manifest", "prepared_BPIPD_9991", "violations", "coverage") %in%
      names
  ))
  expect_false(anyDuplicated(names) > 0)
  expect_length(analysis_targets(plan[0, ]), 5)
})
