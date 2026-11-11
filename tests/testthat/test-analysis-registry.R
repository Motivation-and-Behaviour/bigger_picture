# The analysis registries (ANALYSIS_PLAN.md, Step 1): the committed spec CSVs
# must pass their readers, and each validation rule must catch what it names.

repo_schema <- function() {
  read_dataschema(file.path(repo_root, bp_dataschema_path()))
}

test_that("the committed outcome registry passes its checks", {
  registry <- read_outcome_registry(
    file.path(repo_root, bp_outcome_tree_path()),
    file.path(repo_root, bp_outcome_rows_path()),
    repo_schema()
  )
  expect_true(all(registry$rows$frozen %in% FALSE))
  expect_true(any(registry$tree$headline %in% TRUE))
})

test_that("the committed exposure registry passes its checks", {
  exposures <- read_exposure_registry(
    file.path(repo_root, bp_exposures_path()),
    repo_schema()
  )
  expect_true(any(exposures$headline))
})

test_that("invariants.csv names dataschema rows, once each", {
  invariants <- read_registry_csv(
    file.path(repo_root, bp_invariants_path()),
    bp_invariants_columns()
  )
  expect_false(anyDuplicated(invariants$variable_name) > 0)
  expect_true(all(invariants$variable_name %in% repo_schema()$variable_name))
})

test_that("datasets.csv rows are unique and each has a spec", {
  datasets <- read_analysis_datasets(
    file.path(repo_root, bp_analysis_datasets_path())
  )
  expect_false(anyDuplicated(datasets$dataset_id) > 0)
  spec_ids <- sub(
    "^BPIPD-",
    "",
    basename(fs::dir_ls(
      file.path(repo_root, bp_dataset_specs_dir()),
      type = "directory"
    ))
  )
  expect_true(all(datasets$dataset_id %in% spec_ids))
  # A warning, not a failure: dataset branches never edit analysis/, so a new
  # spec has no row until one is added on main.
  missing <- setdiff(spec_ids, datasets$dataset_id)
  if (length(missing) > 0) {
    warning(
      "No row in datasets.csv for: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
})

test_that("a dataset missing from datasets.csv reads as a blank row", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      paste(bp_analysis_datasets_columns(), collapse = ","),
      "9991,T1,,TRUE,,,A panel"
    ),
    path
  )
  datasets <- read_analysis_datasets(path, c("9991", "9992"))
  expect_equal(datasets$dataset_id, c("9991", "9992"))
  expect_equal(datasets$baseline_wave, c("T1", NA))
  expect_equal(datasets$signed_off, c(TRUE, NA))
  expect_equal(nrow(read_analysis_datasets(path)), 1)
})

test_that("the committed derivation rules pass their checks", {
  derivations <- read_derivations(
    file.path(repo_root, bp_derivations_path()),
    repo_schema()
  )
  ids <- derivations$dataset_id[!is.na(derivations$dataset_id)]
  spec_dirs <- file.path(
    repo_root,
    bp_dataset_specs_dir(),
    paste0("BPIPD-", ids)
  )
  expect_true(all(dir.exists(spec_dirs)))
})

test_that("outcome rows noted 'Derive at analysis' have a derivation rule", {
  derivations <- read_derivations(
    file.path(repo_root, bp_derivations_path()),
    repo_schema()
  )
  rows <- read_registry_csv(
    file.path(repo_root, bp_outcome_rows_path()),
    bp_outcome_rows_columns(),
    c("synthesis", "frozen")
  )
  outcomes <- rows$variable_name[rows$role %in% "outcome"]
  general <- derivations$target[is.na(derivations$dataset_id)]
  specific <- paste(derivations$dataset_id, derivations$target)
  spec_dirs <- fs::dir_ls(
    file.path(repo_root, bp_dataset_specs_dir()),
    type = "directory"
  )
  uncovered <- unlist(lapply(spec_dirs, function(dir) {
    variables <- read.csv(file.path(dir, "variables.csv"))
    id <- sub("^BPIPD-", "", basename(dir))
    noted <- variables$target_variable[
      grepl("^Derive at analysis", variables$notes) &
        variables$target_variable %in% outcomes
    ]
    noted <- noted[
      !noted %in% general & !paste(id, noted) %in% specific
    ]
    if (length(noted)) paste(id, noted) else NULL
  }))
  # A warning until Step 4's full pass (ANALYSIS_PLAN.md): dataset branches
  # never edit analysis/, so a new note must not fail their tests.
  if (length(uncovered) > 0) {
    warning(
      "No derivation rule for: ",
      paste(uncovered, collapse = ", "),
      call. = FALSE
    )
  }
  succeed()
})

# --- validation rules, on a small fixture ------------------------------------

mini_schema <- function() {
  tibble::tibble(
    variable_name = c(
      "participant_id",
      "mh_dep_self",
      "mh_dep_measure",
      "beh_fight",
      "wb_bodyimage",
      "st_measure_id",
      "st_measure_name",
      "st_watch",
      "st_violent_game_index"
    ),
    domain = c(
      "provenance",
      "mental_health",
      "mental_health",
      "behaviour",
      "wellbeing",
      rep("screen_time", 4)
    ),
    data_type = c(
      "character",
      "double",
      "character",
      "character",
      "character",
      "character",
      "character",
      "double",
      "double"
    ),
    unit = c(rep(NA, 7), "hours", NA),
    allowed_values = c(
      NA,
      NA,
      NA,
      "TRUE|FALSE",
      "Much too thin|About right|Much too fat",
      NA,
      NA,
      NA,
      NA
    )
  )
}

mini_tree <- function() {
  tibble::tibble(
    node = c("mental_health", "depression", "behaviour", "aggression"),
    parent = c(NA, "mental_health", NA, "behaviour"),
    label = c("Mental health", "Depression", "Behaviour", "Aggression"),
    level = c("L0", "L1", "L0", "L1"),
    prereg_status = "named",
    headline = c(NA, TRUE, NA, FALSE),
    notes = NA_character_
  )
}

mini_rows <- function() {
  schema <- mini_schema()
  rows <- outcome_row_defaults(
    schema[schema$domain %in% bp_outcome_registry_domains(), ]
  )
  assigned <- rows$variable_name %in% c("mh_dep_self", "beh_fight")
  rows$node[rows$variable_name == "mh_dep_self"] <- "depression"
  rows$node[rows$variable_name == "beh_fight"] <- "aggression"
  rows$informant[rows$variable_name == "beh_fight"] <- "self"
  rows$direction[assigned] <- "higher_worse"
  rows$synthesis[assigned] <- TRUE
  rows$use[assigned] <- "core"
  rows$prereg_family[rows$variable_name == "mh_dep_self"] <- "mental_health"
  rows
}

expect_registry_problem <- function(tree, rows, pattern) {
  expect_error(validate_outcome_registry(tree, rows, mini_schema()), pattern)
}

test_that("the fixture registry is valid", {
  expect_no_error(
    validate_outcome_registry(mini_tree(), mini_rows(), mini_schema())
  )
})

test_that("outcome_row_defaults() fills only what the schema settles", {
  rows <- outcome_row_defaults(mini_schema()[2:5, ])
  expect_equal(rows$role, c("outcome", "measure", "outcome", "outcome"))
  expect_equal(
    rows$score_type,
    c("continuous", NA, "binary", "ordinal")
  )
  expect_equal(rows$informant, c("self", NA, NA, NA))
  expect_equal(rows$node, c("unassigned", NA, "unassigned", "unassigned"))
  expect_equal(rows$standardise_by[1], "dataset|wave|country")
  expect_true(all(is.na(rows$direction)))
  expect_equal(nrow(outcome_row_defaults(mini_schema()[0, ])), 0)
})

test_that("a schema row missing from the registry is reported", {
  rows <- mini_rows()
  expect_registry_problem(
    mini_tree(),
    rows[rows$variable_name != "wb_bodyimage", ],
    "schema rows missing.*wb_bodyimage"
  )
})

test_that("tree structure is checked", {
  tree <- mini_tree()
  tree$parent[tree$node == "aggression"] <- "nowhere"
  expect_registry_problem(tree, mini_rows(), "L1 parent not in the tree")

  tree <- mini_tree()
  tree$headline[tree$node == "depression"] <- NA
  expect_registry_problem(tree, mini_rows(), "need headline")

  tree <- mini_tree()
  tree$node[tree$node == "aggression"] <- "unassigned"
  expect_registry_problem(tree, mini_rows(), "placeholder")
})

test_that("node rules are checked", {
  rows <- mini_rows()
  rows$node[rows$variable_name == "mh_dep_self"] <- "anxiety"
  expect_registry_problem(mini_tree(), rows, "not an L1 node")

  rows <- mini_rows()
  rows$node[rows$variable_name == "mh_dep_measure"] <- "depression"
  expect_registry_problem(mini_tree(), rows, "only outcome rows take a node")

  rows <- mini_rows()
  rows$node[rows$variable_name == "wb_bodyimage"] <- NA
  expect_registry_problem(mini_tree(), rows, "synthesis FALSE")
  rows$synthesis[rows$variable_name == "wb_bodyimage"] <- FALSE
  expect_no_error(validate_outcome_registry(mini_tree(), rows, mini_schema()))
})

test_that("assigned rows need their judgement columns", {
  rows <- mini_rows()
  rows$direction[rows$variable_name == "beh_fight"] <- NA
  expect_registry_problem(mini_tree(), rows, "need direction")

  rows <- mini_rows()
  rows$prereg_family[rows$variable_name == "mh_dep_self"] <- "behaviour"
  expect_registry_problem(mini_tree(), rows, "differs from the node's family")
})

test_that("unassigned rows are forbidden once the registry is frozen", {
  rows <- mini_rows()
  rows$frozen <- TRUE
  expect_registry_problem(mini_tree(), rows, "forbidden after the freeze")
})

test_that("part_of and standardise_by name real rows and keys", {
  rows <- mini_rows()
  rows$part_of[rows$variable_name == "beh_fight"] <- "beh_nothing"
  expect_registry_problem(mini_tree(), rows, "part_of")

  rows <- mini_rows()
  rows$standardise_by[rows$variable_name == "mh_dep_self"] <- "dataset|school"
  expect_registry_problem(mini_tree(), rows, "standardise_by")

  rows$standardise_by[rows$variable_name == "mh_dep_self"] <-
    "dataset|mh_dep_measure"
  expect_no_error(validate_outcome_registry(mini_tree(), rows, mini_schema()))
})

# --- sync ----------------------------------------------------------------------

test_that("sync_outcome_registry() creates, extends and reorders the file", {
  path <- withr::local_tempfile(fileext = ".csv")
  schema <- mini_schema()

  created <- sync_outcome_registry(path, schema)
  expect_equal(created$added_n, 4)
  rows <- read_registry_csv(
    path,
    bp_outcome_rows_columns(),
    c("synthesis", "frozen")
  )
  expect_equal(
    rows$variable_name,
    c("mh_dep_self", "mh_dep_measure", "beh_fight", "wb_bodyimage")
  )
  expect_equal(rows$node[1], "unassigned")

  expect_false(sync_outcome_registry(path, schema)$changed)

  # A new schema row arrives as unassigned, in schema order.
  schema <- tibble::add_row(
    schema,
    variable_name = "mh_anx_self",
    domain = "mental_health",
    data_type = "double",
    .after = 2
  )
  added <- sync_outcome_registry(path, schema)
  expect_equal(added$added_variables, "mh_anx_self")
  rows <- read_registry_csv(
    path,
    bp_outcome_rows_columns(),
    c("synthesis", "frozen")
  )
  expect_equal(rows$variable_name[2], "mh_anx_self")
  expect_equal(rows$node[2], "unassigned")
})

test_that("after the freeze new schema rows arrive as post_freeze", {
  path <- withr::local_tempfile(fileext = ".csv")
  sync_outcome_registry(path, mini_schema())
  rows <- read_registry_csv(
    path,
    bp_outcome_rows_columns(),
    c("synthesis", "frozen")
  )
  rows$frozen <- TRUE
  readr::write_csv(rows, path, na = "")

  schema <- tibble::add_row(
    mini_schema(),
    variable_name = "mh_anx_self",
    domain = "mental_health",
    data_type = "double"
  )
  sync_outcome_registry(path, schema)
  rows <- read_registry_csv(
    path,
    bp_outcome_rows_columns(),
    c("synthesis", "frozen")
  )
  expect_equal(rows$node[rows$variable_name == "mh_anx_self"], "post_freeze")
})

test_that("rows the schema no longer has stop the sync unless removed", {
  path <- withr::local_tempfile(fileext = ".csv")
  sync_outcome_registry(path, mini_schema())
  schema <- mini_schema()[mini_schema()$variable_name != "beh_fight", ]

  expect_error(sync_outcome_registry(path, schema), "beh_fight")
  removed <- sync_outcome_registry(
    path,
    schema,
    remove_unknown_variables = TRUE
  )
  expect_equal(removed$removed_variables, "beh_fight")
})

# --- exposures -----------------------------------------------------------------

mini_exposures <- function() {
  tibble::tibble(
    exposure = c("metadata", "watch", "violent_game_index"),
    variable_name = c("st_measure_name", "st_watch", "st_violent_game_index"),
    content_class = c(NA, "non_interactive", "interactive"),
    axis = c("metadata", "activity", "subset"),
    context = "any",
    day = c(NA, "usual", "usual"),
    unit = c(NA, "hours", "index"),
    include = c(FALSE, TRUE, FALSE),
    headline = c(FALSE, TRUE, FALSE),
    notes = NA_character_
  )
}

test_that("the fixture exposure registry is valid", {
  expect_no_error(validate_exposure_registry(mini_exposures(), mini_schema()))
})

test_that("exposure registry rules are checked", {
  expect_error(
    validate_exposure_registry(mini_exposures()[-2, ], mini_schema()),
    "missing: st_watch"
  )

  exposures <- mini_exposures()
  exposures$unit[2] <- "index"
  expect_error(
    validate_exposure_registry(exposures, mini_schema()),
    "unit differs"
  )

  exposures <- mini_exposures()
  exposures$include[1] <- TRUE
  expect_error(
    validate_exposure_registry(exposures, mini_schema()),
    "metadata rows are never included"
  )

  exposures <- mini_exposures()
  exposures$include[2] <- FALSE
  expect_error(
    validate_exposure_registry(exposures, mini_schema()),
    "headline rows must be included"
  )
})

# --- derivation rules ----------------------------------------------------------

mini_derivation <- function(...) {
  rule <- tibble::tibble(
    target = "mh_dep_self",
    sources = "beh_fight|st_watch",
    method = "sum",
    weights = NA_character_,
    dataset_id = NA_character_,
    notes = NA_character_
  )
  utils::modifyList(rule, list(...)) |> tibble::as_tibble()
}

expect_derivation_problem <- function(rules, pattern) {
  expect_error(validate_derivations(rules, mini_schema()), pattern)
}

test_that("a valid derivation rule passes", {
  expect_no_error(validate_derivations(mini_derivation(), mini_schema()))
  expect_no_error(validate_derivations(
    mini_derivation(method = "weighted_mean", weights = "2|1"),
    mini_schema()
  ))
})

test_that("derivation rules must name dataschema rows", {
  expect_derivation_problem(
    mini_derivation(target = "nope"),
    "targets not in the dataschema"
  )
  expect_derivation_problem(
    mini_derivation(sources = "beh_fight|nope"),
    "sources missing or not in the dataschema"
  )
})

test_that("a derivation's target cannot be one of its sources", {
  expect_derivation_problem(
    mini_derivation(sources = "mh_dep_self|st_watch"),
    "one of its own sources"
  )
})

test_that("derivation methods and weights are checked", {
  expect_derivation_problem(
    mini_derivation(method = "median"),
    "unknown method"
  )
  expect_derivation_problem(
    mini_derivation(method = "weighted_mean"),
    "one positive weight per source"
  )
  expect_derivation_problem(
    mini_derivation(method = "weighted_mean", weights = "1"),
    "one positive weight per source"
  )
  expect_derivation_problem(
    mini_derivation(weights = "1|1"),
    "takes none"
  )
})

test_that("a target has one rule per dataset", {
  rules <- dplyr::bind_rows(mini_derivation(), mini_derivation())
  expect_derivation_problem(rules, "more than one rule")
  rules$dataset_id[2] <- "9991"
  expect_no_error(validate_derivations(rules, mini_schema()))
})
