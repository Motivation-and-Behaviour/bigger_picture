make_measure_dataschema <- function() {
  tibble::tibble(
    variable_name = c(
      "dataset_id",
      "dataset_name",
      "participant_id",
      "age_years",
      "st_measure_id",
      "st_measure_type",
      "st_measure_name",
      "st_responder",
      "st_watch_wd",
      "st_game_wd"
    ),
    domain = c(
      "provenance",
      "provenance",
      "provenance",
      "demographics",
      rep("screen_time", 6)
    ),
    data_type = c(
      "character",
      "character",
      "character",
      "double",
      rep("character", 4),
      "double",
      "double"
    )
  )
}

make_measure_variables <- function(with_diary = TRUE) {
  primary <- tibble::tribble(
    ~target_variable   ,
    ~status            ,
    ~expression        ,
    ~measure           ,
    "participant_id"   ,
    "compatible"       ,
    "as.character(id)" ,
    NA_character_      ,
    "age_years"        ,
    "compatible"       ,
    "age"              ,
    NA_character_      ,
    "st_measure_type"  ,
    "inferred"         ,
    '"Categorical"'    ,
    NA_character_      ,
    "st_measure_name"  ,
    "inferred"         ,
    '"Custom"'         ,
    NA_character_      ,
    "st_responder"     ,
    "inferred"         ,
    '"Self"'           ,
    NA_character_      ,
    "st_watch_wd"      ,
    "compatible"       ,
    "tv_q"             ,
    NA_character_      ,
    "st_game_wd"       ,
    "compatible"       ,
    "game_q"           ,
    NA_character_
  )
  diary <- tibble::tribble(
    ~target_variable   ,
    ~status            ,
    ~expression        ,
    ~measure           ,
    "st_measure_type"  ,
    "inferred"         ,
    '"Continuous"'     ,
    "diary"            ,
    "st_measure_name"  ,
    "inferred"         ,
    '"Time use diary"' ,
    "diary"            ,
    "st_responder"     ,
    "inferred"         ,
    '"Self"'           ,
    "diary"            ,
    "st_watch_wd"      ,
    "compatible"       ,
    "tv_d / 60"        ,
    "diary"
  )
  rows <- if (with_diary) dplyr::bind_rows(primary, diary) else primary
  rows$source_columns <- NA_character_
  rows$notes <- NA_character_
  rows$lookup_table <- NA_character_
  rows
}

make_measure_data <- function() {
  tibble::tibble(
    id = c(1, 2, 3),
    age = c(9, 10, 11),
    tv_q = c(1, 2, 3),
    game_q = c(0, 1, 0),
    tv_d = c(60, NA, 120)
  )
}

test_that("a measure block adds rows for the participants it observes", {
  result <- harmonise_from_tables(
    analysis_base = make_measure_data(),
    spec = make_test_spec(),
    dataschema = make_measure_dataschema(),
    harmonisation_config = make_test_config(make_measure_variables())
  )

  expect_identical(names(result), make_measure_dataschema()$variable_name)
  expect_identical(nrow(result), 5L)
  expect_identical(
    result$st_measure_id,
    c("primary", "primary", "primary", "diary", "diary")
  )

  diary <- result[result$st_measure_id == "diary", ]
  # Only participants with a diary quantity get a diary row
  expect_identical(diary$participant_id, c("1", "3"))
  expect_identical(diary$st_watch_wd, c(1, 2))
  # Screen-time variables the measure does not map are NA, not inherited
  expect_identical(diary$st_game_wd, c(NA_real_, NA_real_))
  # Everything outside the screen-time domain is copied from the primary rows
  expect_identical(diary$age_years, c(9, 11))
  expect_identical(diary$dataset_id, c("99", "99"))
  expect_identical(diary$st_measure_name, c("Time use diary", "Time use diary"))

  primary <- result[result$st_measure_id == "primary", ]
  expect_identical(primary$st_watch_wd, c(1, 2, 3))
  expect_identical(primary$st_measure_name, rep("Custom", 3))
})

test_that("without measure tags the harmoniser behaves as before", {
  result <- harmonise_from_tables(
    analysis_base = make_measure_data(),
    spec = make_test_spec(),
    dataschema = make_measure_dataschema(),
    harmonisation_config = make_test_config(
      make_measure_variables(with_diary = FALSE)
    )
  )

  expect_identical(nrow(result), 3L)
  expect_identical(result$st_measure_id, rep("primary", 3))
})

test_that("a variables.csv without a measure column reads as primary rows", {
  variables <- make_measure_variables(with_diary = FALSE)
  variables$measure <- NULL

  expect_identical(
    harmonisation_measure_of(variables),
    rep(NA_character_, nrow(variables))
  )
  expect_no_error(
    validate_harmonisation_vars(
      variables,
      make_measure_dataschema(),
      character(0)
    )
  )
})

test_that("validate_harmonisation_vars enforces the measure rules", {
  dataschema <- make_measure_dataschema()
  good <- make_measure_variables()

  expect_no_error(validate_harmonisation_vars(good, dataschema, character(0)))

  outside_row <- good[good$target_variable == "age_years", ]
  outside_row$measure <- "diary"
  outside <- dplyr::bind_rows(good, outside_row)
  expect_error(
    validate_harmonisation_vars(outside, dataschema, character(0)),
    "only target screen_time variables.*age_years"
  )

  undescribed <- good[
    !(good$target_variable == "st_responder" & good$measure %in% "diary"),
  ]
  expect_error(
    validate_harmonisation_vars(undescribed, dataschema, character(0)),
    "measure `diary`.*must map st_responder"
  )

  reserved <- good
  reserved$measure[reserved$measure %in% "diary"] <- "primary"
  expect_error(
    validate_harmonisation_vars(reserved, dataschema, character(0)),
    "reserved"
  )

  bad_token <- good
  bad_token$measure[bad_token$measure %in% "diary"] <- "Time Use"
  expect_error(
    validate_harmonisation_vars(bad_token, dataschema, character(0)),
    "lower-case tokens"
  )

  twice <- dplyr::bind_rows(good, good[good$measure %in% "diary", ][1, ])
  expect_error(
    validate_harmonisation_vars(twice, dataschema, character(0)),
    "for one `measure`.*st_measure_type \\[diary\\]"
  )

  # A tagged row does not stand in for the primary row of the same target
  no_primary <- good[
    !(good$target_variable == "st_watch_wd" & is.na(good$measure)),
  ]
  expect_error(
    validate_harmonisation_vars(no_primary, dataschema, character(0)),
    "missing schema variables: st_watch_wd"
  )
})

test_that("sync_harmonisation_vars_file keeps measure rows after the primary rows", {
  dataschema <- make_measure_dataschema()
  variables_file <- withr::local_tempfile(fileext = ".csv")

  # Diary rows first, primary rows out of order, one primary row missing
  scrambled <- make_measure_variables()
  scrambled <- scrambled[c(8:11, 7, 6, 5, 4, 3, 1), ]
  readr::write_csv(scrambled, variables_file, na = "")

  result <- sync_harmonisation_vars_file(variables_file, dataschema)
  expect_identical(result$added_variables, "age_years")
  expect_true(result$changed)

  synced <- readr::read_csv(variables_file, show_col_types = FALSE)
  expect_identical(
    synced$target_variable,
    c(
      "participant_id",
      "age_years",
      "st_measure_type",
      "st_measure_name",
      "st_responder",
      "st_watch_wd",
      "st_game_wd",
      "st_measure_type",
      "st_measure_name",
      "st_responder",
      "st_watch_wd"
    )
  )
  expect_identical(
    harmonisation_measure_of(synced),
    c(rep(NA_character_, 7), rep("diary", 4))
  )
  expect_identical(synced$status[2], "in_progress")

  rerun <- sync_harmonisation_vars_file(variables_file, dataschema)
  expect_false(rerun$changed)
})

test_that("check_harmonisation_exprs reports and fixes per measure", {
  skip_if(!nzchar(bp_air_path()), "Air is not installed")

  variables_file <- withr::local_tempfile(fileext = ".csv")
  variables <- make_measure_variables()
  variables$expression[
    variables$target_variable == "st_watch_wd" & variables$measure %in% "diary"
  ] <- "tv_d /60"
  readr::write_csv(variables, variables_file, na = "")

  issues <- check_harmonisation_exprs(variables_file, fix = TRUE)
  expect_identical(issues$target_variable, "st_watch_wd")
  expect_identical(issues$measure, "diary")
  expect_true(issues$fixed)

  rewritten <- readr::read_csv(variables_file, show_col_types = FALSE)
  is_watch <- rewritten$target_variable == "st_watch_wd"
  expect_identical(
    rewritten$expression[is_watch & is.na(rewritten$measure)],
    "tv_q"
  )
  expect_identical(
    rewritten$expression[is_watch & rewritten$measure %in% "diary"],
    "tv_d / 60"
  )
})
