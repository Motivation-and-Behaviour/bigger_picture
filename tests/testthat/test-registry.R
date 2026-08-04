local_resource_dir <- function(files, env = parent.frame()) {
  base_dir <- withr::local_tempdir(.local_envir = env)
  for (file in files) {
    path <- fs::path(base_dir, file)
    fs::dir_create(fs::path_dir(path))
    fs::file_create(path)
  }
  base_dir
}

relative_matches <- function(base_dir, glob) {
  matches <- resolve_glob_paths(base_dir, glob)
  sort(as.character(fs::path_rel(matches, start = base_dir)))
}

test_that("resolve_glob_paths matches exact and wildcard globs", {
  base_dir <- local_resource_dir(c(
    "data.sav",
    "extra.sav",
    "stata/a.dta",
    "stata/b.dta",
    "docs/report.pdf"
  ))

  expect_identical(relative_matches(base_dir, "data.sav"), "data.sav")
  expect_identical(
    relative_matches(base_dir, "stata/*.dta"),
    c("stata/a.dta", "stata/b.dta")
  )
  expect_identical(relative_matches(base_dir, "missing.sav"), character(0))
})

test_that("resolve_glob_paths accepts a list of globs", {
  base_dir <- local_resource_dir(c("a.csv", "b.csv", "c.csv"))

  expect_identical(
    relative_matches(base_dir, list("a.csv", "c.csv")),
    c("a.csv", "c.csv")
  )
})

test_that("resolve_glob_paths expands brace alternatives", {
  base_dir <- local_resource_dir(c(
    "child.sav",
    "parent.sav",
    "teacher.sav",
    "waves/w1/data.csv",
    "waves/w2/data.csv"
  ))

  expect_identical(
    relative_matches(base_dir, "{child.sav,parent.sav}"),
    c("child.sav", "parent.sav")
  )
  # Whitespace after commas is tolerated
  expect_identical(
    relative_matches(base_dir, "{child.sav, teacher.sav}"),
    c("child.sav", "teacher.sav")
  )
  # Braces combine with wildcards and paths
  expect_identical(
    relative_matches(base_dir, "waves/{w1,w2}/*.csv"),
    c("waves/w1/data.csv", "waves/w2/data.csv")
  )
})

test_that("resolve_glob_paths warns and returns empty for a missing dir", {
  missing_dir <- fs::path(withr::local_tempdir(), "does-not-exist")

  expect_warning(
    result <- resolve_glob_paths(missing_dir, "*.csv"),
    "Base directory does not exist"
  )
  expect_identical(result, character(0))
})

make_resource <- function(name, role = "data") {
  list(name = name, role = role, glob = paste0(name, ".csv"), reader = "csv")
}

make_wave_spec <- function(...) {
  waves <- list(...)
  list(
    dataset_id = "9999",
    dataset_name = "Example Cohort Study",
    .spec_file = "harmonisation/datasets/BPIPD-9999/dataset.yaml",
    waves = waves
  )
}

test_that("check_resource_names accepts names that are unique", {
  spec <- make_wave_spec(
    list(
      wave = "2010",
      wave_dir = "w1",
      resources = list(make_resource("data_2010"))
    ),
    list(
      wave = "2014",
      wave_dir = "w2",
      resources = list(make_resource("data_2014"))
    )
  )

  expect_no_error(check_resource_names(spec))
})

test_that("check_resource_names rejects a name reused across waves", {
  spec <- make_wave_spec(
    list(
      wave = "2010",
      wave_dir = "w1",
      resources = list(make_resource("data"))
    ),
    list(
      wave = "2014",
      wave_dir = "w2",
      resources = list(make_resource("data"))
    )
  )

  err <- expect_error(check_resource_names(spec), "Duplicate resource names")
  message <- conditionMessage(err)

  # The message has to say which spec, which name, and where it came from,
  # because the spec that triggers this is usually hundreds of lines long.
  expect_match(message, "BPIPD-9999/dataset.yaml", fixed = TRUE)
  expect_match(message, "`data` is declared 2 times", fixed = TRUE)
  expect_match(message, "wave 2010, wave 2014", fixed = TRUE)
})

test_that("check_resource_names rejects a name reused across roles", {
  spec <- list(
    dataset_id = "9999",
    resources = list(
      make_resource("survey", role = "data"),
      make_resource("survey", role = "codebook")
    )
  )

  expect_error(check_resource_names(spec), "`survey` is declared 2 times")
})

test_that("check_resource_names rejects a name reused within one wave", {
  spec <- make_wave_spec(
    list(
      wave = "2010",
      wave_dir = "w1",
      resources = list(make_resource("data"), make_resource("data"))
    )
  )

  expect_error(check_resource_names(spec), "`data` is declared 2 times")
})

test_that("check_resource_names truncates long duplicate lists", {
  resources <- lapply(paste0("data_", 1:6), make_resource)
  spec <- make_wave_spec(
    list(wave = "2010", wave_dir = "w1", resources = resources),
    list(wave = "2014", wave_dir = "w2", resources = resources)
  )

  err <- expect_error(check_resource_names(spec))
  expect_match(
    conditionMessage(err),
    "and 1 more duplicated name",
    fixed = TRUE
  )
})

test_that("check_resource_names falls back to the dataset id for the label", {
  spec <- list(
    dataset_id = "9999",
    resources = list(make_resource("data"), make_resource("data"))
  )

  expect_error(
    check_resource_names(spec),
    "BPIPD-9999/dataset.yaml",
    fixed = TRUE
  )
})

test_that("read_dataset_from_spec refuses a spec with duplicate names", {
  base_dir <- local_resource_dir(c("w1/data.csv", "w2/data.csv"))
  spec <- make_wave_spec(
    list(
      wave = "2010",
      wave_dir = "w1",
      resources = list(make_resource("data"))
    ),
    list(
      wave = "2014",
      wave_dir = "w2",
      resources = list(make_resource("data"))
    )
  )

  expect_error(
    read_dataset_from_spec(base_dir, spec),
    "Duplicate resource names"
  )
})
