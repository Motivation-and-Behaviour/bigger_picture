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
