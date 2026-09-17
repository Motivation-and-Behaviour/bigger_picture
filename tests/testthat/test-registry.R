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

# ---- resolve_glob_paths(files = ) ------------------------------------------

test_that("a supplied listing resolves the same paths as walking the dir", {
  base_dir <- local_resource_dir(c(
    "data.sav",
    "extra.sav",
    "stata/a.dta",
    "waves/w1/data.csv",
    "waves/w2/data.csv"
  ))
  files <- list_files_under(base_dir)
  rel <- relative_to_base(files, base_dir)

  for (glob in list(
    "data.sav",
    "*.sav",
    "stata/*.dta",
    "waves/{w1,w2}/*.csv",
    "nothing-here.csv"
  )) {
    walked <- resolve_glob_paths(base_dir, glob)
    expect_identical(resolve_glob_paths(base_dir, glob, files = files), walked)
    expect_identical(
      resolve_glob_paths(base_dir, glob, files = files, rel = rel),
      walked,
      info = glob
    )
  }
})

test_that("relative_to_base survives an empty listing", {
  base_dir <- local_resource_dir(character())

  expect_length(relative_to_base(list_files_under(base_dir), base_dir), 0)
})

test_that("list_files_under warns and returns empty for a missing dir", {
  missing_dir <- fs::path(withr::local_tempdir(), "does-not-exist")

  expect_warning(
    result <- list_files_under(missing_dir),
    "Base directory does not exist"
  )
  expect_length(result, 0)
})

# ---- build_resource_index --------------------------------------------------

test_that("build_resource_index names multi-match globs but not single ones", {
  base_dir <- local_resource_dir(c("one.csv", "many_a.csv", "many_b.csv"))
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(name = "one", role = "data", glob = "one.csv", reader = "csv"),
      list(name = "many", role = "data", glob = "many_*.csv", reader = "csv")
    )
  )

  index <- build_resource_index(base_dir, spec)

  expect_identical(index$element_name, c("one", "many__1", "many__2"))
  expect_identical(index$resource_name, c("one", "many", "many"))
  expect_identical(index$seq, 1:3)
})

test_that("build_resource_index keeps top-level resources ahead of waves", {
  base_dir <- local_resource_dir(c("top.csv", "w1/a.csv", "w2/b.csv"))
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(name = "top", role = "data", glob = "top.csv", reader = "csv")
    ),
    waves = list(
      list(
        wave = "2010",
        label = "Wave 1",
        wave_dir = "w1",
        resources = list(
          list(name = "a", role = "data", glob = "a.csv", reader = "csv")
        )
      ),
      list(
        wave = "2014",
        wave_dir = "w2",
        resources = list(
          list(name = "b", role = "data", glob = "b.csv", reader = "csv")
        )
      )
    )
  )

  index <- build_resource_index(base_dir, spec)

  expect_identical(index$element_name, c("top", "a", "b"))
  # Top-level rows carry no wave; `label` falls back to the wave identifier.
  expect_identical(index$wave, c(NA, "2010", "2014"))
  expect_identical(index$wave_label, c(NA, "Wave 1", "2014"))
})

test_that("build_resource_index drops resources that match no files", {
  base_dir <- local_resource_dir("present.csv")
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(
        name = "present",
        role = "data",
        glob = "present.csv",
        reader = "csv"
      ),
      list(name = "absent", role = "data", glob = "absent.csv", reader = "csv")
    )
  )

  index <- build_resource_index(base_dir, spec)

  expect_identical(index$element_name, "present")
  expect_identical(nrow(index), 1L)
})

test_that("build_resource_index splits roles the way the reader expects", {
  base_dir <- local_resource_dir(c("d.csv", "cb.pdf", "syntax.do"))
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(name = "d", role = "data", glob = "d.csv", reader = "csv"),
      list(name = "cb", role = "codebook", glob = "cb.pdf"),
      list(name = "syn", role = "docs", glob = "syntax.do")
    )
  )

  index <- build_resource_index(base_dir, spec)

  expect_identical(index$role, c("data", "codebook", "docs"))
  expect_identical(assign_read_batches(index)$element_name, "d")
})

test_that("assign_read_batches batches data rows to the requested size", {
  index <- tibble::tibble(
    seq = 1:7,
    element_name = paste0("r", 1:7),
    role = c(rep("data", 5), "codebook", "docs")
  )

  batched <- assign_read_batches(index, size = 2L)

  expect_identical(nrow(batched), 5L)
  expect_identical(batched$tar_group, c(1L, 1L, 2L, 2L, 3L))
})

test_that("assign_read_batches refuses an index with no data files", {
  index <- build_resource_index(
    local_resource_dir("cb.pdf"),
    list(
      dataset_id = "9999",
      resources = list(
        list(name = "cb", role = "codebook", glob = "cb.pdf")
      )
    )
  )

  expect_error(assign_read_batches(index), "No data files matched")
})

# ---- read and assemble -----------------------------------------------------

test_that("assemble_raw_dataset restores spec order from unordered branches", {
  index <- tibble::tibble(
    seq = 1:4,
    resource_name = c("a", "b", "cb", "syn"),
    element_name = c("a", "b", "cb", "syn"),
    role = c("data", "data", "codebook", "docs"),
    base_dir = rep("/base", 4),
    file = c("/base/a.csv", "/base/b.csv", "/base/cb.pdf", "/base/syn.do")
  )
  # Branch 2 arrives before branch 1.
  parts <- list(
    list(list(seq = 2L, name = "b", table = tibble::tibble(x = 2))),
    list(list(seq = 1L, name = "a", table = tibble::tibble(x = 1)))
  )

  raw <- assemble_raw_dataset(parts, index, spec = list(id = "9999"), "/base")

  expect_identical(names(raw$data), c("a", "b"))
  expect_identical(raw$data$a$x, 1)
  expect_identical(raw$codebook, list(cb = "/base/cb.pdf"))
  expect_identical(raw$docs, "/base/syn.do")
  expect_identical(raw$meta$matches$file, index$file)
  expect_identical(raw$meta$dataset_dir, "/base")
})

test_that("assemble_raw_dataset returns bare empty lists when nothing matched", {
  index <- build_resource_index(
    local_resource_dir("present.csv"),
    list(dataset_id = "9999", resources = list())
  )

  raw <- assemble_raw_dataset(list(), index, spec = list(), "/base")

  expect_identical(raw$data, list())
  expect_identical(raw$codebook, list())
  expect_identical(raw$docs, character(0))
})

test_that("read_resource_files tags wave columns only for wave resources", {
  base_dir <- withr::local_tempdir()
  fs::dir_create(fs::path(base_dir, "w1"))
  utils::write.csv(
    data.frame(id = 1:2),
    fs::path(base_dir, "top.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(id = 3:4),
    fs::path(base_dir, "w1", "a.csv"),
    row.names = FALSE
  )

  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(name = "top", role = "data", glob = "top.csv", reader = "csv")
    ),
    waves = list(
      list(
        wave = "2010",
        label = "Wave 1",
        wave_dir = "w1",
        resources = list(
          list(name = "a", role = "data", glob = "a.csv", reader = "csv")
        )
      )
    )
  )

  index <- build_resource_index(base_dir, spec)
  parts <- read_resource_files(index)
  raw <- assemble_raw_dataset(list(parts), index, spec, base_dir)

  expect_false(".wave" %in% names(raw$data$top))
  expect_identical(raw$data$a$.wave, c("2010", "2010"))
  expect_identical(raw$data$a$.wave_label, c("Wave 1", "Wave 1"))
})

test_that("read_dataset_from_spec matches a batched read", {
  base_dir <- withr::local_tempdir()
  for (i in 1:5) {
    utils::write.csv(
      data.frame(id = i),
      fs::path(base_dir, paste0("f", i, ".csv")),
      row.names = FALSE
    )
  }
  spec <- list(
    dataset_id = "9999",
    resources = lapply(1:5, function(i) {
      list(
        name = paste0("f", i),
        role = "data",
        glob = paste0("f", i, ".csv"),
        reader = "csv"
      )
    })
  )

  index <- build_resource_index(base_dir, spec)
  batched <- assign_read_batches(index, size = 2L)
  parts <- lapply(
    split(batched, batched$tar_group),
    read_resource_files
  )

  expect_identical(
    assemble_raw_dataset(parts, index, spec, base_dir),
    read_dataset_from_spec(base_dir, spec)
  )
})
