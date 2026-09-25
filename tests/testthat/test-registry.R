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

# An index of files with the given sizes in bytes, all `data` unless a role
# is given, written to a temp dir so `assign_read_batches()` can stat them.
sized_index <- function(sizes, roles = rep("data", length(sizes))) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  files <- fs::path(dir, paste0("f", seq_along(sizes), ".dat"))
  for (i in seq_along(files)) {
    writeBin(raw(sizes[i]), files[i])
  }
  tibble::tibble(
    seq = seq_along(sizes),
    element_name = paste0("r", seq_along(sizes)),
    role = roles,
    file = as.character(files)
  )
}

test_that("assign_read_batches batches data rows to the requested count", {
  index <- sized_index(
    rep(10L, 7),
    roles = c(rep("data", 5), "codebook", "docs")
  )

  batched <- assign_read_batches(index, size = 2L)

  expect_identical(nrow(batched), 5L)
  expect_identical(batched$tar_group, c(1L, 1L, 2L, 2L, 3L))
})

test_that("assign_read_batches closes a batch before it exceeds the budget", {
  # 40 + 40 fits in 100; adding 30 would not, so a new batch starts there.
  index <- sized_index(c(40L, 40L, 30L, 30L, 30L))

  batched <- assign_read_batches(index, size = 10L, max_bytes = 100)

  expect_identical(batched$tar_group, c(1L, 1L, 2L, 2L, 2L))
})

test_that("assign_read_batches gives an oversized file its own batch", {
  index <- sized_index(c(10L, 500L, 10L, 10L, 500L))

  batched <- assign_read_batches(index, size = 10L, max_bytes = 100)

  expect_identical(batched$tar_group, c(1L, 2L, 3L, 3L, 4L))
})

test_that("assign_read_batches keeps spec order and applies both limits", {
  index <- sized_index(c(60L, 60L, 1L, 1L, 1L, 1L))

  batched <- assign_read_batches(index, size = 3L, max_bytes = 100)

  # 60 alone (60 + 60 > 100); then 60 + 1 + 1 hits the count limit; then 1 + 1.
  expect_identical(batched$tar_group, c(1L, 2L, 2L, 2L, 3L, 3L))
  expect_identical(batched$seq, 1:6)
})

test_that("assign_read_batches treats a missing file as empty", {
  index <- sized_index(c(10L, 10L))
  index$file[2] <- fs::path(fs::path_dir(index$file[1]), "gone.dat")

  expect_identical(assign_read_batches(index)$tar_group, c(1L, 1L))
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

# --- read_tabular_file() -----------------------------------------------------

write_fwf_fixture <- function(dir, lines, layout) {
  data_path <- fs::path(dir, "data.dat")
  writeLines(lines, data_path)
  layout_path <- fs::path(dir, "layout.csv")
  readr::write_csv(layout, layout_path, na = "")
  list(data = data_path, layout = layout_path)
}

fwf_lines <- c("001ALICE 12.5", "002BOB   07.0", "003CAROL 99.9")

fwf_layout <- function(...) {
  tibble::tibble(
    name = c("id", "name", "score"),
    start = c(1L, 4L, 10L),
    end = c(3L, 9L, 13L),
    ...
  )
}

test_that("reader fwf reads a fixed-width file through a layout CSV", {
  fx <- write_fwf_fixture(
    withr::local_tempdir(),
    fwf_lines,
    fwf_layout(type = c("c", "c", "d"), label = c("Id", "Name", "Score"))
  )

  tbl <- read_tabular_file(fx$data, "fwf", list(col_positions = fx$layout))

  expect_s3_class(tbl, "tbl_df")
  expect_identical(names(tbl), c("id", "name", "score"))
  expect_identical(dim(tbl), c(3L, 3L))
  # `type` is honoured: `c` keeps the leading zeros a guess would drop
  expect_identical(tbl$id, c("001", "002", "003"))
  expect_identical(tbl$name, c("ALICE", "BOB", "CAROL"))
  expect_equal(tbl$score, c(12.5, 7, 99.9))
})

test_that("reader fwf lets readr guess types when the layout has none", {
  # No leading zeros here: readr rightly guesses `001` as character.
  fx <- write_fwf_fixture(
    withr::local_tempdir(),
    c("101ALICE 12.5", "102BOB   17.0", "103CAROL 99.9"),
    fwf_layout()
  )

  tbl <- read_tabular_file(fx$data, "fwf", list(col_positions = fx$layout))

  expect_type(tbl$id, "double")
  expect_type(tbl$name, "character")
  expect_type(tbl$score, "double")
})

test_that("reader fwf allows gaps between fields", {
  fx <- write_fwf_fixture(
    withr::local_tempdir(),
    fwf_lines,
    tibble::tibble(
      name = c("id", "score"),
      start = c(1L, 10L),
      end = c(3L, 13L)
    )
  )

  tbl <- read_tabular_file(fx$data, "fwf", list(col_positions = fx$layout))

  expect_identical(names(tbl), c("id", "score"))
  expect_identical(nrow(tbl), 3L)
})

test_that("reader fwf needs col_positions and an existing layout file", {
  dir <- withr::local_tempdir()
  fx <- write_fwf_fixture(dir, fwf_lines, fwf_layout())

  expect_error(
    read_tabular_file(fx$data, "fwf", list()),
    "needs `col_positions`"
  )
  expect_error(
    read_tabular_file(
      fx$data,
      "fwf",
      list(col_positions = fs::path(dir, "missing.csv"))
    ),
    "layout file not found"
  )
})

test_that("reader fwf rejects a malformed layout", {
  dir <- withr::local_tempdir()
  read_with <- function(layout) {
    fx <- write_fwf_fixture(dir, fwf_lines, layout)
    read_tabular_file(fx$data, "fwf", list(col_positions = fx$layout))
  }
  layout <- fwf_layout()

  expect_error(
    read_with(layout[, c("name", "start")]),
    "missing required column\\(s\\) end"
  )
  expect_error(
    read_with(dplyr::mutate(layout, name = c("id", "id", "score"))),
    "duplicate field names: id"
  )
  expect_error(
    read_with(dplyr::mutate(layout, end = c(4L, 9L, 13L))),
    "overlapping fields: id/name"
  )
  expect_error(
    read_with(dplyr::mutate(
      layout,
      start = c(1L, 9L, 10L),
      end = c(3L, 4L, 13L)
    )),
    "`start` is after `end` for: name"
  )
  expect_error(
    read_with(layout[c(2, 1, 3), ]),
    "ascending `start` order"
  )
  expect_error(
    read_with(dplyr::mutate(layout, start = c(0L, 4L, 10L))),
    "1-based"
  )
  expect_error(
    read_with(dplyr::mutate(layout, type = c("c", "x", "d"))),
    "unknown `type` value\\(s\\) x"
  )
  expect_error(
    read_with(dplyr::mutate(layout, start = c("1", "four", "10"))),
    "whole numbers"
  )
})

test_that("reader fwf warns when the layout is wider than the record", {
  fx <- write_fwf_fixture(
    withr::local_tempdir(),
    fwf_lines,
    dplyr::mutate(fwf_layout(), end = c(3L, 9L, 14L))
  )

  expect_warning(
    tbl <- read_tabular_file(fx$data, "fwf", list(col_positions = fx$layout)),
    "ends at position 14 but the first record .* is 13 bytes wide"
  )
  expect_identical(nrow(tbl), 3L)
})

test_that("col_names = FALSE keeps the first row of a headerless file", {
  path <- fs::path(withr::local_tempdir(), "data.dat")
  writeLines(c("1\t5\t9", "2\t6\t10"), path)

  tbl <- read_tabular_file(path, "tsv", list(col_names = FALSE))

  expect_identical(dim(tbl), c(2L, 3L))
  expect_identical(names(tbl), c("X1", "X2", "X3"))
  expect_identical(tbl$X1, c(1, 2))

  # The regression this guards: assume a header and the first participant
  # silently becomes the column names.
  expect_identical(nrow(read_tabular_file(path, "tsv")), 1L)
})

test_that("encoding reaches the delimited readers", {
  path <- fs::path(withr::local_tempdir(), "data.csv")
  # "name\nJos\xe9\n" with the e-acute as a single latin1 byte
  writeBin(
    c(charToRaw("name\n"), as.raw(c(0x4a, 0x6f, 0x73, 0xe9)), charToRaw("\n")),
    path
  )

  tbl <- read_tabular_file(path, "csv", list(encoding = "latin1"))

  expect_identical(enc2utf8(tbl$name), "José")
})

test_that("reader por dispatches to haven::read_por", {
  seen <- NULL
  testthat::local_mocked_bindings(
    read_por = function(file, ...) {
      seen <<- file
      data.frame(CASEID = c(1, 2), T2E1 = c(3, 4))
    },
    .package = "haven"
  )

  tbl <- read_tabular_file("study.por", "por")

  expect_identical(seen, "study.por")
  expect_s3_class(tbl, "tbl_df")
  expect_identical(tbl$T2E1, c(3, 4))
})

test_that("columns keeps only the matching columns of a haven read", {
  path <- fs::path(withr::local_tempdir(), "a_indresp.dta")
  haven::write_dta(
    tibble::tibble(
      pidp = c(1, 2),
      a_hidp = c(10, 20),
      a_pno = c(1, 2),
      a_hiqual_dv = c(3, 4),
      a_other = c(5, 6)
    ),
    path
  )

  tbl <- read_tabular_file(
    path,
    "stata",
    list(columns = c("pidp", "[a-o]_(hidp|pno)", "[a-o]_hiqual_dv"))
  )

  # Order follows the file, not the pattern list.
  expect_identical(names(tbl), c("pidp", "a_hidp", "a_pno", "a_hiqual_dv"))
  expect_identical(as.numeric(tbl$a_hiqual_dv), c(3, 4))
})

test_that("columns patterns match whole names and must each match", {
  path <- fs::path(withr::local_tempdir(), "data.dta")
  haven::write_dta(tibble::tibble(pidp = 1, pidp_old = 2, age = 3), path)

  # Anchored: "pidp" does not also pick up "pidp_old".
  tbl <- read_tabular_file(path, "stata", list(columns = "pidp"))
  expect_identical(names(tbl), "pidp")

  expect_error(
    read_tabular_file(path, "stata", list(columns = c("pidp", "agee"))),
    "`columns` pattern\\(s\\) matched no column in .*: `agee`"
  )
})

test_that("read options are refused on readers that cannot honour them", {
  expect_error(
    read_tabular_file("x.csv", "csv", list(columns = "id")),
    "`columns` is not supported by reader `csv`"
  )
  expect_error(
    read_tabular_file("x.por", "por", list(encoding = "latin1")),
    "`encoding` is not supported by reader `por`"
  )
  expect_error(
    read_tabular_file("x.csv", "csv", list(col_positions = "layout.csv")),
    "`col_positions` is not supported by reader `csv`"
  )
  expect_error(
    read_tabular_file("x.dta", "stata", list(col_names = FALSE)),
    "`col_names` is not supported by reader `stata`"
  )
})

test_that("a read with parse failures errors instead of returning NAs", {
  path <- fs::path(withr::local_tempdir(), "data.csv")
  writeLines(c("id,score,note", "1,12.5,a", "2,n/a,b", "3,.,c"), path)

  # readr guesses `score` as character here, so the read is clean...
  expect_no_error(read_tabular_file(path, "csv"))

  # ...but a fixed-width layout that types it numeric is refused, naming the
  # column, the count and the first offending value.
  fwf_dir <- withr::local_tempdir()
  fx <- write_fwf_fixture(
    fwf_dir,
    c("001 12.5", "002  n/a", "003    ."),
    tibble::tibble(
      name = c("id", "score"),
      start = c(1L, 5L),
      end = c(3L, 8L),
      type = c("c", "d")
    )
  )
  err <- expect_error(
    suppressWarnings(
      read_tabular_file(fx$data, "fwf", list(col_positions = fx$layout))
    ),
    "2 cell\\(s\\) in .* did not parse"
  )
  expect_match(
    conditionMessage(err),
    "Columns affected: score (2)",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "expected a double, got `n/a`",
    fixed = TRUE
  )
})

test_that("report_parse_problems is a no-op for non-readr tables", {
  expect_no_error(report_parse_problems(data.frame(a = 1), "x.por"))
  expect_no_error(report_parse_problems(tibble::tibble(a = 1), "x.dta"))
})

test_that("an unsupported reader still errors", {
  expect_error(
    read_tabular_file("x.foo", "foo"),
    "Unsupported reader: foo"
  )
})

# --- read options in the resource index --------------------------------------

test_that("build_resource_index resolves col_positions against the spec dir", {
  base_dir <- local_resource_dir("data.dat")
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(
        name = "data",
        role = "data",
        glob = "data.dat",
        reader = "fwf",
        col_positions = "layouts/data.csv",
        encoding = "latin1"
      )
    )
  )

  index <- build_resource_index(base_dir, spec)
  opts <- index$read_opts[[1]]
  expect_null(opts$columns)

  expect_identical(
    opts$col_positions,
    as.character(fs::path("harmonisation/datasets/BPIPD-9999/layouts/data.csv"))
  )
  expect_identical(opts$encoding, "latin1")
  expect_null(opts$col_names)
  expect_identical(read_opt_files(index), opts$col_positions)
})

test_that("build_resource_index carries columns into read_opts", {
  base_dir <- local_resource_dir("data.dta")
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(
        name = "data",
        role = "data",
        glob = "data.dta",
        reader = "stata",
        columns = c("pidp", "[a-o]_hidp")
      )
    )
  )

  opts <- build_resource_index(base_dir, spec)$read_opts[[1]]

  expect_identical(opts$columns, c("pidp", "[a-o]_hidp"))
})

test_that("build_resource_index rejects an absolute col_positions", {
  base_dir <- local_resource_dir("data.dat")
  spec <- list(
    dataset_id = "9999",
    resources = list(
      list(
        name = "data",
        role = "data",
        glob = "data.dat",
        reader = "fwf",
        col_positions = "/etc/layout.csv"
      )
    )
  )

  expect_error(build_resource_index(base_dir, spec), "must be relative")
})

test_that("read_opt_files lists layout paths once and drops resources without", {
  index <- tibble::tibble(
    read_opts = list(
      list(col_positions = "a.csv"),
      list(sheet = "Data"),
      list(col_positions = "a.csv"),
      list(col_positions = "b.csv")
    )
  )

  expect_identical(read_opt_files(index), c("a.csv", "b.csv"))
  expect_identical(read_opt_files(empty_resource_index()), character(0))
})

test_that("read_dataset_from_spec reads a fixed-width resource end to end", {
  # `col_positions` resolves against the repo-relative spec directory, so run
  # from a scratch root that has one.
  root <- withr::local_tempdir()
  withr::local_dir(root)
  layout_dir <- fs::path(bp_harmonisation_dataset_dir("9999"), "layouts")
  fs::dir_create(layout_dir)
  readr::write_csv(
    fwf_layout(type = c("c", "c", "d")),
    fs::path(layout_dir, "data.csv")
  )
  data_dir <- fs::path(root, "BPIPD-9999 - Example Cohort Study")
  fs::dir_create(data_dir)
  writeLines(fwf_lines, fs::path(data_dir, "data.dat"))

  spec <- list(
    dataset_id = "9999",
    dataset_name = "Example Cohort Study",
    status = "in_progress",
    resources = list(
      list(
        name = "data",
        role = "data",
        glob = "data.dat",
        reader = "fwf",
        col_positions = "layouts/data.csv"
      )
    )
  )

  raw <- read_dataset_from_spec(data_dir, spec)

  expect_identical(names(raw$data), "data")
  expect_identical(dim(raw$data$data), c(3L, 3L))
  expect_identical(raw$data$data$id, c("001", "002", "003"))
})
