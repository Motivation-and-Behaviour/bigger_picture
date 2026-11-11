# Dataset citations and acknowledgements (references/). Everything here runs
# offline: Airtable is replaced by fake records shaped like the parsed JSON
# that httr2::resp_body_json() returns. IDs and keys are fictional.

fake_dataset_record <- function(id, citations = NULL, acknowledgement = NULL) {
  fields <- list(`Dataset ID` = id)
  if (!is.null(citations)) {
    fields$Citations <- as.list(citations)
  }
  if (!is.null(acknowledgement)) {
    fields$`Required Acknowledgements` <- acknowledgement
  }
  list(id = paste0("rec", gsub("[^0-9]", "", id)), fields = fields)
}

fake_citation_record <- function(id, key = NULL) {
  fields <- if (is.null(key)) list() else list(`Citation Key` = key)
  list(id = id, fields = fields)
}

fake_specs <- function() {
  list(
    list(dataset_id = "9999", status = "harmonised"),
    list(dataset_id = "12", status = "in_progress"),
    list(dataset_id = "5", status = "harmonised"),
    list(dataset_id = "7", status = "blocked")
  )
}

write_example_bib <- function(path, keys) {
  writeLines(
    unlist(lapply(keys, function(key) {
      c(paste0("@article{", key, ","), "  title = {Example},", "}", "")
    })),
    path
  )
}

test_that("build_dataset_references() turns records into long, sorted tables", {
  datasets <- list(
    fake_dataset_record("BPIPD-9999", c("recB", "recA")),
    fake_dataset_record("BPIPD-12", "recA"),
    fake_dataset_record(
      "BPIPD-100",
      acknowledgement = "Line one\r\nLine two  "
    ),
    fake_dataset_record("BPIPD-5", acknowledgement = "   ")
  )
  citations <- list(
    fake_citation_record("recA", "example2020cohort"),
    fake_citation_record("recB", " doe2019:guide "),
    fake_citation_record("recBlank")
  )

  out <- build_dataset_references(datasets, citations)

  expect_identical(names(out$citations), bp_dataset_citations_columns())
  expect_identical(out$citations$dataset_id, c("12", "9999", "9999"))
  expect_identical(
    out$citations$citekey,
    c("example2020cohort", "doe2019:guide", "example2020cohort")
  )
  expect_identical(
    names(out$acknowledgements),
    bp_acknowledgements_columns()
  )
  expect_identical(out$acknowledgements$dataset_id, "100")
  expect_identical(out$acknowledgements$acknowledgement, "Line one\nLine two")
})

test_that("build_dataset_references() keeps the columns when there is nothing", {
  out <- build_dataset_references(list(fake_dataset_record("BPIPD-1")), list())

  expect_identical(names(out$citations), bp_dataset_citations_columns())
  expect_identical(nrow(out$citations), 0L)
  expect_identical(
    readr::format_csv(out$citations, na = ""),
    "dataset_id,citekey\n"
  )
  expect_identical(nrow(out$acknowledgements), 0L)
})

test_that("build_dataset_references() names the records it cannot use", {
  expect_error(
    build_dataset_references(list(fake_dataset_record("BPIPD-12a")), list()),
    "BPIPD-12a"
  )
  expect_error(
    build_dataset_references(list(list(id = "rec1", fields = list())), list()),
    "\\(blank\\)"
  )
  expect_error(
    build_dataset_references(
      list(fake_dataset_record("BPIPD-3"), fake_dataset_record("BPIPD-3")),
      list()
    ),
    "share a `Dataset ID`: BPIPD-3"
  )
  expect_error(
    build_dataset_references(
      list(fake_dataset_record("BPIPD-3", "recBlank")),
      list(fake_citation_record("recBlank"))
    ),
    "no `Citation Key`: BPIPD-3"
  )
  expect_error(
    build_dataset_references(
      list(fake_dataset_record("BPIPD-3", "recGone")),
      list()
    ),
    "no `Citation Key`: BPIPD-3"
  )
  expect_error(
    build_dataset_references(
      list(fake_dataset_record("BPIPD-3")),
      list(
        fake_citation_record("recA", "example2020"),
        fake_citation_record("recB", "example2020")
      )
    ),
    "share a `Citation Key`: example2020"
  )
})

test_that("read_bib_keys() reads entry keys and skips non-entries", {
  bib <- withr::local_tempfile(fileext = ".bib")
  writeLines(
    c(
      "% Exported by Better BibTeX",
      "@string{ije = \"Int J Epidemiol\"}",
      "@comment{jabref-meta: x,}",
      "@Article{smith2020:cohort-profile_v2,",
      "  title = {Cohort profile},",
      "}",
      "  @techreport{ doe.2019 ,",
      "  title = {User guide},",
      "}"
    ),
    bib
  )
  expect_identical(
    read_bib_keys(bib),
    c("smith2020:cohort-profile_v2", "doe.2019")
  )

  empty <- withr::local_tempfile(fileext = ".bib")
  file.create(empty)
  expect_identical(read_bib_keys(empty), character())

  write_example_bib(bib, c("example2020", "example2020"))
  expect_error(read_bib_keys(bib), "more than once: example2020")
})

test_that("read_dataset_citations() checks IDs, duplicates and the bib", {
  dir <- withr::local_tempdir()
  bib <- file.path(dir, "datasets.bib")
  csv <- file.path(dir, "dataset_citations.csv")
  write_example_bib(bib, c("example2020cohort", "doe2019"))

  writeLines(
    c("dataset_id,citekey", "9999,example2020cohort", "9999,doe2019"),
    csv
  )
  out <- read_dataset_citations(csv, bib)
  expect_identical(out$citekey, c("example2020cohort", "doe2019"))

  writeLines(c("dataset_id,citekey", "BPIPD-9999,doe2019"), csv)
  expect_error(read_dataset_citations(csv, bib), "digits only")

  writeLines(c("dataset_id,citekey", "9999,doe2019", "9999,doe2019"), csv)
  expect_error(read_dataset_citations(csv, bib), "more than once")

  writeLines(c("dataset_id,citekey", "9999,"), csv)
  expect_error(read_dataset_citations(csv, bib), "blank for BPIPD-9999")

  writeLines(c("dataset_id,citekey", "9999,missing2020", "12,missing2020"), csv)
  expect_error(
    read_dataset_citations(csv, bib),
    "not in datasets.bib: missing2020 \\(BPIPD-9999, BPIPD-12\\)"
  )
})

test_that("read_dataset_acknowledgements() keeps multi-line text", {
  csv <- withr::local_tempfile(fileext = ".csv")
  text <- "The data were provided by an example agency.\nThe views are ours."
  readr::write_csv(
    tibble::tibble(dataset_id = "9999", acknowledgement = text),
    csv,
    na = ""
  )
  out <- read_dataset_acknowledgements(csv)
  expect_identical(out$acknowledgement, text)

  writeLines(c("dataset_id,acknowledgement", "9999,One", "9999,Two"), csv)
  expect_error(
    read_dataset_acknowledgements(csv),
    "more than one row for BPIPD-9999"
  )

  writeLines(c("dataset_id,acknowledgement", "9999,"), csv)
  expect_error(read_dataset_acknowledgements(csv), "blank for BPIPD-9999")
})

test_that("check_citation_coverage() lists datasets with a status and no citation", {
  citations <- tibble::tibble(dataset_id = "9999", citekey = "example2020")

  expect_identical(
    check_citation_coverage(citations, fake_specs(), "harmonised"),
    "5"
  )
  expect_identical(
    check_citation_coverage(
      citations,
      fake_specs(),
      c("in_progress", "harmonised")
    ),
    c("5", "12")
  )
  expect_identical(
    check_citation_coverage(citations, fake_specs(), character()),
    character()
  )
})

test_that("airtable_list_request() asks only for the named fields", {
  req <- airtable_list_request(
    "Datasets",
    c("Dataset ID", "Citations"),
    "appEXAMPLE",
    "not-a-real-token",
    filter = "{Status} = 'Included'",
    offset = "itrEXAMPLE/recEXAMPLE"
  )

  expect_match(req$url, "^https://api.airtable.com/v0/appEXAMPLE/Datasets\\?")
  expect_identical(
    lengths(regmatches(req$url, gregexpr("fields%5B%5D=", req$url))),
    2L
  )
  expect_match(req$url, "fields%5B%5D=Dataset%20ID", fixed = TRUE)
  expect_match(req$url, "fields%5B%5D=Citations", fixed = TRUE)
  expect_match(req$url, "filterByFormula=%7BStatus%7D", fixed = TRUE)
  expect_match(req$url, "pageSize=100", fixed = TRUE)
  expect_match(req$url, "offset=itrEXAMPLE", fixed = TRUE)
  expect_false(grepl(
    "not-a-real-token",
    paste(utils::capture.output(print(req)), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("airtable_error_body() passes on Airtable's explanation", {
  json_response <- function(body) {
    httr2::response(
      status_code = 422,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(body)
    )
  }
  expect_identical(
    airtable_error_body(json_response(
      "{\"error\":{\"type\":\"UNKNOWN_FIELD_NAME\",\"message\":\"Unknown field name\"}}"
    )),
    "UNKNOWN_FIELD_NAME: Unknown field name"
  )
  expect_identical(
    airtable_error_body(json_response("{\"error\":\"NOT_FOUND\"}")),
    "NOT_FOUND"
  )
})

test_that("sync_dataset_references() writes the snapshot from whitelisted fields", {
  withr::local_dir(withr::local_tempdir())
  write_example_bib("zotero.bib", "example2020cohort")

  calls <- list()
  fake_fetch <- function(table, fields, base_id, token, filter = NULL) {
    calls[[length(calls) + 1]] <<- list(
      table = table,
      fields = fields,
      filter = filter
    )
    if (table == "Datasets") {
      list(fake_dataset_record(
        "BPIPD-9999",
        "recA",
        "Thanks to the example agency."
      ))
    } else {
      list(fake_citation_record("recA", "example2020cohort"))
    }
  }
  sync <- function() {
    sync_dataset_references(
      base_id = "appEXAMPLE",
      token = "not-a-real-token",
      bib_source = "zotero.bib",
      specs = fake_specs(),
      fetch = fake_fetch
    )
  }

  summary <- NULL
  expect_message(summary <- sync(), "1 Included datasets, 1 with citations")

  airtable <- bp_airtable_reference_fields()
  expect_identical(
    calls,
    list(
      list(
        table = airtable$datasets_table,
        fields = airtable$dataset_fields,
        filter = airtable$included_filter
      ),
      list(
        table = airtable$citations_table,
        fields = airtable$citation_fields,
        filter = NULL
      )
    )
  )
  expect_identical(
    read_dataset_citations()$citekey,
    "example2020cohort"
  )
  expect_identical(
    read_dataset_acknowledgements()$acknowledgement,
    "Thanks to the example agency."
  )
  expect_identical(
    readLines(bp_dataset_bib_path()),
    readLines("zotero.bib")
  )
  expect_identical(summary$missing_citations, c("5", "12"))
  expect_identical(summary$not_included, c("5", "12"))
  expect_length(summary$written, 3)

  expect_message(summary <- sync(), "already up to date")
  expect_length(summary$written, 0)
  unlink(bp_references_dir(), recursive = TRUE)
  expect_message(
    sync_dataset_references(
      base_id = "appEXAMPLE",
      token = "not-a-real-token",
      bib_source = "zotero.bib",
      write = FALSE,
      specs = fake_specs(),
      fetch = fake_fetch
    ),
    "write = FALSE"
  )
  expect_false(dir.exists(bp_references_dir()))
})

test_that("sync_dataset_references() writes nothing when a key is not in the bib", {
  withr::local_dir(withr::local_tempdir())
  write_example_bib("zotero.bib", "example2020cohort")
  fake_fetch <- function(table, fields, base_id, token, filter = NULL) {
    if (table == "Datasets") {
      list(fake_dataset_record("BPIPD-9999", "recA"))
    } else {
      list(fake_citation_record("recA", "missing2021"))
    }
  }

  expect_error(
    sync_dataset_references(
      base_id = "appEXAMPLE",
      token = "not-a-real-token",
      bib_source = "zotero.bib",
      specs = list(),
      fetch = fake_fetch
    ),
    "Nothing was written.*missing2021 \\(BPIPD-9999\\)"
  )
  expect_false(dir.exists(bp_references_dir()))
})

test_that("sync_dataset_references() needs credentials and a bib", {
  withr::local_dir(withr::local_tempdir())
  withr::local_envvar(c(AIRTABLE_BASE_ID = NA, AIRTABLE_TOKEN = NA))
  no_fetch <- function(...) stop("fetch should not be called")

  expect_error(
    sync_dataset_references(bib_source = "", fetch = no_fetch),
    "Add AIRTABLE_BASE_ID and AIRTABLE_TOKEN"
  )
  expect_error(
    sync_dataset_references(
      "appEXAMPLE",
      "x",
      bib_source = "nowhere.bib",
      fetch = no_fetch
    ),
    "BP_BIB_SOURCE does not exist"
  )
  expect_error(
    sync_dataset_references(
      "appEXAMPLE",
      "x",
      bib_source = "",
      fetch = no_fetch
    ),
    "export the Zotero collection there first"
  )
})

test_that("the committed references files are valid", {
  citations <- read_dataset_citations(
    file.path(repo_root, bp_dataset_citations_path()),
    file.path(repo_root, bp_dataset_bib_path())
  )
  read_dataset_acknowledgements(
    file.path(repo_root, bp_acknowledgements_path())
  )

  specs <- read_dataset_specs(list_dataset_spec_files(
    file.path(repo_root, bp_dataset_specs_dir())
  ))
  missing <- check_citation_coverage(
    citations,
    specs,
    bp_citation_required_statuses()
  )
  expect(
    length(missing) == 0,
    paste0(
      "Datasets with status ",
      paste(bp_citation_required_statuses(), collapse = "/"),
      " need a citation in Airtable (then run sync_dataset_references()): ",
      paste0("BPIPD-", missing, collapse = ", ")
    )
  )
})
