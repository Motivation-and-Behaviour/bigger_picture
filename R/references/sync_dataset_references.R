# Refresh references/ from Airtable and the Zotero export.
#
# Run by hand, never by a target, a test or CI:
#   Rscript -e 'targets::tar_source(); sync_dataset_references()'
# It needs AIRTABLE_TOKEN and AIRTABLE_BASE_ID in .env, plus BP_BIB_SOURCE when
# the Zotero export is written outside the checkout. Only the fields in
# bp_airtable_reference_fields() are requested.

#' Build one page request to Airtable's list-records endpoint
#'
#' @param table Table name.
#' @param fields Field names to return; nothing else is fetched.
#' @param base_id Airtable base ID.
#' @param token Airtable personal access token.
#' @param filter Optional `filterByFormula` formula.
#' @param offset Paging offset from the previous page, or `NULL`.
#' @return An `httr2` request, not yet performed.
airtable_list_request <- function(
  table,
  fields,
  base_id,
  token,
  filter = NULL,
  offset = NULL
) {
  req <- httr2::request(bp_airtable_api_url())
  req <- httr2::req_url_path_append(req, base_id, table)
  req <- httr2::req_url_query(req, `fields[]` = fields, .multi = "explode")
  req <- httr2::req_url_query(
    req,
    filterByFormula = filter,
    pageSize = 100,
    offset = offset
  )
  # Marks the header as redacted, so a printed request never shows the token.
  req <- httr2::req_auth_bearer_token(req, token)
  # Airtable allows five requests a second per base; 429 and 503 are retried.
  req <- httr2::req_retry(req, max_tries = 5)
  httr2::req_error(req, body = airtable_error_body)
}

#' Airtable's own explanation of a failed request, for the error message
#'
#' @param resp A failed `httr2` response.
#' @return A character vector of message lines.
airtable_error_body <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  error <- body$error
  if (is.list(error)) {
    return(paste0(error$type %||% "", ": ", error$message %||% ""))
  }
  if (is.character(error)) {
    return(error)
  }
  character()
}

#' Fetch every record of an Airtable table, following pagination
#'
#' The only function in the project that calls Airtable.
#'
#' @inheritParams airtable_list_request
#' @return A list of records, each `list(id = "rec...", fields = list(...))`.
#'   Airtable leaves empty fields out of `fields`.
fetch_airtable_records <- function(
  table,
  fields,
  base_id,
  token,
  filter = NULL
) {
  records <- list()
  offset <- NULL
  repeat {
    resp <- httr2::req_perform(airtable_list_request(
      table,
      fields,
      base_id,
      token,
      filter = filter,
      offset = offset
    ))
    body <- httr2::resp_body_json(resp)
    records <- c(records, body$records)
    offset <- body$offset
    if (is.null(offset)) {
      return(records)
    }
  }
}

#' Text value of an Airtable record field, `NA` when the field is empty
#'
#' @param record One record from `fetch_airtable_records()`.
#' @param field Field name.
#' @return A single string or `NA_character_`.
airtable_text_field <- function(record, field) {
  value <- record$fields[[field]]
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }
  as.character(value[[1]])
}

#' Turn Airtable records into the two references tables
#'
#' Pure: no network, no files. Errors name the offending datasets so the
#' record can be fixed in Airtable.
#'
#' @param dataset_records Datasets records with `Dataset ID`, `Citations` and
#'   `Required Acknowledgements`.
#' @param citation_records Citations records with `Citation Key`.
#' @return `list(citations, acknowledgements)`: tibbles with the columns of
#'   `bp_dataset_citations_columns()` and
#'   `bp_acknowledgements_columns()`, all character, sorted by numeric
#'   dataset ID (then key).
build_dataset_references <- function(dataset_records, citation_records) {
  raw_ids <- trimws(vapply(
    dataset_records,
    airtable_text_field,
    character(1),
    field = "Dataset ID"
  ))
  dataset_ids <- sub("^BPIPD-", "", raw_ids)
  bad <- is.na(dataset_ids) | !grepl("^[0-9]+$", dataset_ids)
  if (any(bad)) {
    stop(
      "Airtable Datasets records whose `Dataset ID` is not BPIPD-<number>: ",
      paste(ifelse(is.na(raw_ids), "(blank)", raw_ids)[bad], collapse = ", "),
      call. = FALSE
    )
  }
  repeated <- unique(dataset_ids[duplicated(dataset_ids)])
  if (length(repeated) > 0) {
    stop(
      "Airtable Datasets records share a `Dataset ID`: ",
      paste0("BPIPD-", repeated, collapse = ", "),
      call. = FALSE
    )
  }

  record_ids <- vapply(citation_records, `[[`, character(1), "id")
  keys <- trimws(vapply(
    citation_records,
    airtable_text_field,
    character(1),
    field = "Citation Key"
  ))
  keys[is.na(keys)] <- ""
  shared <- unique(keys[nzchar(keys) & duplicated(keys)])
  if (length(shared) > 0) {
    stop(
      "Airtable Citations records share a `Citation Key`: ",
      paste(shared, collapse = ", "),
      call. = FALSE
    )
  }

  links <- lapply(dataset_records, function(record) {
    as.character(unlist(record$fields[["Citations"]]))
  })
  linked <- unlist(links)
  link_ids <- rep(dataset_ids, lengths(links))
  linked_keys <- unname(keys[match(linked, record_ids)])
  blank <- is.na(linked_keys) | !nzchar(linked_keys)
  if (any(blank)) {
    stop(
      "These datasets link to an Airtable Citations record with no ",
      "`Citation Key`: ",
      paste0("BPIPD-", unique(link_ids[blank]), collapse = ", "),
      call. = FALSE
    )
  }
  citations <- tibble::tibble(
    dataset_id = as.character(link_ids),
    citekey = as.character(linked_keys)
  )
  citations <- citations[!duplicated(citations), ]
  citations <- citations[
    order(
      as.integer(citations$dataset_id),
      citations$citekey,
      method = "radix"
    ),
  ]

  text <- vapply(
    dataset_records,
    airtable_text_field,
    character(1),
    field = "Required Acknowledgements"
  )
  text <- trimws(gsub("\r\n?", "\n", text))
  keep <- !is.na(text) & nzchar(text)
  acknowledgements <- tibble::tibble(
    dataset_id = dataset_ids[keep],
    acknowledgement = text[keep]
  )
  acknowledgements <- acknowledgements[
    order(as.integer(acknowledgements$dataset_id)),
  ]

  list(citations = citations, acknowledgements = acknowledgements)
}

#' Write text to a file only when it differs from what is there
#'
#' @param text File contents.
#' @param path Destination.
#' @return `TRUE` when the file was written.
write_if_changed <- function(text, path) {
  if (file.exists(path) && identical(readr::read_file(path), text)) {
    return(FALSE)
  }
  fs::dir_create(fs::path_dir(path))
  readr::write_file(text, path)
  TRUE
}

#' Copy a file only when the destination differs from it
#'
#' @param from Source file.
#' @param to Destination.
#' @return `TRUE` when the file was copied.
copy_if_changed <- function(from, to) {
  same <- file.exists(to) &&
    identical(
      readBin(from, "raw", file.size(from)),
      readBin(to, "raw", file.size(to))
    )
  if (same) {
    return(FALSE)
  }
  fs::dir_create(fs::path_dir(to))
  fs::file_copy(from, to, overwrite = TRUE)
  TRUE
}

#' Refresh the dataset citations and acknowledgements from Airtable
#'
#' Fetches only the fields in `bp_airtable_reference_fields()` for datasets
#' whose Airtable status is `Included`, checks every citation key against the
#' Zotero export, and only then writes `references/`. If a key is missing from
#' the bib nothing is written, so the committed files always agree.
#'
#' @param base_id Airtable base ID; defaults to `AIRTABLE_BASE_ID`.
#' @param token Airtable token; defaults to `AIRTABLE_TOKEN`.
#' @param bib_source The Better BibTeX export to copy into
#'   `references/datasets.bib`; defaults to `BP_BIB_SOURCE`. Blank to use the
#'   committed bib as it is (e.g. when Zotero exports straight into it).
#' @param write `FALSE` to check and summarise without writing.
#' @param specs Dataset specs for the summary; defaults to the repository's.
#' @param fetch The fetching function; tests pass a fake.
#' @return A summary list, invisibly: counts, repository datasets that are not
#'   `Included` in Airtable, repository datasets without a citation, the files
#'   written, and whether this was a dry run.
sync_dataset_references <- function(
  base_id = NULL,
  token = NULL,
  bib_source = NULL,
  write = TRUE,
  specs = NULL,
  fetch = fetch_airtable_records
) {
  if (
    (is.null(base_id) || is.null(token) || is.null(bib_source)) &&
      file.exists(".env")
  ) {
    dotenv::load_dot_env()
  }
  base_id <- base_id %||% Sys.getenv("AIRTABLE_BASE_ID")
  token <- token %||% Sys.getenv("AIRTABLE_TOKEN")
  bib_source <- bib_source %||% Sys.getenv("BP_BIB_SOURCE")
  if (!nzchar(base_id) || !nzchar(token)) {
    stop(
      "Add AIRTABLE_BASE_ID and AIRTABLE_TOKEN to .env before syncing.",
      call. = FALSE
    )
  }

  bib_target <- bp_dataset_bib_path()
  bib_file <- bib_target
  if (nzchar(bib_source)) {
    if (!file.exists(bib_source)) {
      stop("BP_BIB_SOURCE does not exist: ", bib_source, call. = FALSE)
    }
    bib_file <- bib_source
  }
  if (!file.exists(bib_file)) {
    stop(
      bib_file,
      " does not exist; export the Zotero collection there first.",
      call. = FALSE
    )
  }

  airtable <- bp_airtable_reference_fields()
  dataset_records <- fetch(
    airtable$datasets_table,
    airtable$dataset_fields,
    base_id,
    token,
    filter = airtable$included_filter
  )
  citation_records <- fetch(
    airtable$citations_table,
    airtable$citation_fields,
    base_id,
    token
  )
  references <- build_dataset_references(dataset_records, citation_records)
  citations <- references$citations

  unknown <- setdiff(citations$citekey, read_bib_keys(bib_file))
  if (length(unknown) > 0) {
    stop(
      "Nothing was written: these Airtable citation keys are not in ",
      bib_file,
      ". Add the items to the Zotero collection or correct the key in ",
      "Airtable:\n  ",
      paste(describe_citation_keys(citations, unknown), collapse = "\n  "),
      call. = FALSE
    )
  }

  written <- character()
  if (write) {
    copy_bib <- fs::path_abs(bib_file) != fs::path_abs(bib_target)
    if (copy_bib && copy_if_changed(bib_file, bib_target)) {
      written <- c(written, bib_target)
    }
    outputs <- list(
      list(citations, bp_dataset_citations_path()),
      list(references$acknowledgements, bp_acknowledgements_path())
    )
    for (output in outputs) {
      text <- readr::format_csv(output[[1]], na = "")
      if (write_if_changed(text, output[[2]])) {
        written <- c(written, output[[2]])
      }
    }
  }

  if (is.null(specs)) {
    specs <- if (dir.exists(bp_dataset_specs_dir())) {
      read_dataset_specs()
    } else {
      list()
    }
  }
  included_ids <- sub(
    "^BPIPD-",
    "",
    trimws(vapply(
      dataset_records,
      airtable_text_field,
      character(1),
      field = "Dataset ID"
    ))
  )
  pipeline_statuses <- c("in_progress", "harmonised")
  summary <- list(
    included = length(included_ids),
    with_citations = length(unique(citations$dataset_id)),
    with_acknowledgements = nrow(references$acknowledgements),
    not_included = setdiff(
      spec_dataset_ids(specs, pipeline_statuses),
      included_ids
    ),
    missing_citations = check_citation_coverage(
      citations,
      specs,
      pipeline_statuses
    ),
    written = as.character(written),
    dry_run = !write
  )
  report_reference_sync(summary)
  invisible(summary)
}

#' Print a sync summary: counts and dataset IDs only
#'
#' @param summary The list `sync_dataset_references()` returns.
#' @return `summary`, invisibly.
report_reference_sync <- function(summary) {
  ids <- function(x) {
    if (length(x) == 0) {
      return("none")
    }
    paste(
      strwrap(paste0("BPIPD-", x, collapse = ", "), prefix = "\n    "),
      collapse = ""
    )
  }
  message(
    "Airtable: ",
    summary$included,
    " Included datasets, ",
    summary$with_citations,
    " with citations, ",
    summary$with_acknowledgements,
    " with acknowledgements.\n",
    "Pipeline datasets without a citation (",
    length(summary$missing_citations),
    "): ",
    ids(summary$missing_citations),
    "\n",
    "Pipeline datasets not Included in Airtable: ",
    ids(summary$not_included),
    "\n",
    "Files written: ",
    if (isTRUE(summary$dry_run)) {
      "none (write = FALSE)"
    } else if (length(summary$written) == 0) {
      "none (already up to date)"
    } else {
      paste(summary$written, collapse = ", ")
    }
  )
  invisible(summary)
}
