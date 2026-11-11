#' Citation keys defined in a `.bib` file
#'
#' Reads entry headers such as `@article{key,`; `@comment`, `@string` and
#' `@preamble` blocks define no citable key and are skipped. An empty file has
#' no keys.
#'
#' @param path Path to the `.bib` file.
#' @return A character vector of keys, in file order.
read_bib_keys <- function(path = bp_dataset_bib_path()) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hits <- regmatches(
    lines,
    regexec("^\\s*@([A-Za-z]+)\\s*\\{\\s*([^,\\s]+)\\s*,", lines, perl = TRUE)
  )
  hits <- hits[lengths(hits) == 3]
  types <- tolower(vapply(hits, `[[`, character(1), 2))
  keys <- vapply(hits, `[[`, character(1), 3)
  keys <- keys[!types %in% c("comment", "string", "preamble")]

  repeated <- unique(keys[duplicated(keys)])
  if (length(repeated) > 0) {
    stop(
      basename(path),
      " defines these keys more than once: ",
      paste(repeated, collapse = ", "),
      call. = FALSE
    )
  }
  keys
}

#' Check the `dataset_id` column of a references file
#'
#' IDs are the repository form, digits only (`107`, not `BPIPD-107`), so they
#' join with `analysis_data$dataset_id`.
#'
#' @param ids The `dataset_id` values.
#' @param label File name for error messages.
#' @return `ids`, invisibly.
check_reference_dataset_ids <- function(ids, label) {
  bad <- is.na(ids) | !grepl("^[0-9]+$", ids)
  if (any(bad)) {
    stop(
      label,
      ": `dataset_id` must be digits only (e.g. 107, not BPIPD-107); found ",
      paste(unique(ifelse(is.na(ids), "(blank)", ids)[bad]), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(ids)
}

#' Describe citation keys with the datasets that use them, for messages
#'
#' @param citations A citations tibble (`dataset_id`, `citekey`).
#' @param keys The keys to describe.
#' @return One string per key, e.g. `"example2020 (BPIPD-9999, BPIPD-9998)"`.
describe_citation_keys <- function(citations, keys) {
  vapply(
    keys,
    function(key) {
      ids <- unique(citations$dataset_id[citations$citekey %in% key])
      paste0(key, " (", paste0("BPIPD-", ids, collapse = ", "), ")")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

#' Read and check `references/dataset_citations.csv`
#'
#' One row per dataset and citation. Every key must be defined in the bib file,
#' so a citation can always be rendered.
#'
#' @param path Path to the citations CSV.
#' @param bib_file Path to the bib file the keys must come from.
#' @return A tibble with `dataset_id` and `citekey`, both character.
read_dataset_citations <- function(
  path = bp_dataset_citations_path(),
  bib_file = bp_dataset_bib_path()
) {
  label <- basename(path)
  citations <- read_registry_csv(path, bp_dataset_citations_columns())
  check_reference_dataset_ids(citations$dataset_id, label)

  if (anyNA(citations$citekey)) {
    stop(
      label,
      ": `citekey` is blank for ",
      paste0(
        "BPIPD-",
        unique(citations$dataset_id[is.na(citations$citekey)]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  pairs <- paste(citations$dataset_id, citations$citekey)
  if (anyDuplicated(pairs) > 0) {
    stop(
      label,
      ": these dataset and citation pairs appear more than once: ",
      paste(unique(pairs[duplicated(pairs)]), collapse = "; "),
      call. = FALSE
    )
  }

  unknown <- setdiff(citations$citekey, read_bib_keys(bib_file))
  if (length(unknown) > 0) {
    stop(
      label,
      ": these citation keys are not in ",
      basename(bib_file),
      ": ",
      paste(describe_citation_keys(citations, unknown), collapse = "; "),
      call. = FALSE
    )
  }

  citations
}

#' Read and check `references/dataset_acknowledgements.csv`
#'
#' One row per dataset whose data providers require an acknowledgement, with
#' the text exactly as it must appear.
#'
#' @param path Path to the acknowledgements CSV.
#' @return A tibble with `dataset_id` and `acknowledgement`, both character.
read_dataset_acknowledgements <- function(
  path = bp_acknowledgements_path()
) {
  label <- basename(path)
  acknowledgements <- read_registry_csv(
    path,
    bp_acknowledgements_columns()
  )
  check_reference_dataset_ids(acknowledgements$dataset_id, label)

  if (anyNA(acknowledgements$acknowledgement)) {
    stop(
      label,
      ": `acknowledgement` is blank for ",
      paste0(
        "BPIPD-",
        acknowledgements$dataset_id[is.na(acknowledgements$acknowledgement)],
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  repeated <- unique(acknowledgements$dataset_id[duplicated(
    acknowledgements$dataset_id
  )])
  if (length(repeated) > 0) {
    stop(
      label,
      ": more than one row for ",
      paste0("BPIPD-", repeated, collapse = ", "),
      call. = FALSE
    )
  }

  acknowledgements
}

#' Repository dataset IDs with one of the given spec statuses
#'
#' @param specs Dataset specs, as from `read_dataset_specs()`.
#' @param statuses Spec statuses to keep.
#' @return Dataset IDs (digits only), in numeric order.
spec_dataset_ids <- function(specs, statuses) {
  ids <- vapply(
    specs,
    function(s) as.character(s$dataset_id %||% NA_character_),
    character(1),
    USE.NAMES = FALSE
  )
  status <- vapply(
    specs,
    function(s) as.character(s$status %||% NA_character_),
    character(1),
    USE.NAMES = FALSE
  )
  ids <- unique(ids[status %in% statuses])
  ids[order(as.integer(ids))]
}

#' Repository datasets that still need a citation
#'
#' @param citations A citations tibble (`dataset_id`, `citekey`).
#' @param specs Dataset specs, as from `read_dataset_specs()`.
#' @param statuses Spec statuses that need a citation.
#' @return Dataset IDs (digits only) with one of `statuses` and no citation,
#'   in numeric order.
check_citation_coverage <- function(
  citations,
  specs = read_dataset_specs(),
  statuses = bp_citation_required_statuses()
) {
  setdiff(spec_dataset_ids(specs, statuses), citations$dataset_id)
}
