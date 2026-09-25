#' Tidier for BPIPD-80 (ISCOLE)
#'
#' One cross-sectional subject-level file. The SAS release only stores a format
#' name per column, so value labels are parsed from the study's `proc format`
#' file and attached here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant
tidy_BPIPD_80 <- function(raw_dataset, spec) {
  # .xlsx copy has the same rows but no labels; unused.
  data <- raw_dataset$data$iscole_allsubjects_data_sas
  if (is.null(data)) {
    stop(
      "BPIPD-80: resource `iscole_allsubjects_data_sas` is missing.",
      call. = FALSE
    )
  }
  data <- tibble::as_tibble(data)

  # `PID` is what the mapping uses as `participant_id`.
  if (!"PID" %in% names(data) || anyDuplicated(data$PID) > 0L) {
    stop(
      "BPIPD-80: `PID` is missing or does not uniquely identify rows.",
      call. = FALSE
    )
  }

  formats_path <- grep(
    "formats_allsites\\.sas$",
    raw_dataset$docs,
    value = TRUE
  )
  if (length(formats_path) != 1L) {
    stop(
      "BPIPD-80: expected exactly one ISCOLE SAS formats file among the dataset docs.",
      call. = FALSE
    )
  }

  bp80_apply_sas_value_labels(data, bp80_read_sas_value_formats(formats_path))
}

#' Parse the numeric `value` statements out of a SAS `proc format` file
bp80_read_sas_value_formats <- function(path) {
  text <- paste(
    iconv(readLines(path, warn = FALSE), "WINDOWS-1252", "UTF-8", sub = ""),
    collapse = "\n"
  )
  # Drop /* ... */ comments (includes one commented-out format block).
  text <- gsub("(?s)/\\*.*?\\*/", " ", text, perl = TRUE)

  quoted <- "'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\""
  header <- "(?is)^\\s*value\\s+(\\$?)([A-Za-z_][A-Za-z0-9_]*)"
  pair <- paste0("(?:-?\\d+)\\s*=\\s*(?:", quoted, ")")

  statements <- strsplit(text, ";", fixed = TRUE)[[1]]
  formats <- list()

  for (statement in statements) {
    name <- regmatches(statement, regexec(header, statement, perl = TRUE))[[1]]
    if (length(name) == 0L || nzchar(name[2])) {
      next
    }

    body <- sub(header, "", statement, perl = TRUE)
    pairs <- regmatches(body, gregexpr(pair, body, perl = TRUE))[[1]]
    if (length(pairs) == 0L) {
      next
    }

    codes <- as.numeric(sub("\\s*=.*$", "", pairs))
    labels <- bp80_unquote_sas(sub("^[^=]*=\\s*", "", pairs))

    keep <- !duplicated(codes)
    formats[[tolower(name[3])]] <- stats::setNames(codes[keep], labels[keep])
  }

  formats
}

#' Strip the outer quotes from a SAS string literal and unescape doubled quotes
bp80_unquote_sas <- function(x) {
  inner <- substr(x, 2L, nchar(x) - 1L)
  ifelse(
    substr(x, 1L, 1L) == "'",
    gsub("''", "'", inner, fixed = TRUE),
    gsub('""', '"', inner, fixed = TRUE)
  )
}

#' Attach parsed SAS value labels to every numeric column that names a format
bp80_apply_sas_value_labels <- function(data, formats) {
  for (column_name in names(data)) {
    column <- data[[column_name]]
    format_name <- attr(column, "format.sas", exact = TRUE)

    if (is.null(format_name) || !is.numeric(column)) {
      next
    }

    labels <- formats[[tolower(sub("\\.$", "", format_name))]]
    if (is.null(labels)) {
      next
    }

    data[[column_name]] <- haven::labelled(
      as.numeric(column),
      labels = labels,
      label = attr(column, "label", exact = TRUE)
    )
  }

  data
}
