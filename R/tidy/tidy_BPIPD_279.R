#' Tidier for BPIPD-279 (National Survey of Early Childhood Health (NSECH))
#'
#' One cross-sectional file holding 1,939 of the 2,068 documented children; it
#' has no value labels, so they are parsed from the NCHS formats program.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per sampled child
tidy_BPIPD_279 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  # One child was sampled per household and `PSU` is the household case id (Methodology report).
  if (anyDuplicated(df$PSU) > 0) {
    stop("BPIPD-279: `PSU` does not uniquely identify children.", call. = FALSE)
  }

  statements <- bp279_sas_statements(raw_dataset$codebook$sas_formats)
  formats <- bp279_read_value_formats(statements)
  assigned <- bp279_read_format_assignments(statements, names(df))

  # The formats program swaps codes 2 and 3 of `A4Q02Q`; the questionnaire, the frequency tables and the published weighted shares (Kuo et al. 2004) give 2 = 3-6 days.
  names(formats$A4Q02Q)[match(c(2, 3), formats$A4Q02Q)] <- c(
    "2-THREE TO SIX DAYS",
    "3-ONE TO TWO DAYS"
  )

  df[names(assigned)] <- Map(
    function(x, format) {
      haven::labelled(
        x,
        labels = formats[[format]],
        label = attr(x, "label", exact = TRUE)
      )
    },
    df[names(assigned)],
    assigned
  )

  df
}

#' Split a SAS program into statements, without its block comments
bp279_sas_statements <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  text <- gsub("(?s)/\\*.*?\\*/", " ", text, perl = TRUE)
  strsplit(text, ";", fixed = TRUE)[[1]]
}

#' Parse the numeric `VALUE` statements into named code vectors
bp279_read_value_formats <- function(statements) {
  header <- "(?is)^\\s*value\\s+([A-Za-z_][A-Za-z0-9_]*)"
  pair <- "(-?[0-9]+)\\s*=\\s*('[^']*'|\"[^\"]*\")"
  formats <- list()

  for (statement in grep(header, statements, value = TRUE, perl = TRUE)) {
    name <- toupper(sub(paste0(header, ".*$"), "\\1", statement, perl = TRUE))
    pairs <- regmatches(statement, gregexpr(pair, statement, perl = TRUE))[[1]]
    labels <- sub("^[^=]*=\\s*", "", pairs)
    labels <- trimws(substr(labels, 2L, nchar(labels) - 1L))
    # A repeated name replaces the earlier format, as in SAS (`AGEFOOD` is defined twice, identically).
    formats[[name]] <- stats::setNames(
      as.numeric(sub("\\s*=.*$", "", pairs)),
      labels
    )
  }

  formats
}

#' Parse the `FORMAT` statement into a format name per column
bp279_read_format_assignments <- function(statements, columns) {
  statement <- grep("(?i)^\\s*format\\b", statements, value = TRUE, perl = TRUE)
  tokens <- strsplit(
    trimws(sub("(?i)^\\s*format", "", statement, perl = TRUE)),
    "\\s+"
  )[[1]]
  is_format <- endsWith(tokens, ".")
  format_names <- toupper(sub("\\.$", "", tokens[is_format]))

  lists <- lapply(tokens[!is_format], bp279_expand_sas_list, columns = columns)
  assigned <- stats::setNames(
    rep(format_names[cumsum(is_format)[!is_format] + 1L], lengths(lists)),
    unlist(lists)
  )

  # `A7Q01R` is not in the public file, and the repeat of `A4Q06` as `NUM2D.` contradicts the frequency tables, which print `A4Q06Q.`.
  assigned <- assigned[names(assigned) != "A7Q01R"]
  assigned[!duplicated(names(assigned))]
}

#' Expand a SAS variable list (`x1-x4` numbered, `a--b` by position) to names
bp279_expand_sas_list <- function(token, columns) {
  if (grepl("--", token, fixed = TRUE)) {
    ends <- match(strsplit(token, "--", fixed = TRUE)[[1]], columns)
    columns[ends[1]:ends[2]]
  } else if (grepl("-", token, fixed = TRUE)) {
    ends <- strsplit(token, "-", fixed = TRUE)[[1]]
    digits <- sub("^.*?([0-9]+)$", "\\1", ends)
    numbers <- seq(as.integer(digits[1]), as.integer(digits[2]))
    paste0(
      sub("[0-9]+$", "", ends[1]),
      formatC(numbers, width = nchar(digits[1]), flag = "0")
    )
  } else {
    token
  }
}
