#' Tidier for BPIPD-1760 (Swift)
#'
#' One Excel sheet with three header rows (block, variable code, wording): the
#' codes become the column names and the block plus wording the labels. Three
#' children have a father's and a mother's row; only the mother's is kept.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child, keyed on `child_code`, with `family_id` and
#'   `Q3_date` (the parent's birth date in `Q3` as a date) added
tidy_BPIPD_1760 <- function(raw_dataset, spec) {
  sheet <- raw_dataset$data[[1]]

  # Sheet row 1 became the column names; rows 2-3 hold the code and wording.
  block <- names(sheet)
  block[grepl("^\\.\\.\\.[0-9]+$", block)] <- NA_character_
  block <- bp1760_carry_forward(block)
  code <- as.character(unlist(sheet[1, ], use.names = FALSE))
  wording <- as.character(unlist(sheet[2, ], use.names = FALSE))

  # The codebook puts these two child-administered NIH Toolbox tests under
  # executive function, not under the carried-over block header.
  block[match(names(BP1760_EF_TESTS), code)] <- BP1760_EF_TESTS

  # Three trailing columns have no code: two are empty, one holds a stray note.
  keep <- !is.na(code)
  label <- bp1760_labels(block[keep], wording[keep], code[keep])

  out <- tibble::as_tibble(sheet[-(1:2), keep])
  names(out) <- sub("^Child Code$", "child_code", code[keep])
  out[] <- Map(bp1760_column, out, label)

  # One row per child: the mother's row of each two-parent pair stays.
  out <- out[!out$child_code %in% BP1760_FATHER_ROWS, ]
  child_label <- attr(out$child_code, "label")
  out$child_code <- sub("_M$", "", out$child_code)
  attr(out$child_code, "label") <- child_label

  if (anyDuplicated(out$child_code) > 0) {
    stop("BPIPD-1760: `child_code` is not unique.", call. = FALSE)
  }

  out$Q3_date <- bp1760_birth_date(out$Q3)
  attr(out$Q3_date, "label") <- "Parent's date of birth, Q3 as a date"
  out$family_id <- bp1760_family_id(out$child_code, out$Q3_date, out$Q73)
  attr(out$family_id, "label") <- paste(
    "Family: first child sharing the parent's birth date (Q3) and",
    "kindergarten decile (Q73), else the child's own code"
  )

  out
}

BP1760_FATHER_ROWS <- c("C38_F", "C402_F", "C408_F")

BP1760_EF_TESTS <- c(
  EF_DIM = "Executive function (NIH Toolbox) | Dimensional Change Card Sort",
  EF_FLA = "Executive function (NIH Toolbox) | Flanker Inhibitory Control and Attention"
)

#' Carry each merged block header across the columns it spans
bp1760_carry_forward <- function(x) {
  for (i in seq_along(x)[-1]) {
    if (is.na(x[i])) {
      x[i] <- x[i - 1L]
    }
  }
  x
}

#' Join the block header and the question wording, falling back to the code
bp1760_labels <- function(block, wording, code) {
  parts <- Map(c, block, wording)
  label <- vapply(
    parts,
    function(x) paste(x[!is.na(x)], collapse = " | "),
    character(1),
    USE.NAMES = FALSE
  )
  ifelse(nzchar(label), label, code)
}

#' Restore a column's numbers, lost to the text header rows, and label it
bp1760_column <- function(x, label) {
  numeric_x <- suppressWarnings(as.numeric(x))
  if (!any(is.na(numeric_x) & !is.na(x))) {
    x <- numeric_x
  }
  attr(x, "label") <- label
  x
}

#' Link children whose parent has one birth date within one kindergarten
#'
#' The id is a child code, so the birth date decides the link and stays out of it.
bp1760_family_id <- function(child_code, birth_date, decile) {
  key <- paste(birth_date, decile)
  key[is.na(birth_date)] <- paste0("own_", child_code[is.na(birth_date)])
  child_code[match(key, key)]
}

#' Read the free-text birth dates: Excel serials and day-first dates in any
#' separator, with or without a month name; anything else is NA
bp1760_birth_date <- function(x) {
  out <- vapply(x, bp1760_parse_date, numeric(1), USE.NAMES = FALSE)
  as.Date(out, origin = "1970-01-01")
}

bp1760_parse_date <- function(x) {
  if (is.na(x)) {
    return(NA_real_)
  }
  if (grepl("^[0-9]{5}$", x) && as.numeric(x) < 50000) {
    return(as.numeric(as.Date(as.numeric(x), origin = "1899-12-30")))
  }

  text <- tolower(x)
  month <- which(vapply(tolower(month.abb), grepl, logical(1), x = text))
  numbers <- regmatches(text, gregexpr("[0-9]+", text))[[1]]
  if (length(month) == 1) {
    numbers <- c(numbers[1], month, numbers[2])
  } else if (length(numbers) == 2) {
    # Typed without one separator: day, month and four-digit year run together.
    joined <- paste0(numbers, collapse = "")
    n <- nchar(joined)
    numbers <- c(
      substr(joined, 1, 2),
      substr(joined, 3, n - 4),
      substr(joined, n - 3, n)
    )
  }
  if (length(numbers) != 3) {
    return(NA_real_)
  }

  year <- as.numeric(numbers[3])
  year <- if (year < 30) {
    year + 2000
  } else if (year < 100) {
    year + 1900
  } else {
    year
  }
  date <- sprintf(
    "%04d-%02d-%02d",
    year,
    as.numeric(numbers[2]),
    as.numeric(numbers[1])
  )
  as.numeric(as.Date(date, optional = TRUE))
}
