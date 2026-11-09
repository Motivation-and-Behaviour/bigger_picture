#' Tidier for BPIPD-1417 (Ciacchini)
#'
#' One Excel sheet in which every coded item is followed by an unnamed column
#' holding its response labels; those become value labels on the codes.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per answer set (single occasion, no participant id)
tidy_BPIPD_1417 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  # The label columns have no header, so the reader names them `...<n>`.
  label_cols <- grep("^\\.\\.\\.[0-9]+$", names(df))
  code_cols <- label_cols - 1L
  df[code_cols] <- Map(
    bp1417_label_codes,
    df[code_cols],
    df[label_cols],
    names(df)[code_cols]
  )

  df <- df[-label_cols]

  # 32 answer sets recur on several rows differing at most on BSMAS (and its
  # copy); each set is kept once, as its first row.
  bsmas_cols <- c("BSMAS", "BSMAS cut-off underlined")
  df[!duplicated(df[setdiff(names(df), bsmas_cols)]), ]
}

#' Attach a coded item's value labels from the label column beside it
bp1417_label_codes <- function(code, label, item) {
  # A `SM checking` label for code 2 has the code-3 label pasted into it; the
  # published questionnaire (Appendix A) reads "Quite often (every 30 minutes)".
  garbled <- "Quite often (every Very often (every 10 minutes)0 minutes)"
  label[label == garbled] <- "Quite often (every 30 minutes)"

  pairs <- unique(data.frame(code = code, label = label))
  pairs <- pairs[order(pairs$code), ]
  if (anyDuplicated(pairs$code) > 0) {
    stop("BPIPD-1417: `", item, "` has two labels for one code.", call. = FALSE)
  }

  haven::labelled(code, labels = stats::setNames(pairs$code, pairs$label))
}
