#' Tidier for BPIPD-41 (ELEVA)
#'
#' The extract is a single sheet with the 2019 and 2022 rounds already stacked
#' (`survey`) and carries no participant identifier, so the rounds cannot be
#' linked and no id is constructed here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent per survey round (2019 or 2022) who
#'   answered at least one item
tidy_BPIPD_41 <- function(raw_dataset, spec) {
  data <- raw_dataset$data$data
  if (is.null(data)) {
    stop("BPIPD-41: resource `data` is missing.", call. = FALSE)
  }
  df <- tibble::as_tibble(data)

  # 134 rows (115 in 2019, 19 in 2022) carry `survey` and nothing else.
  items <- setdiff(names(df), "survey")
  observed <- rowSums(!is.na(df[items])) > 0L
  df <- df[observed, , drop = FALSE]

  bp41_apply_labels(df, raw_dataset$codebook$codebook)
}

#' Attach variable labels from the workbook's `dictionary` sheet
bp41_apply_labels <- function(df, path) {
  if (is.null(path)) {
    stop("BPIPD-41: codebook resource `codebook` is missing.", call. = FALSE)
  }
  dictionary <- readxl::read_excel(path, sheet = "dictionary")
  labels <- stats::setNames(
    as.character(dictionary$description),
    as.character(dictionary$variable)
  )
  for (nm in intersect(names(df), names(labels))) {
    attr(df[[nm]], "label") <- unname(labels[[nm]])
  }
  df
}
