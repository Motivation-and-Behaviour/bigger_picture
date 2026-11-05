#' Tidier for BPIPD-86 (Cogni-Action Project)
#'
#' One workbook: the `Database` sheet is the data, and the `Codes` sheet
#' supplies the variable and value labels the Excel reader cannot carry.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child at a single measurement occasion
tidy_BPIPD_86 <- function(raw_dataset, spec) {
  codes <- readxl::read_excel(
    raw_dataset$codebook$codebook_codes_sheet,
    sheet = "Codes"
  )

  bp86_apply_codebook(tibble::as_tibble(raw_dataset$data$data), codes)
}

#' Attach the `Codes` sheet's description and value labels to each column
bp86_apply_codebook <- function(tbl, codes) {
  # `Label_<k>` holds the label for code `k`, over `First_Level`-`Last_Level`.
  label_columns <- grep("^Label_[0-9]+$", names(codes), value = TRUE)
  values <- as.numeric(sub("^Label_", "", label_columns))

  for (column in intersect(names(tbl), codes$Variable_name)) {
    row <- match(column, codes$Variable_name)
    labels <- as.character(unlist(codes[row, label_columns]))
    labelled <- !is.na(labels)

    if (any(labelled)) {
      tbl[[column]] <- haven::labelled(
        tbl[[column]],
        labels = stats::setNames(values[labelled], labels[labelled])
      )
    }

    attr(tbl[[column]], "label") <- as.character(codes$Description[[row]])
  }

  tbl
}
