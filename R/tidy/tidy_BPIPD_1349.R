#' Tidier for BPIPD-1349 (Viros-Martin)
#'
#' One cross-sectional CSV with no labels and no participant id, so variable
#' and value labels are attached from the CSV codebook.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent (single occasion), in release order
tidy_BPIPD_1349 <- function(raw_dataset, spec) {
  # RESPID was removed before release and the original record order is kept (README), so row position is the only key.
  df <- tibble::as_tibble(raw_dataset$data$data)
  bp1349_apply_labels(df, raw_dataset$codebook$codebook)
}

#' Attach the codebook's headings, questionnaire wording and native code labels
bp1349_apply_labels <- function(df, path) {
  codebook <- readr::read_csv(path, col_types = readr::cols(.default = "c"))
  for (i in seq_len(nrow(codebook))) {
    entry <- codebook[i, ]
    codes <- bp1349_parse_codes(entry$native_value_labels)
    if (length(codes) > 0L) {
      df[[entry$variable]] <- haven::labelled(
        df[[entry$variable]],
        labels = codes
      )
    }
    label <- entry$item_label_ES
    # Technical and derived columns have no questionnaire wording to add.
    if (grepl("^(paragraph|table)", entry$questionnaire_location)) {
      wording <- gsub("\\s+", " ", trimws(entry$questionnaire_wording_ES))
      label <- paste0(label, " [", wording, "]")
    }
    attr(df[[entry$variable]], "label") <- label
  }
  df
}

#' Parse codes written as "1=Hombre; 2=Mujer" into named values
bp1349_parse_codes <- function(text) {
  pairs <- regmatches(text, gregexpr("-?[0-9]+=[^;]+", text))[[1]]
  stats::setNames(
    as.numeric(sub("=.*$", "", pairs)),
    trimws(sub("^-?[0-9]+=", "", pairs))
  )
}
