#' Tidier for BPIPD-1440 (Coksan)
#'
#' One cross-sectional CSV exported from jamovi without labels, so variable
#' and value labels are attached from the Excel codebook.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant (single occasion)
tidy_BPIPD_1440 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)
  bp1440_apply_labels(df, raw_dataset$codebook$codebook)
}

#' Attach variable and value labels from the codebook's `Codebook` sheet
bp1440_apply_labels <- function(df, path) {
  codebook <- readxl::read_excel(path, sheet = "Codebook")
  for (i in seq_len(nrow(codebook))) {
    column <- codebook$`Variable Name`[[i]]
    codes <- bp1440_parse_codes(codebook$`Coding / Response Options`[[i]])
    if (length(codes) > 0L) {
      df[[column]] <- haven::labelled(df[[column]], labels = codes)
    }
    attr(df[[column]], "label") <- codebook$`Description / Item Wording`[[i]]
  }
  df
}

#' Parse codes written as "1 = Almost never; 2 = Rarely" into named values
bp1440_parse_codes <- function(text) {
  pairs <- regmatches(text, gregexpr("[0-9]+ = [^;]+", text))[[1]]
  stats::setNames(
    as.numeric(sub(" = .*$", "", pairs)),
    trimws(sub("^[0-9]+ = ", "", pairs))
  )
}
