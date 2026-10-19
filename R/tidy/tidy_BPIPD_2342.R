#' Tidier for BPIPD-2342 (Samson)
#'
#' Single cross-section: the workbook's `DATA` sheet is the whole release, and
#' its `LEGEND` sheet supplies the variable and value labels the data lack.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child
tidy_BPIPD_2342 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  bp2342_apply_legend(df, raw_dataset$codebook$codebook)
}

#' Attach the workbook's `LEGEND` variable and value labels
bp2342_apply_legend <- function(df, path) {
  legend <- readxl::read_excel(
    path,
    sheet = "LEGEND",
    col_names = c("variable", "value", "value_label", "information"),
    col_types = "text",
    skip = 1
  )
  legend <- tidyr::fill(legend, "variable", .direction = "down")

  for (variable in intersect(names(df), legend$variable)) {
    rows <- legend[legend$variable == variable, , drop = FALSE]
    information <- rows$information[!is.na(rows$information)]
    label <- if (length(information) > 0L) information[[1]] else NULL
    coded <- !is.na(rows$value) & !is.na(rows$value_label)

    # The legend's `Subj` values are the leading digits of the id, which encode
    # sex (10890 female, 10896/10897 male), not codes of `Subj` itself.
    if (variable == "Subj" || !any(coded)) {
      attr(df[[variable]], "label") <- label
      next
    }

    df[[variable]] <- haven::labelled(
      as.numeric(df[[variable]]),
      labels = stats::setNames(
        as.numeric(rows$value[coded]),
        rows$value_label[coded]
      ),
      label = label
    )
  }

  df
}
