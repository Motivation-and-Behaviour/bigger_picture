#' Tidier for BPIPD-736
#'
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble
tidy_BPIPD_736 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data[[1]])

  # `id`, `country`, `age`, `gender` and `schooltype` are baseline-only
  wave_cols <- grep("^p[bmy]", names(df), value = TRUE)

  stems <- paste0("q", sub("^p[bmy]_?", "", wave_cols))
  new_names <- paste0(stems, "_", substr(wave_cols, 2L, 2L))
  names(df)[match(wave_cols, names(df))] <- new_names

  df <- unify_wave_value_labels_736(df, new_names)

  long <- tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(new_names),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*)_([bmy])$"
  )

  long <- dplyr::mutate(
    long,
    wave = unname(c(b = "Baseline", m = "3-month", y = "12-month")[wave]),
    .after = "id"
  )

  measured <- setdiff(
    names(long),
    c("id", "wave", "country", "age", "gender", "schooltype")
  )
  measured <- measured[!vapply(long[measured], is.character, logical(1))]
  observed <- rowSums(!is.na(long[measured])) > 0L

  long[observed, ]
}

unify_wave_value_labels_736 <- function(df, cols) {
  for (grp in split(cols, sub("_[bmy]$", "", cols))) {
    reference <- grp[endsWith(grp, "_b")]
    reference <- if (length(reference) > 0L) reference[[1L]] else grp[[1L]]

    labels <- attr(df[[reference]], "labels")
    label <- attr(df[[reference]], "label")
    if (!is.null(label)) {
      label <- sub(" during past [0-9]+ months$", "", label)
    }

    for (nm in grp) {
      if (is.character(df[[nm]])) {
        next
      }
      value <- as.vector(df[[nm]])
      value <- if (is.null(labels)) {
        value
      } else {
        haven::labelled(value, labels = labels)
      }
      attr(value, "label") <- label
      df[[nm]] <- value
    }
  }

  df
}
