#' Tidier for BPIPD-736 (Saving and Empowering Young Lives in Europe)
#'
#' One wide Stata file holds all three assessments of the German SEYLE site:
#' the wave is carried in the column prefix (`pb`, `pm`, `py`) and the item
#' number in the rest of the name, so the file is pivoted to one row per wave.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per pupil per wave
tidy_BPIPD_736 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data[[1]])

  # `id`, `country`, `age`, `gender` and `schooltype` are baseline-only
  wave_cols <- grep("^p[bmy]", names(df), value = TRUE)

  stems <- paste0("q", sub("^p[bmy]_?", "", wave_cols))
  new_names <- paste0(stems, "_", substr(wave_cols, 2L, 2L))
  names(df)[match(wave_cols, names(df))] <- new_names

  df <- bp736_unify_wave_value_labels(df, new_names)

  # `pivot_longer()` carries attributes only on the `haven_labelled` copies, so
  # the plain numeric and free-text items (`q90`, "Hours per day online", and
  # the scored scales) would arrive unlabelled; keep each stem's label here and
  # put it back once the copies have been stacked.
  item_labels <- vapply(
    df[new_names],
    function(x) {
      label <- attr(x, "label")
      if (is.null(label)) NA_character_ else as.character(label)
    },
    character(1)
  )
  names(item_labels) <- sub("_[bmy]$", "", new_names)
  item_labels <- item_labels[!is.na(item_labels)]
  item_labels <- item_labels[!duplicated(names(item_labels))]

  long <- tidyr::pivot_longer(
    df,
    cols = dplyr::all_of(new_names),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*)_([bmy])$"
  )

  long[names(item_labels)] <- Map(
    function(x, label) {
      attr(x, "label") <- label
      x
    },
    long[names(item_labels)],
    item_labels
  )

  # The source labels name what each prefix letter is: `p[bmy]_bdi_cat` are
  # labelled "Depression baseline", "Depression 3-month", "Depression 12-month".
  long <- dplyr::mutate(
    long,
    wave = unname(c(b = "Baseline", m = "3-month", y = "12-month")[wave]),
    .after = "id"
  )

  # Stata string variables come back as "" rather than NA, so the free-text
  # items cannot tell us whether a pupil answered this wave's questionnaire.
  measured <- setdiff(
    names(long),
    c("id", "wave", "country", "age", "gender", "schooltype")
  )
  measured <- measured[!vapply(long[measured], is.character, logical(1))]
  observed <- rowSums(!is.na(long[measured])) > 0L
  long <- long[observed, ]

  if (anyDuplicated(long[c("id", "wave")]) > 0L) {
    stop(
      "BPIPD-736: `id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  long
}

#' Give every wave's copy of an item one set of labels
#'
#' An item has one copy per wave it was asked at (all three, or baseline and
#' 12-month for the 30 items the 3-month questionnaire skipped). The copies
#' carry the same codes and differ only in wording ("I don't know" against
#' "Don't know"), so binding them would warn without a question to answer;
#' copies whose code sets disagreed would be a real question about meaning,
#' so that stops instead. The value labels come from the first wave that
#' carries any, because a copy can be unlabelled where another is not
#' (`pb9c` has no value labels while `py9c` labels 77 "Don't know"). The
#' variable label is the baseline copy's, with the wave's own
#' recall window or wave name trimmed off ("during past 6 / 3 / 12 months",
#' "baseline", "3-month", "12-month") so the pivoted column gets one label.
bp736_unify_wave_value_labels <- function(df, cols) {
  for (grp in split(cols, sub("_[bmy]$", "", cols))) {
    coded <- grp[
      !vapply(
        df[grp],
        function(x) is.null(attr(x, "labels")),
        logical(1)
      )
    ]
    code_sets <- unique(lapply(
      df[coded],
      function(x) sort(unname(attr(x, "labels")))
    ))
    if (length(code_sets) > 1L) {
      stop(
        "BPIPD-736: wave copies of `",
        sub("_[bmy]$", "", grp[[1L]]),
        "` carry different value codes.",
        call. = FALSE
      )
    }

    labels <- NULL
    if (length(coded) > 0L) {
      labels <- attr(df[[coded[[1L]]]], "labels")
    }

    reference <- grp[endsWith(grp, "_b")]
    reference <- if (length(reference) > 0L) reference[[1L]] else grp[[1L]]
    label <- attr(df[[reference]], "label")
    if (!is.null(label)) {
      label <- sub(
        " (during past [0-9]+ months|baseline|3-month|12-month)$",
        "",
        label
      )
    }

    for (nm in grp) {
      value <- as.vector(df[[nm]])
      if (!is.null(labels) && !is.character(value)) {
        value <- haven::labelled(value, labels = labels)
      }
      attr(value, "label") <- label
      df[[nm]] <- value
    }
  }

  df
}
