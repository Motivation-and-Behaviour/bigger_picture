#' Tidier for BPIPD-736 (Saving and Empowering Young Lives in Europe)
#'
#' One wide Stata file holds all three assessments of the German SEYLE site;
#' wave is carried in the column prefix (`pb`/`pm`/`py`), item number in the
#' rest of the name. Pivoted to one row per pupil per wave.
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

  # pivot_longer() keeps attributes only on haven_labelled copies, so the
  # plain numeric/text items (q90, the scored scales) would arrive unlabelled;
  # save each stem's label here and reapply once the copies are stacked.
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

  # Source labels confirm the prefix letters: `p[bmy]_bdi_cat` is labelled
  # "Depression baseline"/"3-month"/"12-month".
  long <- dplyr::mutate(
    long,
    wave = unname(c(b = "Baseline", m = "3-month", y = "12-month")[wave]),
    .after = "id"
  )

  # Stata strings come back as "" not NA, so free-text items can't tell us
  # whether a pupil answered this wave's questionnaire.
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
#' 12-month for the 30 items the 3-month questionnaire skipped). Copies carry
#' the same codes and differ only in wording ("I don't know" vs "Don't know"),
#' which would just make binding warn; copies whose code sets differ would
#' change meaning, so they stop the tidier. Value labels
#' are taken from the first wave that has any, since one copy can be
#' unlabelled where another isn't (`pb9c` has none, `py9c` labels 77 "Don't
#' know"). The variable label is the baseline copy's with its recall window
#' or wave name trimmed off, so the pivoted column gets one label.
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
