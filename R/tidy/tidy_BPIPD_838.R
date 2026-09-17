#' Tidier for BPIPD-838 (Lo)
#'
#' A single wide SPSS file holds all five parent-report waves, with the wave in
#' a column suffix (`_T1` ... `_T5`), so the tidier normalises the suffixes and
#' pivots the file to long.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave they took part in
tidy_BPIPD_838 <- function(raw_dataset, spec) {
  # The .sav copy carries the value labels; its .xlsx twin is ignored.
  sav <- raw_dataset$data$`data-sav`
  if (is.null(sav)) {
    stop("BPIPD-838: resource `data-sav` is missing.", call. = FALSE)
  }
  df <- tibble::as_tibble(sav)

  if (anyDuplicated(df$SC) > 0) {
    stop(
      "BPIPD-838: `SC` does not uniquely identify participants.",
      call. = FALSE
    )
  }

  # T1's class and consent items were unsuffixed.
  # Suffix them so they pivot too.
  df <- dplyr::rename_with(
    df,
    ~ paste0(.x, "_T1"),
    dplyr::all_of(c("Class", "Consent1", "Consent2", "Consent3"))
  )

  # `T2`-`T5` are per-wave participation flags (1 = Yes, NA otherwise) stored
  # without a wave suffix. There is no T1 flag because every child has T1 data
  # (1,428 of 1,428 rows carry a measured T1 value), so T1's flag is a constant.
  # Suffixing all five folds them into a single `participated` column.
  df <- df |>
    dplyr::mutate(
      participated_T1 = haven::labelled(
        rep(1, nrow(df)),
        labels = c(Yes = 1),
        label = "Participated in this wave"
      ),
      .before = "T2"
    ) |>
    dplyr::rename_with(
      ~ paste0("participated_", .x),
      dplyr::all_of(c("T2", "T3", "T4", "T5"))
    )

  # Reverse-scored items carry the wave token mid-name (`E7_T1r`)
  df <- dplyr::rename_with(df, ~ sub("^(.*)_T([0-9]+)(r?)$", "\\1\\3_T\\2", .x))

  df <- bp838_unify_wave_value_labels(df)

  # The columns that will survive the pivot, minus the two free-text
  # "other, specify" items, which store "" rather than NA and so would make
  # every participant-wave look observed.
  stems <- unique(sub(
    "_T[0-9]+$",
    "",
    grep("_T[0-9]+$", names(df), value = TRUE)
  ))
  measured <- setdiff(stems, c("participated", "C1_32aa", "C2_14aa"))

  long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::matches("_T[0-9]+$"),
      names_to = c(".value", "wave"),
      names_pattern = "^(.*)_(T[0-9]+)$"
    ) |>
    dplyr::relocate(wave, .after = SC)

  # Every child sits on every wave's roster, so a participant-wave with no
  # measured value at all is a wave the child skipped, not an observation.
  # Keeps 3,756 of 7,140 rows: T1 1,428, T2-T4 300 each, T5 1,428. At T2-T4
  # this reproduces the participation flags exactly; the 395 T5 rows carrying
  # no flag are kept because they still record the child's class and age.
  observed <- rowSums(!is.na(long[measured])) > 0L
  long[observed, , drop = FALSE]
}

#' Give every wave of a variable the same SPSS value labels
bp838_unify_wave_value_labels <- function(df) {
  wave_cols <- grep("_T[0-9]+$", names(df), value = TRUE)
  stems <- sub("_T[0-9]+$", "", wave_cols)

  for (cols in split(wave_cols, stems)) {
    labels <- Filter(Negate(is.null), lapply(df[cols], attr, "labels"))
    unlabelled <- cols[vapply(
      df[cols],
      function(x) is.null(attr(x, "labels")),
      logical(1)
    )]
    if (length(labels) == 0L || length(unlabelled) == 0L) {
      next
    }
    df[unlabelled] <- lapply(df[unlabelled], function(x) {
      out <- haven::labelled(as.vector(x), labels = labels[[1]])
      attr(out, "label") <- attr(x, "label")
      out
    })
  }

  df
}
