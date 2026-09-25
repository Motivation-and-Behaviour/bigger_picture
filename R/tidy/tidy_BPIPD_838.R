#' Tidier for BPIPD-838 (Lo)
#'
#' One wide SPSS file holds all five parent-report waves, wave in a column
#' suffix (`_T1`-`_T5`); this normalises the suffixes, pivots to long and
#' carries the T1-only background items to the later waves.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave they took part in
tidy_BPIPD_838 <- function(raw_dataset, spec) {
  # .sav carries value labels; .xlsx twin is ignored.
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

  # T1's class/consent items are unsuffixed; suffix them so they pivot too.
  df <- dplyr::rename_with(
    df,
    ~ paste0(.x, "_T1"),
    dplyr::all_of(c("Class", "Consent1", "Consent2", "Consent3"))
  )

  # `T2`-`T5` are unsuffixed per-wave participation flags (1 = Yes, NA
  # otherwise); there's no T1 flag since every child has T1 data, so T1's
  # flag is a constant. Suffixing all five gives one `participated` column.
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

  long <- df |>
    tidyr::pivot_longer(
      cols = dplyr::matches("_T[0-9]+$"),
      names_to = c(".value", "wave"),
      names_pattern = "^(.*)_(T[0-9]+)$"
    ) |>
    dplyr::relocate(wave, .after = SC)

  # A row is kept only where the child actually took part in that wave.
  # `participated` is the authoritative record (T2-T5; T1's is a constant,
  # since every child has T1 data). T5 carries study-derived class and age
  # for children who did not take part, so filtering on any measured value
  # being present (rather than on `participated` itself) would keep those
  # as if they were observations.
  long <- long[!is.na(long$participated), , drop = FALSE]

  # Carry T1-only items forward: parent education always; income and partner
  # status as T5 is 12 months on. The study's T5 age is T1 + 1, so age fills T2-T4.
  bp838_carry_from_t1(
    long,
    c("Edu_mother", "Edu_father", "Household_income_gp", "S1", "Age_child")
  )
}

#' Fill each participant's missing values in `cols` with their T1 value
bp838_carry_from_t1 <- function(long, cols) {
  t1 <- long[long$wave == "T1", , drop = FALSE]
  t1_row <- match(long$SC, t1$SC)

  for (col in cols) {
    from_t1 <- t1[[col]][t1_row]
    long[[col]] <- dplyr::if_else(is.na(long[[col]]), from_t1, long[[col]])
  }

  long
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
