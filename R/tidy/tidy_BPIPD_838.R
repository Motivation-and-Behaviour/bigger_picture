#' Tidier for BPIPD-838
#'
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble to be used as the harmonisation input
tidy_BPIPD_838 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$`data-sav`)

  # T1's class and consent items were unsuffixed.
  # Suffix them so they pivot too.
  df <- dplyr::rename_with(
    df,
    ~ paste0(.x, "_T1"),
    dplyr::all_of(c("Class", "Consent1", "Consent2", "Consent3"))
  )

  # Reverse-scored items carry the wave token mid-name (`E7_T1r`)
  df <- dplyr::rename_with(df, ~ sub("^(.*)_T([0-9]+)(r?)$", "\\1\\3_T\\2", .x))

  df <- unify_wave_value_labels(df)

  df |>
    tidyr::pivot_longer(
      cols = dplyr::matches("_T[0-9]+$"),
      names_to = c(".value", "wave"),
      names_pattern = "^(.*)_(T[0-9]+)$"
    ) |>
    dplyr::relocate(wave, .after = SC)
}

#' Give every wave of a variable the same SPSS value labels
unify_wave_value_labels <- function(df) {
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
