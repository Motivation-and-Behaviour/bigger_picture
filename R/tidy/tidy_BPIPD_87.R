#' Tidier for BPIPD-87 (Mendeley)
#'
#' One cross-sectional survey file (Argentina, 2019); the only assembly is
#' removing a repeated submission so that `Código` keys the table.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent
tidy_BPIPD_87 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  # One questionnaire was submitted twice, identical except `Submission Date`.
  resubmitted <- duplicated(df[setdiff(names(df), "Submission Date")])
  df <- df[!resubmitted, , drop = FALSE]

  if (anyDuplicated(df[["Código"]]) > 0) {
    stop("BPIPD-87: `Código` does not uniquely identify rows.", call. = FALSE)
  }

  # This column repeats the weekend-nap hours beside it; its header is misaligned.
  mislabelled <- paste(
    "¿Cuánto tiempo de pantalla (dispositivos electrónicos) utilizas",
    "antes de dormirte los días de semana?"
  )
  attr(df[[mislabelled]], "label") <- paste(
    "MISLABELLED: duplicates the weekend nap duration in hours",
    "(¿Cuánto tiempo dormís la siesta los fines de semana o feriados?);",
    "not a screen-time item"
  )

  df
}
