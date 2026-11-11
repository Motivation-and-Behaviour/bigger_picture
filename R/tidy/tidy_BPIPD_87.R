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

  df[["Fecha de nacimiento"]] <- bp87_birth_date(df[["Fecha de nacimiento"]])

  # 18 rows of these 0/1 flags hold FALSE in place of 0.
  for (col in c("COMPUbedtime1D01", "COMPUbedtime2D01")) {
    df[[col]] <- as.numeric(replace(df[[col]], df[[col]] %in% "FALSE", "0"))
  }

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

#' Birth dates from Excel serial numbers, repairing years mistyped at entry
bp87_birth_date <- function(serial) {
  serial <- as.numeric(serial)
  stored <- as.POSIXlt(as.Date(serial, origin = "1899-12-30"))
  year <- stored$year + 1900L

  # No codebook; read from the data, these fit the ages of each school year.
  repaired_year <- dplyr::case_when(
    year %in% 1:9 ~ 2000L + year,
    year %in% c(101L, 201L, 301L, 401L, 501L, 601L) ~ 2000L + year %/% 100L
  )
  repaired <- ISOdate(
    repaired_year,
    stored$mon + 1L,
    stored$mday,
    hour = 0,
    tz = "UTC"
  )

  out <- as.POSIXct(serial * 86400, origin = "1899-12-30", tz = "UTC")
  out[serial < 0] <- repaired[serial < 0]
  attr(out, "label") <- paste(
    "Birth date; years stored as 1-9 are read as 2001-2009 and years stored",
    "as 101-601 as 2001-2006, other years before 1900 are NA"
  )
  out
}
