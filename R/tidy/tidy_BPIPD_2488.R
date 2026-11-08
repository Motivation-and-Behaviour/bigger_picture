#' Tidier for BPIPD-2488 (Arango)
#'
#' Single cross-sectional Excel sheet; variable labels come from the
#' workbook's `Codebook` sheet, since the Excel reader carries none, and
#' from the contributor's reply for the two screen columns it lacks.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant
tidy_BPIPD_2488 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  bp2488_add_codebook_labels(df, raw_dataset$codebook$codebook)
}

#' Attach the unheaded `Codebook` sheet's descriptions as variable labels
bp2488_add_codebook_labels <- function(df, path) {
  codebook <- readxl::read_excel(
    path,
    sheet = "Codebook",
    col_names = c("variable", "description"),
    col_types = "text"
  )

  labels <- stats::setNames(codebook$description, codebook$variable)
  labels <- labels[!is.na(labels)]

  # The codebook abbreviates the one name the data sheet spells out.
  names(labels)[names(labels) == "Low-HRQL-PH"] <- "Low-HRQL-Physical Health"

  # Supplied later without Codebook entries; described in the contributor's reply.
  labels[c("Hours_PC_VG", "Screen_Time")] <- c(
    "Hours on computer/video games",
    "Screen time: TV_Hoursperday + Hours_PC_VG, summed by the study"
  )

  for (column in intersect(names(df), names(labels))) {
    attr(df[[column]], "label") <- labels[[column]]
  }

  df
}
