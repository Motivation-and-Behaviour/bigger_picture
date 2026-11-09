#' Tidier for BPIPD-1214 (Canadian Community Health Survey 1.1)
#'
#' One cross-sectional PUMF, read from a code-only CSV; variable and value
#' labels are attached from the release's SAS label and format files.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent (`ADMA_RNO`) in a health region that
#'   fielded the sedentary activities module
tidy_BPIPD_1214 <- function(raw_dataset, spec) {
  pumf <- tibble::as_tibble(raw_dataset$data$cchs_1_1_pumf)

  # Sedentary activities is optional content chosen by health region (user
  # guide 4.3, questionnaire SA_BEG); regions that never fielded it go.
  answered <- Reduce(
    `|`,
    lapply(pumf[c("SACA_1", "SACA_2", "SACA_3")], dplyr::between, 1, 8)
  )
  pumf <- pumf[pumf$GEOADPMF %in% unique(pumf$GEOADPMF[answered]), ]

  if (anyDuplicated(pumf$ADMA_RNO) > 0) {
    stop(
      "BPIPD-1214: `ADMA_RNO` does not uniquely identify rows.",
      call. = FALSE
    )
  }

  bp1214_attach_sas_labels(pumf, raw_dataset$codebook)
}

#' Attach the SAS variable labels and value formats to the PUMF columns
bp1214_attach_sas_labels <- function(pumf, codebook) {
  read_sas <- function(name) {
    iconv(readLines(codebook[[name]], warn = FALSE), "latin1", "UTF-8")
  }
  quoted_pair <- "^\\s*(\\S+)\\s*=\\s*'(.*)'\\s*$"
  pair_value <- function(lines) {
    gsub("''", "'", trimws(sub(quoted_pair, "\\2", lines)), fixed = TRUE)
  }

  labels <- read_sas("cchs_1_1_sas_variable_labels")
  labels <- labels[grepl(quoted_pair, labels)]
  variable_labels <- stats::setNames(
    pair_value(labels),
    sub(quoted_pair, "\\1", labels)
  )

  assigned <- read_sas("cchs_1_1_sas_format_assignments")
  assigned <- strsplit(trimws(grep("\\.\\s*$", assigned, value = TRUE)), "\\s+")
  format_of <- stats::setNames(
    sub("\\.$", "", vapply(assigned, `[`, "", 2L)),
    vapply(assigned, `[`, "", 1L)
  )

  proc <- read_sas("cchs_1_1_sas_value_labels")
  is_value <- grepl("^\\s*VALUE\\s", proc)
  format_names <- sub("^\\s*VALUE\\s+(\\S+).*$", "\\1", proc[is_value])
  is_pair <- grepl(quoted_pair, proc)
  value_formats <- split(
    stats::setNames(
      as.numeric(sub(quoted_pair, "\\1", proc[is_pair])),
      pair_value(proc[is_pair])
    ),
    format_names[cumsum(is_value)[is_pair]]
  )

  for (column in names(pumf)) {
    pumf[[column]] <- if (column %in% names(format_of)) {
      haven::labelled(
        pumf[[column]],
        labels = value_formats[[format_of[[column]]]],
        label = variable_labels[[column]]
      )
    } else {
      structure(pumf[[column]], label = variable_labels[[column]])
    }
  }

  pumf
}
