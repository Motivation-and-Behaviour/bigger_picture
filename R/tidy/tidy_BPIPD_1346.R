#' Tidier for BPIPD-1346 (HSLS:09, High School Longitudinal Study of 2009)
#'
#' One wide public-use student file holds every round; only the base year (fall
#' 2009) asked screen items, so its respondents and its instruments are kept.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per base-year student questionnaire respondent
tidy_BPIPD_1346 <- function(raw_dataset, spec) {
  # The school file is not used: `SCH_ID` is suppressed (-5) on the student file.
  raw <- raw_dataset$data$pets_student_data

  # `X1SQSTAT` 1-3 is a base-year student respondent; 8 has no base-year data.
  respondent <- raw$X1SQSTAT %in% 1:3
  tidied <- tibble::as_tibble(raw[respondent, bp1346_columns(names(raw))])

  if (anyDuplicated(tidied$STU_ID) > 0L) {
    stop("BPIPD-1346: `STU_ID` does not uniquely identify rows.", call. = FALSE)
  }
  bp1346_carry_parent_education(bp1346_apply_labels(tidied, raw))
}

#' Columns kept from the student file: named columns, then the base-year instruments
bp1346_columns <- function(available) {
  # X1 composites, S1 student, P1 parent, M1 and N1 maths and science teacher,
  # A1 administrator and C1 counselor questionnaires.
  instruments <- available[grepl("^(X1|S1|P1|M1|N1|A1|C1)", available)]
  union(unlist(bp1346_named_columns(), use.names = FALSE), instruments)
}

#' Columns kept from outside the base-year instrument prefixes, by group
bp1346_named_columns <- function() {
  # Replicate weights and every later round's columns are not kept.
  list(
    # `SCH_ID`, `STRAT_ID` and `psu` are suppressed (-5) on the public-use file.
    design = c(
      "STU_ID",
      "SCH_ID",
      "STRAT_ID",
      "psu",
      "W1STUDENT",
      "W1PARENT",
      "W1MATHTCH",
      "W1SCITCH"
    ),
    status = c("X2UNIV1", "X2UNIV2A", "X2UNIV2B", "X3UNIV1", "X4UNIV1"),
    base_year_rescored = c("X2X1TXMSCR", "X2MTHINT_R", "X2SCIINT"),
    # First follow-up (spring 2012) parent education, carried to the base year.
    parent_education_2012 = c(
      "X2PQSTAT",
      "X2PAREDU",
      "X2MOMEDU",
      "X2DADEDU",
      "X2PAREDU_IM",
      "X2MOMEDU_IM",
      "X2DADEDU_IM"
    ),
    # Transcript composites for the 2009-10 school year, collected in 2013-14.
    grade_9_transcript = c(
      "X3TSTATYR09",
      "X3THIMATH9",
      "X3THISCI9",
      "X3TCRED9TH",
      "X3TGPA9TH",
      "X3TAGPA09"
    )
  )
}

#' Carry 2012 parent education to rows with no base-year parent questionnaire
bp1346_carry_parent_education <- function(tidied) {
  # The 2012 coding adds an occupational certificate (3), so the fill is a new
  # column in that coding; only non-imputed 2012 parent reports fill.
  respondent_2012 <- as.numeric(tidied$X2PQSTAT) %in% 1:4
  parents <- c(parent = "PAR", mother = "MOM", father = "DAD")
  for (who in names(parents)) {
    base <- as.numeric(tidied[[paste0("X1", parents[[who]], "EDU")]])
    later <- tidied[[paste0("X2", parents[[who]], "EDU")]]
    imputed <- as.numeric(tidied[[paste0("X2", parents[[who]], "EDU_IM")]])
    fill <- base == -8 &
      respondent_2012 &
      imputed == 0 &
      as.numeric(later) %in% 1:7
    tidied[[paste0(who, "_edu_carried")]] <- haven::labelled(
      as.integer(ifelse(fill, as.numeric(later), NA)),
      labels = attr(later, "labels"),
      label = paste0(
        attr(later, "label"),
        "; carried from 2012 where X1",
        parents[[who]],
        "EDU is -8, from a non-imputed 2012 parent report (X2PQSTAT 1-4, X2",
        parents[[who]],
        "EDU_IM 0)"
      )
    )
  }
  tidied
}

#' Attach the labels the .rdata file holds as data-frame attributes
bp1346_apply_labels <- function(tidied, raw) {
  at <- match(names(tidied), names(raw))
  tidied[] <- Map(
    bp1346_label_column,
    tidied,
    attr(raw, "var.labels")[at],
    attr(raw, "label.table")[at]
  )
  tidied
}

#' Label one column, attaching value labels only where the file records them
bp1346_label_column <- function(x, label, value_labels) {
  if (is.null(value_labels)) {
    attr(x, "label") <- label
    return(x)
  }
  codes <- stats::setNames(
    as.vector(value_labels, mode = typeof(x)),
    names(value_labels)
  )
  haven::labelled(x, labels = codes, label = label)
}
