#' Tidier for BPIPD-905 (Mandic)
#'
#' One unlabelled sheet, already one row per student; variable labels come from
#' the workbook's `ABREVATION MEANING` sheet, value labels from the questionnaire.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per student (single cross-section)
tidy_BPIPD_905 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  df <- bp905_label_variables(df, raw_dataset$codebook$abbreviations)
  bp905_label_values(df)
}

#' English variable labels from the workbook's `ABREVATION MEANING` sheet
bp905_label_variables <- function(df, path) {
  dictionary <- readxl::read_excel(
    path,
    sheet = "ABREVATION MEANING",
    col_names = c("variable", "label"),
    col_types = "text",
    skip = 2
  )

  labels <- stats::setNames(dictionary$label, dictionary$variable)
  for (column in intersect(names(df), names(labels))) {
    attr(df[[column]], "label") <- labels[[column]]
  }

  df
}

#' Attach the questionnaire's response options to the coded columns
bp905_label_values <- function(df) {
  groups <- bp905_value_labels()

  for (pattern in names(groups)) {
    for (column in grep(pattern, names(df), value = TRUE)) {
      df[[column]] <- haven::labelled(
        df[[column]],
        labels = groups[[pattern]],
        label = attr(df[[column]], "label")
      )
    }
  }

  df
}

#' Questionnaire response options, named by the regex of the columns sharing them
bp905_value_labels <- function() {
  list(
    "^SPOL$" = c(Male = 1, Female = 2),
    "^SKOLA$" = c(
      Gymnasium = 1,
      `Four-year vocational` = 2,
      `Three-year vocational` = 3
    ),
    "^RAZRED$" = c(First = 1, Second = 2, Third = 3, Fourth = 4),
    "^INTERNET_(TJEDAN|VIK)$" = c(
      `I don't spend time on the Internet at all` = 0,
      `From a few minutes to half an hour` = 1,
      `1-2 hours a day` = 2,
      `2-3 hours a day` = 3,
      `3-4 hours a day` = 4,
      `More than 4 hours a day` = 5,
      `I can't estimate` = 6
    ),
    "^UCEST_SVAKOD_" = c(
      `1-3 h a day` = 1,
      `3-5 h daily` = 2,
      `5-8 h daily` = 3,
      `More than 8 hours a day` = 4
    ),
    # Apps are listed so the daily-hours `UCEST_SVAKOD_` columns do not match.
    "^UCEST_(FB|IG|SNAPCHAT|TWITTER|TIKTOK|YT|PINTEREST|FPS|MMORPG|TPS|SI)$" = c(
      Never = 0,
      `1-2 times a month` = 1,
      `About once a week` = 2,
      `Several times a week` = 3,
      Daily = 4
    ),
    "^SDQ_" = c(
      `False - it doesn't describe me at all` = 1,
      `Mostly incorrect` = 2,
      `Mostly true` = 3,
      `True - fully describes me` = 4
    ),
    "^GSE_" = c(
      `It doesn't apply to me at all` = 1,
      `It's mostly not about me` = 2,
      `It neither applies nor does it apply to me` = 3,
      `It's mostly about me` = 4,
      `It's all about me` = 5
    ),
    "^HIP_" = c(
      Never = 1,
      Rare = 2,
      Sometimes = 3,
      Often = 4,
      `Very common` = 5
    ),
    "^UEK_" = c(
      `Not at all` = 1,
      `Mostly not` = 2,
      Sometimes = 3,
      `Mostly yes` = 4,
      `Totally yes` = 5
    ),
    "^DASS_" = c(
      `It didn't apply to me at all` = 0,
      `It applied to me to a certain extent or for a while` = 1,
      `It applied to me to a greater extent or a good part of the time` = 2,
      `Almost entirely or most of the time it was about me` = 3
    ),
    "^IMQ_" = c(
      `(Almost) never` = 1,
      Sometimes = 2,
      `In 50% of cases` = 3,
      `Most times` = 4,
      `(Almost) always` = 5
    )
  )
}
