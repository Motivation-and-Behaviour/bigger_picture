#' Attach variable labels to the named columns of a table
bp1336_label <- function(tbl, labels) {
  tbl[names(labels)] <- Map(
    function(x, label) {
      attr(x, "label") <- label
      x
    },
    tbl[names(labels)],
    labels
  )
  tbl
}

#' Labels for the person-level ESM well-being file
bp1336_blw_labels <- function() {
  esm <- "person mean of daily ESM ratings (0-100 VAS, 3 prompts/day over 14 days)"
  c(
    name = "Participant identifier",
    gender = "Gender (codes 1/2; the release does not state which is which)",
    age = "Age in years (two implausible values of 25 are left in place)",
    highschool = "High school (codes 1/2; meaning not stated in the release)",
    grade = "High school grade (1 or 2)",
    Happy = paste("Happy,", esm),
    Angry = paste("Angry,", esm),
    Relaxed = paste("Relaxed,", esm),
    Sad = paste("Sad,", esm),
    Content = paste("Content,", esm),
    Lonely = paste("Lonely,", esm),
    Focused = paste("Focused,", esm),
    Engaged = paste("Engaged,", esm),
    Competent = paste("Competent,", esm),
    `Feel meaningful` = paste("Feel meaningful,", esm),
    `Feel connected` = paste("Feel connected,", esm),
    `Feel satisfied` = paste("Feel satisfied,", esm),
    Positive_Emotion = paste(
      "Positive emotion (happy, relaxed, content),",
      esm
    ),
    Negative_Emotion = paste("Negative emotion (angry, sad, lonely),", esm),
    Eudaimonic_Activity = paste(
      "Eudaimonic activity (engaged, competent, focused),",
      esm
    ),
    Eudaimonic_Wellbeing = paste(
      "Eudaimonic well-being (meaningful, connected, satisfied),",
      esm
    )
  )
}

#' Label and suffix one person-level appnome metric file
bp1336_metric <- function(tbl, family, unit) {
  cols <- setdiff(names(tbl), "name")
  labels <- stats::setNames(
    paste0(
      sub("_[^_]+$", "", cols),
      ": ",
      sub("^.*_", "", cols),
      " of daily ",
      family,
      " (",
      unit,
      ")"
    ),
    cols
  )
  tbl |>
    bp1336_label(labels) |>
    dplyr::rename_with(~ paste0(.x, "_", family), dplyr::all_of(cols))
}

#' Tidier for BPIPD-1336 (Liu; "appnome" study of Swiss adolescents)
#'
#' The person-level ESM well-being file joined on `name` to the three appnome
#' metric files, each suffixed by metric family because they share column names.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant (a single 14-day burst, no waves)
tidy_BPIPD_1336 <- function(raw_dataset, spec) {
  # Four composite names contain a newline, which no expression can reference.
  blw <- raw_dataset$data$between_blw |>
    dplyr::rename(
      Positive_Emotion = "Positive\nEmotion",
      Negative_Emotion = "Negative\nEmotion",
      Eudaimonic_Activity = "Eudaimonic\nActivity",
      Eudaimonic_Wellbeing = "Eudaimonic\nWell-being"
    ) |>
    bp1336_label(bp1336_blw_labels())

  # Full joins keep the one participant with appnome metrics but no ESM row.
  out <- blw |>
    dplyr::full_join(
      bp1336_metric(
        raw_dataset$data$user_screentime_app_metrics,
        "screentime",
        "minutes/day"
      ),
      by = "name",
      relationship = "one-to-one"
    ) |>
    dplyr::full_join(
      bp1336_metric(
        raw_dataset$data$user_activation_app_metrics,
        "activation",
        "app opens/day"
      ),
      by = "name",
      relationship = "one-to-one"
    ) |>
    dplyr::full_join(
      bp1336_metric(
        raw_dataset$data$user_notifications_app_metrics,
        "notification",
        "notifications/day"
      ),
      by = "name",
      relationship = "one-to-one"
    )

  if (anyDuplicated(out$name) > 0) {
    stop("BPIPD-1336: `name` does not uniquely identify rows.", call. = FALSE)
  }

  tibble::as_tibble(out)
}
