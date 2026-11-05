#' Tidier for BPIPD-989 (MediaQuotient)
#'
#' One SPSS file of parent MediaQuotient questionnaires, each reporting on the
#' youngest (or only) child and the oldest child under 18 in paired columns;
#' this splits each questionnaire into one row per child reported on.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per questionnaire (single occasion)
tidy_BPIPD_989 <- function(raw_dataset, spec) {
  mq <- tibble::as_tibble(raw_dataset$data$mq_all_data)

  # No column is unique: `uniqueno` is 0 for 323 rows and `group` + `subject`
  # repeats, so the row position keys the questionnaire.
  mq <- dplyr::mutate(
    mq,
    questionnaire_id = sprintf("%04d", seq_len(nrow(mq))),
    .before = 1
  )

  # 50 questionnaires answer no item at all.
  items <- grep("^v[0-9]{3}$", names(mq), value = TRUE)
  mq <- mq[rowSums(!is.na(mq[items])) > 0, , drop = FALSE]

  # `name` holds respondents' names, a direct identifier no mapping uses.
  mq$name <- NULL

  map <- bp989_child_items()
  household <- mq[setdiff(names(mq), c(map$youngest, map$oldest))]
  youngest <- bp989_child(mq, map, "youngest")
  # The oldest-child block is filled only for a second child (Q4).
  oldest <- bp989_child(mq, map, "oldest")
  oldest <- oldest[rowSums(!is.na(oldest[map$stem])) > 0, , drop = FALSE]

  df <- dplyr::bind_rows(youngest, oldest) |>
    dplyr::left_join(
      household,
      by = "questionnaire_id",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      participant_id = paste0(questionnaire_id, "_", child),
      .before = 1
    )
  df <- df[order(df$questionnaire_id, df$child == "oldest"), , drop = FALSE]

  # `bind_rows()` drops the labels of unlabelled columns such as `hrs_wk`.
  df <- bp989_label_stems(df, map)
  attr(df$questionnaire_id, "label") <-
    "Row position of the questionnaire in the source file"
  attr(df$participant_id, "label") <-
    "Questionnaire row position and child (<row>_youngest, <row>_oldest)"
  attr(df$child, "label") <- paste(
    "Child the per-child items describe: youngest child at least 2 years old",
    "or only child (youngest), or oldest child under 18 (oldest)"
  )

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-989: `participant_id` is not unique.", call. = FALSE)
  }

  df
}

#' One child's per-child items under the shared stems
bp989_child <- function(mq, map, child) {
  out <- mq[c("questionnaire_id", map[[child]])]
  names(out) <- c("questionnaire_id", map$stem)
  # Both children's columns need one label for the labelled vectors to bind.
  out <- bp989_label_stems(out, map)
  dplyr::mutate(out, child = child, .after = "questionnaire_id")
}

#' Label each per-child stem with its wording and both source columns
bp989_label_stems <- function(df, map) {
  for (i in seq_len(nrow(map))) {
    attr(df[[map$stem[i]]], "label") <- sprintf(
      "%s (youngest: %s; oldest: %s)",
      map$label[i],
      map$youngest[i],
      map$oldest[i]
    )
  }
  df
}

#' Per-child item pairs: tidied stem, youngest and oldest source columns, label
bp989_child_items <- function() {
  tibble::tribble(
    ~stem                   , ~youngest , ~oldest   , ~label                                                     ,
    "q3_age"                , "v007"    , "v009"    , "Q3/Q4. Age of the child"                                  ,
    "q3a_sex"               , "v008"    , "v010"    , "Q3a/Q4a. Child: Male or female"                           ,
    "q5a"                   , "v011"    , "v012"    , "Q5a. How often need to ask to see movie"                  ,
    "q5b"                   , "v013"    , "v014"    , "Q5b. How often have TV on while doing homework"           ,
    "q5c"                   , "v015"    , "v016"    , "Q5c. How often want to dress like sports, music,stars"    ,
    "q5d"                   , "v017"    , "v018"    , "Q5d. How often want to buy products seen on TV"           ,
    "q5e"                   , "v019"    , "v020"    , "Q5e. How often watch educational TV"                      ,
    "q5f"                   , "v021"    , "v022"    , "Q5f. How often watch music videos on TV"                  ,
    "q5g"                   , "v023"    , "v024"    , "Q5g. How often read books"                                ,
    "q5h"                   , "v025"    , "v026"    , "Q5h. How often play video/computer games"                 ,
    "q5i"                   , "v027"    , "v028"    , "Q5i. How often copy characters seen on TV"                ,
    "q5j"                   , "v029"    , "v030"    , "Q5j. How often see R-rated movies"                        ,
    "q5k"                   , "v031"    , "v032"    , "Q5k. How often draw pictures or write stories"            ,
    "q5l"                   , "v033"    , "v034"    , "Q5l. How often see you read"                              ,
    "q5m"                   , "v035"    , "v036"    , "Q5m. How often go to the library"                         ,
    "q5n"                   , "v037"    , "v038"    , "Q5n. Do activities not including electronic media"        ,
    "q6a"                   , "v039"    , "v040"    , "Q6a. How often help select TV programs"                   ,
    "q6b"                   , "v041"    , "v042"    , "Q6b. How often watch TV programs with child"              ,
    "q6c"                   , "v043"    , "v044"    , "Q6c. How often use rating system to choose programs"      ,
    "q6d"                   , "v045"    , "v046"    , "Q6d. How often know what movie child is going to see"     ,
    "q6e"                   , "v047"    , "v048"    , "Q6e. Check rating before allowing child to see movie"     ,
    "q6f"                   , "v049"    , "v050"    , "Q6f. Check movie reviews/ratings other than industry"     ,
    "q6g"                   , "v051"    , "v052"    , "Q6g. Check with parents re:movies at parties/sleep-overs" ,
    "q6h"                   , "v053"    , "v054"    , "Q6h. How often know what tapes/CDs your child buys"       ,
    "q6i"                   , "v055"    , "v056"    , "Q6i. How often put time limits on computer/video games"   ,
    "q6j"                   , "v057"    , "v058"    , "Q6j. How often monitor your child's internet use"         ,
    "q6k"                   , "v059"    , "v060"    , "Q6k. Help child find approp. internet sites"              ,
    "q6l"                   , "v061"    , "v062"    , "Q6l. How often talk to child about TV programs"           ,
    "q6m"                   , "v063"    , "v064"    , "Q6m. How often talk about music child listens to"         ,
    "q6n"                   , "v065"    , "v066"    , "Q6n. How often read to your child"                        ,
    "q7_school_performance" , "v067"    , "v068"    , "Q7/Q8. School performance of the child"                   ,
    "q9_bedroom_tv"         , "v069"    , "v070"    , "Q9/Q10. Does the child have TV in his or her bedroom"     ,
    "q26_wd_6am_noon"       , "v103"    , "v109"    , "Q26. Weekdays 6am-Noon"                                   ,
    "q26_wd_noon_6pm"       , "v104"    , "v110"    , "Q26. Weekdays Noon-6pm"                                   ,
    "q26_wd_6pm_midnight"   , "v105"    , "v111"    , "Q26. Weekdays 6pm-Midnight"                               ,
    "q26_we_6am_noon"       , "v106"    , "v112"    , "Q26. Weekends 6am-Noon"                                   ,
    "q26_we_noon_6pm"       , "v107"    , "v113"    , "Q26. Weekends Noon-6pm"                                   ,
    "q26_we_6pm_midnight"   , "v108"    , "v114"    , "Q26. Weekends 6pm-Midnight"                               ,
    "q27_vacation_tv"       , "v115"    , "v116"    , "Q27. TV watched during school vacations"                  ,
    "hrs_wk"                , "hrs_wk1" , "hrs_wk2" , "Hours per week the child watches TV"
  )
}
