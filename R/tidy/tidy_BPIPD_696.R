#' Tidier for BPIPD-696 (PLUMS, Kaur)
#'
#' The three workbooks hold one sheet per trial arm per timepoint, so the six
#' sheets stack into one long table with `arm` taken from the sheet each row
#' came from. The midline and endline intervention sheets store the screen-use
#' and sleep items under the full question wording; they are renamed to the
#' control sheets' short names before the bind.
#'
#' Every sheet holds duplicate records: the same child entered twice, with the
#' two entries differing on a handful of items. `bp696_deduplicate()` collapses
#' them to one row per child per wave.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per timepoint
tidy_BPIPD_696 <- function(raw_dataset, spec) {
  sheets <- raw_dataset$data

  tables <- Map(
    function(tbl, name) {
      arm <- sub("^data-[a-z]+-", "", name)
      tbl <- bp696_align_names(tbl)
      if (arm == "intervention") {
        tbl <- bp696_rename_intervention(tbl)
      }
      if (name == "data-baseline-control") {
        tbl <- bp696_swap_income_columns(tbl)
      }
      tibble::add_column(tbl, arm = arm, .after = "Participant_ID")
    },
    sheets,
    names(sheets)
  )

  bp696_check_arms(tables)

  df <- dplyr::bind_rows(bp696_align_types(tables))

  # One baseline row carries a single stray cell and no participant id.
  df <- df[!is.na(df$Participant_ID), ]

  # Three spellings of the date of birth across the six sheets; parsing here
  # keeps the mapping step from having to know which sheet a row came from.
  df <- tibble::add_column(
    df,
    dob = bp696_parse_dob(df$Q4_What_is_the_date_of_birth_of_the_child),
    .after = "Q4_What_is_the_date_of_birth_of_the_child"
  )

  df <- bp696_deduplicate(df)

  if (anyDuplicated(df[c("participant_id", ".wave")]) > 0) {
    stop(
      "BPIPD-696: `participant_id` and `.wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  df
}

#' Collapse duplicate records and build the participant identifier
#'
#' `Participant_ID` repeats within every sheet. Almost all repeats are the same
#' child entered twice: the two rows agree on date of birth and sex and differ
#' only on scattered items, with the sheet's own scale scores recomputed from
#' each entry. Those collapse to the more complete row.
#'
#' The exception is a block of baseline control ids (RCT-D-159 to RCT-D-225)
#' whose two rows carry different dates of birth or different sexes, so they are
#' two different children sharing an id. Those keep one row each and are
#' separated by an occurrence suffix, which says the identity is unresolved
#' rather than asserting they are the same child.
bp696_deduplicate <- function(df) {
  child <- paste(
    df$.wave,
    df$arm,
    df$Participant_ID,
    ifelse(is.na(df$dob), "", format(df$dob)),
    ifelse(is.na(df$Q5_Genderof_the_child), "", df$Q5_Genderof_the_child)
  )

  # Within a child, the entry with fewest empty cells; sheet order breaks ties.
  missing <- rowSums(is.na(df))
  keep <- vapply(
    split(seq_len(nrow(df)), child),
    function(i) i[which.min(missing[i])],
    integer(1)
  )
  df <- df[sort(keep), ]

  # A suffix only where one id covers two children in the same wave; everywhere
  # else the id is `arm`-`Participant_ID` and links a child across waves.
  key <- paste(df$.wave, df$arm, df$Participant_ID)
  occurrence <- stats::ave(seq_len(nrow(df)), key, FUN = seq_along)
  collisions <- stats::ave(seq_len(nrow(df)), key, FUN = length)

  tibble::add_column(
    df,
    participant_id = ifelse(
      collisions > 1L,
      paste(df$arm, df$Participant_ID, occurrence, sep = "-"),
      paste(df$arm, df$Participant_ID, sep = "-")
    ),
    .before = 1
  )
}

#' Put family size and monthly income back under their own headers
#'
#' The baseline control sheet carries monthly income under the
#' `Q14total_family_members` header and the household size under
#' `Q15Monthly_income`; the other five sheets carry them the right way round.
#' `Q16Per_capita_income` equals income divided by household size to within 1%
#' in every row of every sheet once the two are swapped, and the swapped values
#' then agree exactly with the same child's midline and endline rows.
bp696_swap_income_columns <- function(tbl) {
  size <- "Q14total_family_members"
  income <- "Q15Monthly_income"
  both <- c(size, income)
  if (!all(both %in% names(tbl))) {
    stop(
      "BPIPD-696: baseline control sheet is missing ",
      paste(setdiff(both, names(tbl)), collapse = " and "),
      call. = FALSE
    )
  }

  swapped <- tbl[[income]]
  tbl[[income]] <- tbl[[size]]
  tbl[[size]] <- swapped
  tbl
}

#' Parse the date of birth out of its three spellings
#'
#' The baseline and control sheets hold Excel date serials as text, some midline
#' and endline control rows hold `dd-mm-yyyy`, and the midline and endline
#' intervention sheets hold dates that read as POSIXct and print `yyyy-mm-dd`.
bp696_parse_dob <- function(x) {
  out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")

  serial <- suppressWarnings(as.numeric(x))
  is_serial <- !is.na(serial) & serial > 20000 & serial < 60000
  out[is_serial] <- as.Date(serial[is_serial], origin = "1899-12-30")

  iso <- is.na(out) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  out[iso] <- as.Date(substr(x[iso], 1, 10))

  dmy <- is.na(out) & grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", x)
  out[dmy] <- as.Date(x[dmy], format = "%d-%m-%Y")

  unparsed <- is.na(out) & !is.na(x)
  if (any(unparsed)) {
    stop(
      "BPIPD-696: unparsed dates of birth: ",
      paste(unique(x[unparsed]), collapse = ", "),
      call. = FALSE
    )
  }

  out
}

#' Restore the column names readxl could not take from a header row
bp696_align_names <- function(tbl) {
  # Two CBCL header cells are blank in the baseline sheets; the midline and
  # endline control sheets name the same two positions.
  names(tbl)[names(tbl) == "...224"] <- "Q13_Cries_a_lot"
  names(tbl)[names(tbl) == "...236"] <-
    "Q25_Doesn’t_get_along_with_other_children"

  # A trailing column of the baseline intervention sheet has no header and one
  # value throughout; naming it stops `add_column()` renumbering it later.
  names(tbl)[names(tbl) == "...328"] <- "unnamed_baseline_intervention_column"

  # The midline control sheet repeats the Q9 column; the two copies are equal.
  garden <-
    "Q9_Do_you_have_a_garden_park_area_in_near_the_house_where_the_child_can_play"
  tbl <- tbl[names(tbl) != paste0(garden, "...136")]
  names(tbl)[names(tbl) == paste0(garden, "...34")] <- garden

  tbl
}

#' Give an intervention sheet the control sheets' column names
bp696_rename_intervention <- function(tbl) {
  map <- bp696_intervention_columns()
  map <- map[map %in% names(tbl)]
  names(tbl)[match(map, names(tbl))] <- names(map)
  tbl
}

#' Stop if an intervention sheet holds a column its control sheet does not
#'
#' A stale rename map strands an item in its own column instead of failing, so
#' nothing downstream would notice.
bp696_check_arms <- function(tables) {
  extras <- c(
    "Q18.11_INTERNET_CONNECTION_usually_placed_in_the_room_where_the_child_sleeps_plays",
    "Q21.7_Any_other_reasons_please_specify",
    "Q28.10_Any_others_specify",
    "unnamed_baseline_intervention_column"
  )

  for (wave in c("baseline", "midline", "endline")) {
    stray <- setdiff(
      names(tables[[paste0("data-", wave, "-intervention")]]),
      c(names(tables[[paste0("data-", wave, "-control")]]), extras)
    )
    if (length(stray) > 0) {
      stop(
        "BPIPD-696: ",
        wave,
        " intervention columns with no control counterpart: ",
        paste(stray, collapse = ", "),
        call. = FALSE
      )
    }
  }
}

#' Cast to character the few columns the sheets read as more than one type
#'
#' Each is a numeric item with one text answer typed into it in a single sheet,
#' apart from the date of birth, which is an Excel serial in some sheets and a
#' date in others; casting keeps every value for the mapping step to resolve.
bp696_align_types <- function(tables) {
  observed <- function(name) {
    unique(unlist(lapply(tables, function(tbl) {
      x <- tbl[[name]]
      if (is.null(x) || all(is.na(x))) NULL else class(x)[1]
    })))
  }

  all_names <- unique(unlist(lapply(tables, names)))
  mixed <- all_names[
    vapply(all_names, function(name) length(observed(name)) > 1L, logical(1))
  ]

  lapply(tables, function(tbl) {
    hit <- intersect(mixed, names(tbl))
    tbl[hit] <- lapply(tbl[hit], as.character)
    tbl
  })
}

#' The control sheets' name for each intervention-sheet column that differs
#'
#' Paired by question number within instrument block (screen-use questionnaire,
#' sleep scale, behaviour checklist); the entries below the screen-use items
#' were matched by position and confirmed against their values.
bp696_intervention_columns <- function() {
  c(
    "Q1watching_too_muchST" = "Q1_Do_you_think_that_your_child_is_watching_too_much_digital_media",
    "Q2watching_excessiveST_can_affect_health" = "Q2_Have_you_ever_thought_that_watching_excessive_digital_media_can_affect_the_mental__physical_health_of_your_child",
    "Q3limiting_ST" = "Q3_Did_you_ever_think_of_limiting_the_digital_screen_time_of_your_child?",
    "Q3.1Yes.providing_alternatives" = "Q3.1_1_I_am_providing_alternative_to_limit_the_excessive_digital_screen_time_since",
    "Q3.2Yes_planned_to_limit" = "Q3.2_1__I_have_planned_to_limit_it_but_am_2t_actively_doing_it",
    "Q3.3_No. never_thought_about_it" = "Q3.3_2__I_have_never_thought_about_it",
    "Q4reason_why_not" = "Q4_If_2__then_tell_us_the_reason_why_you_were_2t_able_to_limit_the_child’s_digital_screen_time",
    "Q5tell_us_how" = "Q5_If_you_have_planned_it__then_tell_us_how_do_you_plan_to_limit_the_excessive_digital_media",
    "Q5.1_Reading_booksd" = "Q5.1_Reading_books__stories_to_the_child",
    "Q5.2playing_inside_the_house" = "Q5.2_Kept_the_child_busy_in_playing_inside_the_house_such_as_colouring__drawing__painting_etc",
    "Q5.3outside_play" = "Q5.3_Promoting_outside_play_such_as_sports__active_play_etc",
    "Q5.4_Promoting_socialization" = "Q5.4_Promoting_socialization_with_others_such_as__play-dates__social_gatherings_etc",
    "Q5.5_Changing_home_environment_" = "Q5.5_Changes_in_the_home_environment_such_as__access_to_gadgets__placement_of_gadgets__control_device_etc",
    "Q5.6_Restricting_the_digital-media" = "Q5.6_Restricting_the_digital-media_duration_and_habits_of_the_child_at_home",
    "Q6ST_allowed" = "Q6_If_you_are_already_doing_the_activities_then_please_tell_us_what_all_alternatives_are_you_providing_the_child_to_reduce_the_screen_time_among_them_along_with_the_duration",
    "Q6.1AlreadyReading_books" = "Q6.1_Reading_books__stories_to_the_child_since",
    "Q6.2already_inside_play" = "Q6.2_Promoting_inside_play_such_as__coloring__drawing__painting_etc",
    "Q6.3_already.outside_play" = "Q6.3_Promoting_outside_play_such_as__sports__active_play_etc",
    "Q6.4AlreadySocializing_with_others" = "Q6.4_Socializing_with_others_such_as__play-dates__social_gatherings_etc._since",
    "Q6.5_Changed_home_environment" = "Q6.5_Changes_in_the_home_environment_such_as__access_to_gadgets__placement_of_gadgets__control_device_etc._since",
    "Q6.6Restricted_ST" = "Q6.6_Restricting_the_digital-media_duration_and_habits_of_the_child_at_home",
    "Q7Since.6_months.continue" = "Q7_For_those_who_have_been_doing_the_activities_for_the_last_6_months_Will_you_continue_restrict_the_child_in_the_same_way_in_future_also?_in_order_to_regulate_your_child’s_screen_",
    "Q14total_family_members" = "Q14_What_is_the_total_number_of_family_members",
    "Q15Monthly_income" = "Q15_Monthly_family_income",
    "Q16Per_capita_income" = "Q16_Per_capita_income_of_the_family",
    "Q17Informal__care_days_per_week" = "Q17_Informal_child_care_Number_of_days_per_week",
    "Q17.1Informal_child_care_Duration.hours" = "Q17.1_Informal_child_care_Duration_day__total_hours",
    "Q17.2formal_care_days_per_week" = "Q17.2_formal_child_care_Number_of_days_per_week",
    "Q17.3formal__care_Duration" = "Q17.3_formal_child_care_Duration_day__total_hours",
    "Q17.4parents_at.home" = "Q17.4_informal_child_care_With_the_parentsat_home",
    "Q17.5formal_care_With_the_parentsat_home" = "Q17.5_formal_care_With_the_parentsat_home",
    "Q18TV_set_with_cable" = "Q18_TV_set_with_cable_satellite_connection",
    "Q18.2Mobile_phone_without_internet" = "Q18.2_Mobile_phone_without_internet",
    "Q18.3Smart_phone_with_internet" = "Q18.3_Smart_phone_with_internet",
    "Q18.4_Hand_held_devices" = "Q18.4_Hand_held_devices_on_which_video_games_can_be_played_tablet",
    "Q18.5_Internet_connection" = "Q18.5_Internet_connection_broadband_WiFi",
    "Q18.6TV_where_the_child_sleeps_plays" = "Q18.6_TV_usually_placed_in_the_room_where_the_child_sleeps_plays",
    "Q18.7_COMPUTER_room_where_the_child_sleeps_plays" = "Q18.7_COMPUTER_usually_placed_in_the_room_where_the_child_sleeps_plays",
    "Q18.8_MOBILE_PHONE.room_where_the_child_sleeps_plays" = "Q18.8_MOBILE_PHONE_WITHOUT_INTERNET_usually_placed_in_the_room_where_the_child_sleeps_plays",
    "Q18.9_MOBILE_PHONE_WITH_INTERNET_the_room_where_the_child_sleeps_plays" = "Q18.9_MOBILE_PHONE_WITH_INTERNET_usually_placed_in_the_room_where_the_child_sleeps_plays",
    "Q18.10_TABLET_usually_where_the_child_sleeps_plays" = "Q18.10_TABLET_usually_placed_in_the_room_where_the_child_sleeps_plays",
    "Q19_Frequency_WATCHES_TV" = "Q19_Frequency_of_performing_the_activity_in_a_given_week_WATCHES_TV",
    "Q19.12duration_WATCHES_TV.WND" = "Q19.12_Average_duration_on_holidays_per_day_MIN_WATCHES_TV",
    "Q19.18supervised_WATCHES_TV" = "Q19.18_Whether_the_child_does_the_above_said_activity_supervised_in_a_week_WATCHES_TV",
    "Q19.1_Frequency_SMART_PHONE" = "Q19.1_Frequency_of_performing_the_activity_in_a_given_week_SMART_PHONE",
    "Q19.7duration_SMART_PHONE.WKD" = "Q19.7_Average_duration_on_working__school_days_per_day_MIN__SMART_PHONE",
    "Q19.13duration_SMARTPHONE.WND" = "Q19.13_Average_duration_on_holidays_per_day_MIN_SMART_PHONE",
    "Q19.19_supervised_SMARTPHONE" = "Q19.19_Whether_the_child_does_the_above_said_activity_supervised_in_a_week_SMART_PHONE",
    "Q19.2_OTHER_GADGETS" = "Q19.2_Frequency_of_performing_the_activity_in_a_given_week_OTHER_GADGETS",
    "Q19.8Duration_OTHER_GADGETS.WKD" = "Q19.8_Average_duration_on_working__school_days_per_day_MIN_OTHER_GADGETS",
    "Q19.14durationOTHER_GADGETS.WND" = "Q19.14_Average_duration_on_holidays_per_day_MIN_OTHER_GADGETS",
    "Q19.20supervisedOTHER_GADGETS" = "Q19.20_Whether_the_child_does_the_above_said_activity_supervised_in_a_week_OTHER_GADGETS",
    "Q19.3_Frequency_DRAWS_COLORS" = "Q19.3_Frequency_of_performing_the_activity_in_a_given_week_WRITES_DRAWS_COLORS",
    "Q19.15duration_DRAWS_COLORS.WND" = "Q19.15_Average_duration_on_holidays_per_day_MIN_DRAWS_COLORS",
    "Q19.9duration_WRITES_DRAWS_COLORS.WKD" = "Q19.9_Average_duration_on_working__school_days_per_day_MIN_WRITES_DRAWS_COLORS",
    "Q19.21FrequencyWRITES_DRAWS_COLORS" = "Q19.21_Frequency_of_performing_the_activity_in_a_given_week_WRITES_DRAWS_COLORS",
    "Q19.4Frequency.READS_PRETENDS_TO_READ" = "Q19.4_Frequency_of_performing_the_activity_in_a_given_week_READS_PRETENDS_TO_READ",
    "Q19.16durationREADS_PRETENDS_TO_READ.WND" = "Q19.16_Average_duration_on_holidays_per_day_MIN_READS_PRETENDS_TO_READ",
    "Q19.10duration_READS_PRETENDS_TO_READ.WKD" = "Q19.10_Average_duration_on_working__school_days_per_day_MIN_READS_PRETENDS_TO_READ",
    "Q19.22Frequency of supervision_READS_PRETENDS_TO_READ" = "Q19.22_Frequency_of_performing_the_activity_in_a_given_week_READS_PRETENDS_TO_READ",
    "Q19.5_Frequency_pretends to read" = "Q19.5_Frequency_of_performing_the_activity_in_a_given_week_READ_BY_SOMEONE",
    "Q19.11durationREAD_BY_SOMEONE.WKD" = "Q19.11_Average_duration_on_working__school_days_per_day_MIN_READ_BY_SOMEONE",
    "Q19.17duration_READ_BY_SOMEONE.WND" = "Q19.17_Average_duration_on_holidays_per_day_MIN_READ_BY_SOMEONE",
    "Q19.23Frequency_READ_BY_SOMEONE" = "Q19.23_Frequency_of_performing_the_activity_in_a_given_week_READ_BY_SOMEONE",
    "Q20_videos_name.A" = "Q20_Which_programs_videos_did_the_child_watch_1terday_A",
    "Q20.4duration.A" = "Q20.4_What_was_the_duration_of_these_programs_videos_A",
    "Q20.1programs_name.B" = "Q20.1_Which_programs_videos_did_the_child_watch_1terday_B",
    "Q20.5duration.B" = "Q20.5_What_was_the_duration_of_these_programs_videos_B",
    "Q20.2_name.videos.C" = "Q20.2_Which_programs_videos_did_the_child_watch_1terday_C",
    "Q20.6duration.C" = "Q20.6_What_was_the_duration_of_these_programs_videos_C",
    "Q20.3name.videos.D" = "Q20.3_Which_programs_videos_did_the_child_watch_1terday_D",
    "Q20.7duration.D" = "Q20.7_What_was_the_duration_of_these_programs_videos_D",
    "Q21rules_digital_screen" = "Q21_Do_you_have_any_rules_regarding_when__where__what_&_how_to_watch_digital_screen",
    "Q21.2Only_children’s_channel" = "Q21.2Only_children’s_channel_allowed",
    "Q21.23Nomedia_gadget_1h_before_sleep" = "Q21.23The_child_isn’t_allowed_any_media_gadget_1h_before_sleep",
    "Q21.4watch_supervised" = "Q21.4_The_child_is_allowed_only_watch_supervised_by_adults",
    "Q21.5distance_TV" = "Q21.5_The_child_isn’t_allowed_to_sit_near_the_TV",
    "Q21.6restricted_duration" = "Q21.6_The_child_is_allowed_to_watch_only_for_a_restricted_duration",
    "Q22ST.MOTHER" = "Q22_Average_duration_of_screen_time_per_day_MOTHER_in_min",
    "Q22.1ST.FATHER" = "Q22.1_Average_duration_of_screen_time_per_day_FATHER_in_min",
    "Q22.2frequency_of_media_gadget_MOTHER" = "Q22.2_In_a_week_what_is_the_frequency_of_media_gadget_usage_MOTHER",
    "Q22.3frequency_of_media_gadgetFATHER" = "Q22.3_In_a_week_what_is_the_frequency_of_media_gadget_usage_FATHER",
    "Q22.4Gadgets_used_MOTHER" = "Q22.4_Gadgets_used_MOTHER",
    "Q22.5Gadgets_used_FATHER" = "Q22.5_Gadgets_used_FATHER",
    "Q22.6time_with_child.MOTHER" = "Q22.6_Average_time_spent_with_the_child_at_home_MOTHER-in-min",
    "Q22.7time_child_FATHER" = "Q22.7_Average_time_spent_with_the_child_at_home_FATHER-in-min",
    "Q23duration.outside_playWKD" = "Q23_Average_duration_of_outside_play_per_day_on_working_school_days_MIN",
    "Q23.1duration_outside_play_WND" = "Q23.1_Average_duration_on_holidays_of_outside_play_per_day__MIN",
    "Q25homework_assignments" = "Q25_The_child_uses_for_completing_their_homework_assignments",
    "Q25.1video_calling_" = "Q25.1_The_child_uses_video_calling_applications_to_talk_to_the_family_friends",
    "Q25.2learning" = "Q25.2_The_child_uses_for_learning_poems_rhymes_ABC_etc_online",
    "Q25.3maths_numbers_tables" = "Q25.3_The_child_uses_to_learns_maths_numbers_tables_online",
    "Q25.4recognize_shapes_sounds_colors" = "Q25.4_The_child_uses_to_recognize_shapes_sounds_colors_when_shown_online",
    "Q25.5_sciences" = "Q25.5_The_child_learns_various_sciences_online",
    "Q25.6draw__write_online" = "Q25.6_The_child_learns_to_draw__write_online",
    "Q25.7video-games" = "Q25.7_The_child_plays_video-games",
    "Q25.9adult_programs" = "Q25.9_The_child_to_watch_adult_programs",
    "Q25.10letters_words_vocabulary_language_online" = "Q25.10_The_child_uses_to_learns_letters__words__vocabulary__language_online",
    "Q25.11random_content" = "Q25.11_The_child_uses_to_watch_random_things_for_enjoyment__music__advertisements__babyTV__click_photos_etc",
    "Q26.2_Talks_to_the_character_on_the_screen" = "Q26.2_Talks_to_the_character_on_the__screen",
    "Q27Perceptions.learning_good_habits" = "Q27_The_child_is_learning_good_habits",
    "Q27.1Perceptions.Increasing_his__her_knowledge" = "Q27.1_The_child_is_Increasing_his__her_k2wledge",
    "Q27.2Perceptions.learning_new_skills" = "Q27.2_The_child_is_learning_new_skills",
    "Q27.3Perceptions.growth&development" = "Q27.3_Its_good_for_my_childs_growth_&_development",
    "Q28Perceptions.imitating_what_he_watches" = "Q28_The_child_starts_imitating_what_he_watches",
    "Q28.1Perceptions.sleep_problems" = "Q28.1_The_child_develops_sleep_problems",
    "Q28.2Perceptions.eating_unhealthy_food" = "Q28.2_The_child_might_start_eating_unhealthy_food",
    "Q28.3Perceptions._aggressive" = "Q28.3_the_child_might_become_aggressive",
    "Q28.4Perceptions.isoltion" = "Q28.4_The_child_isoltaes_himself_herself",
    "Q28.5Perceptions.Impairs_concentration" = "Q28.5_It_might_Impairs_the_child’s_concentration",
    "Q28.6Perceptions.behavior_problems" = "Q28.6_It_might_cause_behavior_problems",
    "Q28.7Perceptions.impair__eyesight" = "Q28.7_It_might_impair_the_child’s_e1ight",
    "Q28.8Perceptions.NOT.good.growth_development" = "Q28.8_It’s_2t_good_for_my_childs_growth_development",
    "Q28.9Perceptions.No_negative_effects" = "Q28.9_2_negative_effects",
    "Q28.11Perceptions.background_TV_affects_the_children" = "Q28.11_Do_you_think_presence_of_TV_in_the_background_in_the_room_where_the_child_plays__sleeps_affects_the_children__per_week",
    "Q1hours_of_sleep_" = "Q1Hours_sleep",
    "Q2time_child_takes_fall_asleep" = "Q2usually_fall_asleep",
    "Q3The_child_goes_to_bed_reluctantly" = "Q3_The_child_goes_to_bed_reluctantly",
    "Q4.difficulty_getting_to_sleep_at_night" = "Q4_The_child_has_difficulty_getting_to_sleep_at_night",
    "Q5.anxious_or_afraid_when_falling_asleep" = "Q5_The_child_feels_anxious_or_afraid_when_falling_asleep",
    "Q_6.startles_or_jerks_parts_of_the_body_while_falling_asleep" = "Q_6The_child_startles_or_jerks_parts_of_the_body_while_falling_asleep",
    "Q7.epetitive_actions_such_as_rocking_or_head_banging_while_falling_asleep" = "Q7_The_child_shows_repetitive_actions_such_as_rocking_or_head_banging_while_falling_asleep",
    "Q8.vivid_dream-like_scenes_while_falling_asleep" = "Q8_The_child_experiences_vivid_dream-like_scenes_while_falling_asleep",
    "Q9.sweats_excessively_while_falling_asleep" = "Q9_The_child_sweats_excessively_while_falling_asleep",
    "Q10.wakes_up_more_than_twice_per_night" = "Q10_The_child_wakes_up_more_than_twice_per_night",
    "Q11.After_waking_up_difficulty_to_fall_asleep_again" = "Q11_After_waking_up_in_the_night__the_child_has_difficulty_to_fall_asleep_again",
    "Q12.frequent_twitching_or_jerking_of_legs_while_asleep_or_often_changes_position_or_kicks_the_covers_off_the_bed" = "Q12_The_child_has_frequent_twitching_or_jerking_of_legs_while_asleep_or_often_changes_position_during_the_night_or_kicks_the_covers_off_the_bed",
    "Q13.difficulty_in_breathing_during_the_night" = "Q13_The_child_has_difficulty_in_breathing_during_the_night",
    "Q14.gasps_for_breath_or_is_unable_to_breathe_during_sleep" = "Q14_The_child_gasps_for_breath_or_is_unable_to_breathe_during_sleep",
    "Q15.child_snores" = "Q15_The_child_s2res",
    "Q16.sweats_excessively_during_the_night" = "Q16_The_child_sweats_excessively_during_the_night",
    "Q17.child_sleepwalking" = "Q17_You_have_observed_the_child_sleepwalking",
    "Q18.talking_in_his_her_sleep" = "Q18_You_have_observed_the_child_talking_in_his_her_sleep",
    "Q19.grinds_teeth_during_sleep" = "Q19_The_child_grinds_teeth_during_sleep",
    "Q20.wakes_screaming_or_confused_no_memory_of_these_events_the_next_morning" = "Q20_The_child_wakes_from_sleep_screaming_or_confused_so_that_you_can2t_seem__to_get_through_to_him_her__but_has_2_memory_of_these_events_the_next_morning",
    "Q21.nightmares_which_he_she_doesn’t_remember" = "Q21_The_child_has_nightmares_which_he_she_doesn’t_remember_the_next_day",
    "Q22.unusually_difficult_to_wake_up_in_the_morning" = "Q22_The_child_is_unusually_difficult_to_wake_up_in_the_morning",
    "Q23.awakes__morning_feeling_tired" = "Q23_The_child_awakes_in_the_morning_feeling_tired",
    "Q24.unable_to_move_when_waking_up_in_the_morning" = "Q24_The_child_feels_unable_to_move_when_waking_up_in_the_morning",
    "Q25.daytime_somnolence" = "Q25_The_child_experiences_daytime_som2lence",
    "Age" = "Q4.1_Approximate_age",
    "19.6duration_WATCHES_TV.WKD" = "19.6_Average_duration_on_working__school_days_per_day_MIN_WATCHES_TV",
    "WKD.ST" = "Weekday_ST",
    "WND.ST" = "Weekend_ST",
    "AV.ST" = "av.ST.",
    "AV.ST" = "av.dse",
    "Educational.ST" = "Online.ST",
    "Q27.4Perceptions.positive_effects" = "Q27.4_2_positive_effects",
    "Q27.4Perceptions.others" = "Q27.4_Any_others_specify",
    "Q26.asleep_suddenly_in_inappropriate_situations" = "Q26_The_child_falls_asleep_suddenly_in_inappropriate_situations",
    "QQ25_Disorders_of_initiating_and_maintaining_sleep__sum_the_score_of_the_items_1_2_3_4_5_10_11" = "Disorders_of_initiating_and_maintaining_sleep_1_2_3_4_5_10_11",
    "Q26_Sleep_Breathing_Disorders__sum_the_score_of_the_items_13_14_15" = "Sleep_Breathing_Disorders_13_14_15",
    "Q27_Disorders_of_arousal__sum_the_score_of_the_items_17_20_21" = "Disorders_of_arousal_sum_17_20_21",
    "Q28_Sleep-Wake_Transition_Disorders__sum_the_score_of_the_items_6_7_8_12_18_19" = "Sleep-Wake_Transition_Disorders_6_7_8_12_18_19",
    "Q29_Disorders_of_excessive_som2lence__sum_the_score_of_the_items_22_23_24_25_26" = "Disorders_of_excessive_somnolence__22_23_24_25_26",
    "Q30_Sleep_Hyperhydrosis__sum_the_score_of_the_items_9_16" = "Sleep_Hyperhydrosis_9_16",
    "Q40_Hits_others" = "2"
  )
}
