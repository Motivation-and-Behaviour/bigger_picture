#' Tidier for BPIPD-2309 (Project AWeSome)
#'
#' Project AWeSome: one cohort in two 3-week experience sampling bursts, burst 1
#' (E1, Nov-Dec 2019) from two of its three releases joined survey for survey,
#' burst 2 (E2, June 2020) from one; each burst's surveys are summarised per
#' adolescent, with the past-hour screen minutes scaled to hours per day by the
#' hours the survey windows cover.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent per burst (`ID`, `wave`)
tidy_BPIPD_2309 <- function(raw_dataset, spec) {
  # Every Beyens column is also in the Pouwels or Valkenburg ESM release (comparison in the notes), so that release is not used.
  # The ESM release keeps answers on 423 surveys the other releases left blank, and codes survey states as burst 2 does.
  esm <- raw_dataset$data$burst1_valkenburg_esm[c(
    "ID",
    "BeepNE1",
    "TimeE1",
    "StateE1",
    "INP01E1",
    "SNP01E1",
    "AWBE1",
    "ENJ01E1",
    "ENV01E1",
    "INS01E1"
  )]
  # `TimeE1` separates the one survey number recorded twice (ID 237, survey 108).
  burst1 <- raw_dataset$data$burst1_pouwels |>
    dplyr::select(!c("INP01E1", "SNP01E1")) |>
    dplyr::inner_join(
      esm,
      by = c("ID", "BeepNE1", "TimeE1"),
      relationship = "one-to-one",
      unmatched = "error"
    ) |>
    dplyr::rename(
      Age = "AgeW1",
      ING01S = "ING01S1",
      WAG01S = "WAG01S1",
      SNG01S = "SNG01S1"
    )
  burst2 <- raw_dataset$data$burst2_valkenburg |>
    dplyr::rename(ING01S = "ING01S2", WAG01S = "WAG01S2", SNG01S = "SNG01S2")

  df <- dplyr::bind_rows(
    bp2309_summarise_burst(
      bp2309_add_totals(burst1, bp2309_platforms_e1()),
      "E1",
      c(bp2309_items_e1(), bp2309_items_totals()),
      "StateE1",
      "WeekNE1"
    ),
    bp2309_summarise_burst(
      bp2309_add_totals(burst2, bp2309_platforms_e2()),
      "E2",
      c(bp2309_items_e2(), bp2309_items_totals()),
      "StateE2",
      "WeekNE2"
    )
  )

  # Both bursts fall in the 2019/20 school year, so track stays at one school stage.
  df$EduLevel <- carry_within(df$EduLevel, df$ID, match(df$wave, c("E1", "E2")))
  # No age item at burst 2: burst 1 age plus the 189 days between the bursts' start dates (notification schemes).
  e1 <- df[df$wave == "E1", ]
  e2 <- df$wave == "E2"
  df$Age[e2] <- e1$Age[match(df$ID[e2], e1$ID)] + 189 / 365.25

  if (anyDuplicated(df[c("ID", "wave")]) > 0) {
    stop(
      "BPIPD-2309: `ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }
  person <- names(bp2309_person_labels())
  df <- df[c("ID", "wave", person[-1], setdiff(names(df), c("wave", person)))]
  bp2309_label(df)
}

#' One row per adolescent: person columns, answered surveys, and each item's mean and count
bp2309_summarise_burst <- function(beeps, wave, items, state, weekday) {
  person <- dplyr::distinct(
    beeps[intersect(names(bp2309_person_labels()), names(beeps))]
  )
  beeps$answered <- beeps[[state]] %in% c("completed", "partially completed")
  # Day of the week 1 = Monday: study day 1, a Tuesday in both bursts, is coded 2 (notification schemes).
  beeps$day_type <- ifelse(beeps[[weekday]] %in% c(6, 7), "we", "wd")

  # Surveys are reports within one burst, so they become summary columns, not rows or waves.
  all_days <- bp2309_summarise(beeps, names(items), "ID")
  by_day_type <- bp2309_summarise(beeps, names(items), c("ID", "day_type")) |>
    tidyr::pivot_wider(
      names_from = "day_type",
      values_from = !dplyr::all_of(c("ID", "day_type")),
      names_glue = "{.value}_{day_type}"
    )

  out <- person |>
    dplyr::left_join(all_days, by = "ID", relationship = "one-to-one") |>
    dplyr::left_join(by_day_type, by = "ID", relationship = "one-to-one")

  # Minutes per sampled hour x hours the survey windows cover / 60, and the 5:2 usual day.
  windows <- bp2309_windows()
  hours <- stats::setNames(windows$hours, windows$day_type)
  minutes <- names(items)[bp2309_is_minutes(names(items))]
  for (item in minutes) {
    wd <- out[[paste0(item, "_mean_wd")]] * hours[["wd"]] / 60
    we <- out[[paste0(item, "_mean_we")]] * hours[["we"]] / 60
    out[[paste0(item, "_hours")]] <- (5 * wd + 2 * we) / 7
    out[[paste0(item, "_hours_wd")]] <- wd
    out[[paste0(item, "_hours_we")]] <- we
  }

  summaries <- c(
    "esm_answered",
    unlist(lapply(names(items), function(item) {
      c(
        paste0(item, c("_mean", "_n")),
        if (item %in% minutes) paste0(item, "_hours")
      )
    }))
  )
  out |>
    dplyr::select(
      dplyr::all_of(names(person)),
      dplyr::all_of(as.vector(rbind(
        summaries,
        paste0(summaries, "_wd"),
        paste0(summaries, "_we")
      )))
    ) |>
    tibble::add_column(wave = wave, .after = "ID")
}

#' Per-survey platform and set totals in minutes, each capped at 60, as the burst's own paper built them
bp2309_add_totals <- function(beeps, platforms) {
  # A platform sum needs all its items; only the 423 surveys kept from the Valkenburg ESM release answer some and not others.
  # A set total needs every platform in the set answered, so an adolescent routed past one has none; items are answered only where asked.
  sets <- c(
    platforms,
    list(
      insta_snap_total = c("instagram_total", "snapchat_total"),
      three_platform_total = names(platforms)
    )
  )
  for (total in names(sets)) {
    beeps[[total]] <- pmin(rowSums(beeps[sets[[total]]]), 60)
  }
  beeps
}

#' Burst 1 items in each platform total, as Valkenburg et al. (2021, J Commun 71(1), pp. 63-64) summed them
bp2309_platforms_e1 <- function() {
  # The paper lists three Snapchat activities, leaving out SNP03E1 and SNA02E1; its counts and shares over 60 reproduce only so.
  list(
    instagram_total = c("INP01E1", "INP02E1", "INA01E1"),
    whatsapp_total = c("WAP01E1", "WAA01E1"),
    snapchat_total = c("SNP01E1", "SNP02E1", "SNA01E1")
  )
}

#' Burst 2 platform items, each already a platform total (Valkenburg et al. 2021, TMB, Method)
bp2309_platforms_e2 <- function() {
  list(
    instagram_total = "ING01E2",
    whatsapp_total = "WAG01E2",
    snapchat_total = "SNG01E2"
  )
}

#' Whether a summarised item is past-hour screen minutes, and so gets hours columns
bp2309_is_minutes <- function(x) {
  # Platform items are IN/WA/SN plus P (passive), A (active) or G (general) in the study's scheme.
  grepl("^(IN|WA|SN)[PAG]0[1-3]E[12]$|_total$", x)
}

#' Hours per day the survey windows cover: from an hour before the first window opens to the end of the last
bp2309_windows <- function() {
  # ESM 1 (study folder) and ESM 2 (osf.io/vkr4u) notification schemes have the same first and last windows.
  # Classes with other break times differ only in the school-day windows (survey times in the data, see notes).
  tibble::tibble(
    day_type = c("wd", "we"),
    hours = c(15, 11.5),
    span = c(
      "6:00-21:00 (first window 7:00-7:30, last 20:30-21:00)",
      "9:30-21:00 (first window 10:30-11:00, last 20:30-21:00)"
    )
  )
}

#' Answered-survey count, and each item's mean and count over answered surveys
bp2309_summarise <- function(beeps, items, by) {
  dplyr::summarise(
    beeps,
    esm_answered = sum(answered),
    dplyr::across(
      dplyr::all_of(items),
      list(
        mean = ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE),
        n = ~ sum(!is.na(.x))
      )
    ),
    .by = dplyr::all_of(by)
  )
}

#' Variable labels for the person-level columns
bp2309_person_labels <- function() {
  # Beyens et al. (2021) codebook, Pouwels et al. (2021) and its authors' script, and the burst 2 OSF codebook (osf.io/gcfm4).
  flag <- "Which social media do you use more than once a week? %s (0 = no, 1 = yes); E1 from survey wave 1 (%s1), E2 from survey wave 2 (%s2); only users got that burst's %s ESM items"
  agree <- "(1 = do not agree at all, 5 = completely agree); E2 rows only"
  c(
    ID = "Participant number (1-388, the same adolescent in both bursts)",
    Class = "Class number at burst 1 (35 classes in the data; numbered differently from ClassNr); E1 rows only",
    ClassNr = "Class number at burst 2 (1-31); E2 rows only",
    Sex = "Sex (0 = boy 'Jongen', 1 = girl 'Meisje'; authors' recode, which left 'Anders' missing); E1 rows only",
    Gender = "Gender (Jongen = boy, Meisje = girl, Anders = other); E2 rows only",
    Grade = "School grade (8 or 9); E1 rows only",
    Age = "Age in years, decimal: E1 from AgeW1 (age at the project's first wave); E2 filled as AgeW1 plus 189 days (0.517 years), the interval between the bursts' start dates",
    ETH01 = "Ethnic background item 1 (Nederland = the Netherlands, Anders = other); E1 rows only",
    ETH03 = "Ethnic background item 3 (Nederlander = Dutch, Anders = other); E1 rows only",
    EduLevel = "Educational track (Vmbo = lower prevocational, Havo = intermediate general secondary, Vwo = academic preparatory); recorded at E1, carried to E2 (same school year)",
    ING01S = sprintf(flag, "Instagram", "ING01S", "ING01S", "Instagram"),
    WAG01S = sprintf(flag, "WhatsApp", "WAG01S", "WAG01S", "WhatsApp"),
    SNG01S = sprintf(flag, "Snapchat", "SNG01S", "SNG01S", "Snapchat"),
    SUSAPE1 = "Suspicious answer patterns in burst 1 (0 = no, 1 = yes); E1 rows only",
    CSE01W = paste(
      "Self-esteem contingency, peer approval: I feel more satisfied about myself when others praise me",
      agree
    ),
    CSE02W = paste(
      "Self-esteem contingency, peer approval: I feel more satisfied about myself when I get a lot of attention from others",
      agree
    ),
    CSE03W = paste(
      "Self-esteem contingency, appearance: I feel more satisfied about myself when I think I am looking good",
      agree
    ),
    CSE04W = paste(
      "Self-esteem contingency, appearance: I feel more satisfied about myself when I think I am attractive",
      agree
    )
  )
}

#' Wording of the burst 1 ESM items
bp2309_items_e1 <- function() {
  # Beyens et al. (2021) codebook; Pouwels et al. (2021, pp. 312-313); Valkenburg et al. (2022, JCMC, Method).
  minutes <- "minutes, 'How much time in the past hour have you spent %s?' (0-60)"
  scale <- "(1 = not at all, 4 = a little, 7 = completely)"
  c(
    INP01E1 = sprintf(minutes, "viewing posts/stories of others on Instagram"),
    INP02E1 = sprintf(minutes, "reading direct messages on Instagram"),
    INA01E1 = sprintf(minutes, "sending direct messages on Instagram"),
    WAP01E1 = sprintf(minutes, "reading messages on WhatsApp"),
    WAA01E1 = sprintf(minutes, "sending messages on WhatsApp"),
    SNP01E1 = sprintf(minutes, "viewing stories of others on Snapchat"),
    SNP02E1 = sprintf(minutes, "viewing snaps of others on Snapchat"),
    SNP03E1 = sprintf(
      minutes,
      "reading direct messages on Snapchat [wording inferred, see notes]"
    ),
    SNA01E1 = sprintf(minutes, "sending snaps on Snapchat"),
    SNA02E1 = sprintf(
      minutes,
      "sending direct messages on Snapchat [wording inferred, see notes]"
    ),
    AWBE1 = paste(
      "affective well-being, 'How happy do you feel right now?'",
      scale
    ),
    FCLE1 = paste(
      "friendship closeness, 'How close to your close friends do you feel right now?'",
      scale
    ),
    ENJ01E1 = paste(
      "enjoyment, 'How much did you like what you did in the past hour?'",
      scale
    ),
    ENV01E1 = paste(
      "envy, 'How jealous of someone did you feel in the past hour?'",
      scale
    ),
    INS01E1 = paste(
      "inspiration, 'How inspired by someone did you feel in the past hour?'",
      scale
    ),
    FCO01E1 = "in touch with close friends in the past hour via Instagram (0 = no, 1 = yes)",
    FCO02E1 = "in touch with close friends in the past hour via WhatsApp (0 = no, 1 = yes)",
    FCO03E1 = "in touch with close friends in the past hour via Snapchat (0 = no, 1 = yes)"
  )
}

#' Wording of the burst 2 ESM items, from the OSF codebook (osf.io/gcfm4)
bp2309_items_e2 <- function() {
  minutes <- "minutes, 'How much time in the past hour have you spent on %s?' (0-60)"
  c(
    SEE01E2 = "self-esteem, 'How satisfied about yourself do you feel right now?' (1 = not at all, 4 = a little, 7 = completely)",
    ING01E2 = sprintf(minutes, "Instagram"),
    WAG01E2 = sprintf(minutes, "WhatsApp"),
    SNG01E2 = sprintf(minutes, "Snapchat"),
    EJM01E2 = "valence of social media experiences, 'How much did you like your experience on social media in the past hour?' (1 = not at all, 4 = a little, 7 = very much)"
  )
}

#' Wording of the per-survey platform and set totals, the same names in both bursts
bp2309_items_totals <- function() {
  total <- "%s minutes in the past hour, per survey, capped at 60 (E1 as Valkenburg et al. 2021, J Commun 71(1), pp. 63-64; E2 as Valkenburg et al. 2021, TMB 2(2), Method): %s"
  c(
    instagram_total = sprintf(
      total,
      "Instagram",
      "E1 INP01E1 (viewing posts/stories) + INP02E1 (reading direct messages) + INA01E1 (sending direct messages), E2 ING01E2 (time on Instagram)"
    ),
    whatsapp_total = sprintf(
      total,
      "WhatsApp",
      "E1 WAP01E1 (reading messages) + WAA01E1 (sending messages), E2 WAG01E2 (time on WhatsApp)"
    ),
    snapchat_total = sprintf(
      total,
      "Snapchat",
      "E1 SNP01E1 (viewing stories) + SNP02E1 (viewing snaps) + SNA01E1 (sending snaps), not SNP03E1 or SNA02E1, E2 SNG01E2 (time on Snapchat)"
    ),
    insta_snap_total = sprintf(
      total,
      "Instagram plus Snapchat",
      "instagram_total + snapchat_total, only on surveys answering both (adolescents asked about both platforms)"
    ),
    three_platform_total = sprintf(
      total,
      "Instagram, WhatsApp and Snapchat",
      "the three platform totals summed, only on surveys answering all three (adolescents asked about all three platforms); the papers' 'time spent with social media' summed the platforms answered"
    )
  )
}

#' Attach variable labels to the person-level and summary columns
bp2309_label <- function(df) {
  suffixes <- c("", "_wd", "_we")
  scopes <- c(
    "all 126 surveys of the burst",
    "the 90 weekday surveys",
    "the 36 weekend surveys"
  )
  item <- c(bp2309_items_e1(), bp2309_items_e2(), bp2309_items_totals())
  labels <- c(
    bp2309_person_labels(),
    wave = "ESM burst (E1 = burst 1, Nov-Dec 2019; E2 = burst 2, June 2020)"
  )
  for (i in seq_along(suffixes)) {
    labels[paste0("esm_answered", suffixes[i])] <- paste0(
      "ESM surveys completed or partially completed, of ",
      scopes[i]
    )
    labels[paste0(names(item), "_mean", suffixes[i])] <- paste0(
      "Mean over answered ESM surveys (",
      scopes[i],
      "): ",
      item
    )
    labels[paste0(names(item), "_n", suffixes[i])] <- paste0(
      "Answered ESM surveys (",
      scopes[i],
      "): ",
      item
    )
  }

  minutes <- item[bp2309_is_minutes(names(item))]
  scheme <- ifelse(
    grepl("E1$", names(minutes)),
    "ESM 1 notification scheme",
    ifelse(
      grepl("E2$", names(minutes)),
      "ESM 2 notification scheme, osf.io/vkr4u",
      "ESM 1 and ESM 2 notification schemes"
    )
  )
  windows <- bp2309_windows()
  covered <- sprintf("%s hours covered, %s", windows$hours, windows$span)
  labels[paste0(names(minutes), "_hours_wd")] <- sprintf(
    "Hours per weekday: mean minutes over answered weekday surveys x %s, %s, / 60: %s",
    covered[1],
    scheme,
    minutes
  )
  labels[paste0(names(minutes), "_hours_we")] <- sprintf(
    "Hours per weekend day: mean minutes over answered weekend surveys x %s, %s, / 60: %s",
    covered[2],
    scheme,
    minutes
  )
  labels[paste0(names(minutes), "_hours")] <- sprintf(
    "Hours per usual day: (5 x weekday hours + 2 x weekend hours) / 7, NA unless both; weekday minutes x %s, weekend minutes x %s, %s, / 60: %s",
    windows$hours[1],
    windows$hours[2],
    scheme,
    minutes
  )
  for (col in names(df)) {
    attr(df[[col]], "label") <- labels[[col]]
  }
  df
}
