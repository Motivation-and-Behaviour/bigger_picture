#' Tidier for BPIPD-278 (Panel Study on Korean Children)
#'
#' Annual CSVs 2008-2023, plus in-depth subsample files in 2009, 2013 and 2020;
#' the survey year is stripped from every name and the codebook labels all.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave attended, 2010-2023
tidy_BPIPD_278 <- function(raw_dataset, spec) {
  sources <- bp278_sources()
  waves <- Map(
    function(files, year) bp278_wave(raw_dataset$data[files], year),
    sources,
    as.integer(names(sources))
  )
  df <- dplyr::bind_rows(bp278_align_types(lapply(waves, `[[`, "data")))
  map <- dplyr::bind_rows(lapply(waves, `[[`, "map"))

  # Carried before 2008-2009 go, so those waves can still be the source.
  df <- bp278_carry(df)

  # Drop-wave rule: 2008 and 2009 field no screen item; the first, in 2010, is
  # the mother's report of weekday minutes of TV/video and of games.
  df <- df[df$survey_year >= 2010L, , drop = FALSE]
  # Items fielded only in 2008-2009, or never answered, are left empty by that.
  df <- df[vapply(df, function(x) any(!is.na(x)), logical(1))]

  codebook <- bp278_read_codebook(raw_dataset$codebook$pskc_codebook)
  df <- bp278_label(df, map[map$year >= 2010L, ], codebook)
  for (col in names(bp278_carried())) {
    attr(df[[col]], "label") <- paste0(
      attr(df[[col]], "label"),
      " {tidier: ",
      bp278_carried()[[col]],
      "}"
    )
  }

  baseline <- bp278_baseline(raw_dataset$data, codebook)
  df <- dplyr::left_join(
    df,
    baseline,
    by = "N_ID",
    relationship = "many-to-one"
  )

  if (anyDuplicated(df[c("N_ID", "wave")]) > 0) {
    stop(
      "BPIPD-278: `N_ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  attr(df$N_ID, "label") <- "PSKC panel child identifier, stable across waves"
  attr(df$wave, "label") <- "PSKC wave number (1 = 2008 ... 16 = 2023)"
  attr(df$survey_year, "label") <- "Survey year of the wave file"
  attr(df$interview_date, "label") <-
    "Caregiver (CAPI) interview date, from DHu..int003; not released from 2018"
  df
}

#' The data resources holding each survey year, main file first
bp278_sources <- function() {
  list(
    "2008" = "pskc_wave1_2008",
    "2009" = c("pskc_wave2_2009_part1", "pskc_wave2_2009_part2"),
    "2010" = "pskc_wave3_2010",
    "2011" = "pskc_wave4_2011",
    "2012" = "pskc_wave5_2012",
    "2013" = c("pskc_wave6_2013_part1", "pskc_wave6_2013_part2"),
    "2014" = "pskc_wave7_2014",
    "2015" = "pskc_wave8_2015",
    "2016" = "pskc_wave9_2016",
    "2017" = "pskc_wave10_2017",
    "2018" = "pskc_wave11_2018",
    "2019" = "pskc_wave12_2019",
    "2020" = c("pskc_wave13_2020_part2", "pskc_wave13_2020_part1"),
    "2021" = "pskc_wave14_2021",
    "2022" = "pskc_wave15_2022",
    "2023" = "pskc_wave16_2023"
  )
}

#' Codebook categories with no dataschema use, left out of the very wide release
bp278_excluded_categories <- function() {
  c(
    chc = "childcare services: day-care centre",
    chg = "childcare services",
    chl = "childcare services: plans for institution use",
    chm = "childcare services: main substitute caregiver",
    chp = "childcare services: individual substitute care",
    cht = "childcare services: other",
    sss = "childcare support policies",
    plc = "education policy",
    vct = "use of birth and childcare support schemes",
    edu = "school support received and satisfaction",
    ppe = "parent participation in childcare-institution programmes",
    crb = "parenting beliefs",
    crk = "parenting knowledge",
    cre = "expectations of the child's future",
    voc = "value of children",
    vgr = "gender-role values",
    wfb = "work-family balance",
    mrs = "marital satisfaction",
    mrc = "marital conflict",
    cfl = "marital conflict",
    ppc = "husband's participation in parenting",
    brp = "plans for further births",
    tbh = "mother's behaviour type",
    tab = "mother's behaviour type",
    der = "father's own emotion dysregulation (in-depth)",
    qst = "attachment Q-set",
    pst = "parent's Personality Assessment Inventory",
    fie = "family life events",
    tpc = "teacher-parent communication",
    sav = "household savings",
    rmd = "home remodelling",
    mld = "mould in the home",
    pet = "pets",
    env = "indoor environment",
    cho = "household chores",
    enn = "family meal frequency",
    adx = "atopic dermatitis",
    arx = "allergic rhinitis",
    fax = "food allergy",
    ana = "anaphylaxis",
    phd = "handedness",
    eat = "eating habits",
    dfc = "toileting and washing",
    pap = "school fitness test (PAPS)",
    tcb = "teacher's perceptions and teaching efficacy",
    tdm = "teacher's own characteristics",
    ted = "teacher's class activities",
    tdp = "teacher's own traits",
    tcn = "teacher-child interaction quality",
    iev = "school or centre environment",
    soc = "school climate",
    jdl = "school-level delinquency",
    cov = "COVID-19 policy satisfaction",
    car = "career maturity",
    cmt = "community spirit",
    saf = "perceived safety",
    fut = "perceptions of the future",
    gri = "grit",
    cla = "subject preference",
    pri = "perceived private education",
    com = "computer skills",
    aut = "autonomy"
  )
}

#' Parent-questionnaire items renamed, by year, to their caregiver-questionnaire stem
bp278_moved_items <- function() {
  # Each pair shares its codes, so a carried construct has one column.
  parents <- c(
    EMtdmg014 = "DMtdmg014",
    FFtdmg014 = "DFtdmg014",
    EMtdmg003 = "DMtdmg003",
    FFtdmg003 = "DFtdmg003"
  )
  # In 2015 the sons and daughters counts sit in the household roster, and
  # "other disorder" takes the letter it has from 2016.
  y2015 <- c(
    DHudmg019a = "DChdmg002a",
    DHudmg019b = "DChdmg002b",
    DChhlt032g = "DChhlt032j"
  )
  list(
    "2008" = parents,
    "2015" = y2015,
    "2018" = parents,
    "2023" = c(EMtjcg003 = "DMtjcg003")
  )
}

#' Stems split by year: a change of unit, or the stem reused for another item
bp278_splits <- function() {
  # From each year's codebook wording: a unit change takes `_min` or `_pct`.
  # A stem reused for another item takes, in its odd years, their first year.
  tibble::tribble(
    ~pattern                                      , ~years                    , ~suffix ,
    "^DChdlc0(0[7-9]|10|13)$"                     , list(2011L)               , "_min"  ,
    "^DChslp019$"                                 , list(2011L)               , "_min"  ,
    "^DMtjce026[ab]$"                             , list(c(2011:2013, 2015L)) , "_min"  ,
    "^DChmid015[a-e]$"                            , list(2016L)               , "_pct"  ,
    "^ICh(cbs|cbt|cbp)0(0[457-9]|1[0-9]|2[0-4])$" , list(2021:2023)           , "_2021" ,
    "^ICh(trs|trt|trp)0(05|09|1[0-35-9]|2[0-7])$" , list(2021:2023)           , "_2021" ,
    "^IChcbl1(1[4-9]|[2-5][0-9])[a-z]?$"          , list(2021:2023)           , "_2021" ,
    "^IChtrf1(1[4-9]|[2-9][0-9])[a-z]?$"          , list(2021:2023)           , "_2021" ,
    "^JChsel"                                     , list(c(2020L, 2022L))     , "_2020" ,
    "^DChhlt0(26|3[0-9]|4[0-4])$"                 , list(2011:2013)           , "_2011" ,
    "^DChhlt04[57]$"                              , list(2012L)               , "_2011" ,
    "^EChhlt040$"                                 , list(2018L)               , "_2018" ,
    "^DChhlt03[13][a-z]$"                         , list(2014L)               , "_2014" ,
    "^DChhlt03[1235][a-z]$"                       , list(2015:2016)           , "_2015" ,
    "^DChhlt029[ab]$"                             , list(2015:2016)           , "_2015" ,
    "^DChhlt04(5[bc]|6)$"                         , list(2020:2022)           , "_2020" ,
    "^DChslp02[23][ab]$"                          , list(2011:2014)           , "_2011" ,
    "^DChslp022[cd]$"                             , list(2018L)               , "_2018" ,
    "^DChslp023[ab]$"                             , list(2015:2017)           , "_2015" ,
    "^DChslp024[abc]$"                            , list(2019L)               , "_2019" ,
    "^JChscl00[12]$"                              , list(2019L)               , "_2019" ,
    "^JChscl016$"                                 , list(2016:2017)           , "_2016" ,
    "^JChscl026$"                                 , list(2018L)               , "_2018" ,
    "^LInscm003$"                                 , list(2018L)               , "_2018" ,
    "^LInscm00[457]$"                             , list(2015L)               , "_2015" ,
    "^LInscm004$"                                 , list(2018L)               , "_2018" ,
    "^KChdrg007$"                                 , list(2013L)               , "_2013" ,
    "^DCheds209[a-h]$"                            , list(2017L)               , "_2017" ,
    "^DMtjce027$"                                 , list(2022L)               , "_2022" ,
    "^FFtjce024$"                                 , list(2017:2018)           , "_2017"
  )
}

#' Columns carried within child, with the rule appended to their labels
bp278_carried <- function() {
  education <- "parent education carried from the earliest wave recording it"
  gap <- "carried from the nearest interview within 12 months"
  c(
    DMtdmg014 = education,
    DFtdmg014 = education,
    DHuses006 = paste("household income", gap),
    DHuses007 = paste("household income band", gap),
    DHudmg019 = paste("household size", gap),
    DHudmg020 = paste("household composition", gap),
    DMtdmg003 = paste("mother's marital status", gap),
    DFtdmg003 = paste("father's marital status", gap),
    DMtjcg003 = paste("mother's employment status", gap),
    DChdmg002a = paste("number of sons", gap),
    DChdmg002b = paste("number of daughters", gap)
  )
}

#' Whether each child took part in the wave, from the study's participation flags
bp278_attended <- function(tbl, year) {
  flag <- function(prefix) tbl[[paste0(prefix, substr(year, 3, 4), "int001")]]
  if (year <= 2015L) {
    return(!flag("DHu") %in% 9)
  }
  if (year == 2016L) {
    return(!flag("JCh") %in% 9)
  }
  if (year == 2018L) {
    return(!flag("DHu") %in% 2)
  }
  # From 2017 the child-behaviour (CBCL/TRF) questionnaire has no flag of its
  # own, and two children per year answered only that.
  behaviour <- grep(
    "^ICh[0-9]{2}(?!int)",
    names(tbl),
    value = TRUE,
    perl = TRUE
  )
  answered <- rowSums(!is.na(tbl[behaviour])) > 0
  if (year %in% c(2017L, 2019L, 2020L)) {
    return(flag("W") %in% 1 | answered)
  }
  # 2021-2023 flag each questionnaire but have no wave-level code.
  flag("DHu") %in%
    1 |
    flag("JCh") %in% 1 |
    flag("LIn") %in% 1 |
    flag("EMt") %in% 2:3 |
    flag("FFt") %in% 2:3 |
    answered
}

#' A column's stem: its name without the survey year, after any moves or splits
bp278_stem <- function(source, year) {
  stem <- sub("^([A-L][A-Z][a-z]|W)[0-9]{2}", "\\1", source)
  moved <- bp278_moved_items()[[as.character(year)]]
  stem[stem %in% names(moved)] <- moved[stem[stem %in% names(moved)]]
  splits <- bp278_splits()
  in_year <- vapply(splits$years, function(y) year %in% unlist(y), logical(1))
  for (i in which(in_year)) {
    hit <- grepl(splits$pattern[i], stem)
    stem[hit] <- paste0(stem[hit], splits$suffix[i])
  }
  stem
}

#' One survey year: its files joined, attended rows, kept columns as stems
bp278_wave <- function(tables, year) {
  tbl <- Reduce(
    function(x, y) {
      dplyr::left_join(x, y, by = "N_ID", relationship = "one-to-one")
    },
    tables
  )
  tbl <- tbl[bp278_attended(tbl, year), , drop = FALSE]

  source <- setdiff(names(tbl), "N_ID")
  stem <- bp278_stem(source, year)
  category <- substr(sub("^([A-L][A-Z][a-z]|W)", "", stem), 1, 3)
  keep <- !category %in% names(bp278_excluded_categories())

  data <- tbl[c("N_ID", source[keep])]
  names(data) <- c("N_ID", stem[keep])
  data <- tibble::add_column(
    data,
    wave = year - 2007L,
    survey_year = year,
    interview_date = bp278_interview_date(tbl, year),
    .after = "N_ID"
  )
  map <- tibble::tibble(column = stem[keep], year = year, source = source[keep])
  list(data = data, map = map)
}

#' The caregiver interview date, whose encoding changes by year
bp278_interview_date <- function(tbl, year) {
  if (year <= 2015L) {
    # SPSS date: seconds since 14 October 1582.
    seconds <- tbl[[paste0("DHu", substr(year, 3, 4), "int003")]]
    return(as.Date(seconds / 86400, origin = "1582-10-14"))
  }
  if (year == 2016L) {
    mmdd <- tbl$DHu16int003
    return(as.Date(ISOdate(year, mmdd %/% 100, mmdd %% 100)))
  }
  if (year == 2017L) {
    # No year is recorded and fieldwork ran from July, so the eight January
    # dates may fall in 2018; they are left missing.
    month <- tbl$DHu17int003b
    month[month < 7] <- NA
    return(as.Date(ISOdate(year, month, tbl$DHu17int003c)))
  }
  rep(as.Date(NA), nrow(tbl))
}

#' Make a stem numeric across waves when all its text values are numbers, else text
bp278_align_types <- function(tables) {
  classes <- dplyr::bind_rows(lapply(tables, function(tbl) {
    tibble::tibble(
      column = names(tbl),
      character = vapply(tbl, is.character, logical(1)),
      numeric = vapply(tbl, is.numeric, logical(1))
    )
  }))
  mixed <- intersect(
    classes$column[classes$character],
    classes$column[classes$numeric]
  )
  lapply(tables, function(tbl) {
    for (col in intersect(mixed, names(tbl))) {
      text <- unlist(lapply(tables, function(t) {
        if (is.character(t[[col]])) t[[col]]
      }))
      if (all(grepl("^\\s*-?[0-9.]+\\s*$", text[!is.na(text)]))) {
        tbl[[col]] <- as.numeric(tbl[[col]])
      } else {
        tbl[[col]] <- as.character(tbl[[col]])
      }
    }
    tbl
  })
}

#' Recorded values only: the release's don't-know and no-response codes as NA
bp278_recorded <- function(x) {
  x[x %in% c(88888888, 99999999)] <- NA
  x
}

#' Fill a child's missing values from the nearest interview within `max_gap` months
bp278_carry_gap <- function(x, id, month, max_gap = 12) {
  source <- bp278_recorded(x)
  out <- x
  for (child in unique(id[is.na(source) & !is.na(month)])) {
    rows <- which(id == child)
    for (i in rows[is.na(source[rows]) & !is.na(month[rows])]) {
      gap <- abs(month[rows] - month[i])
      gap[is.na(source[rows])] <- NA
      near <- which(!is.na(gap) & gap <= max_gap)
      if (length(near) > 0) {
        out[i] <- source[rows][near[which.min(gap[near])]]
      }
    }
  }
  out
}

#' Carry parent education, household SES, family structure and maternal employment
bp278_carry <- function(df) {
  rules <- bp278_carried()
  month <- 12 *
    as.integer(format(df$interview_date, "%Y")) +
    as.integer(format(df$interview_date, "%m"))
  # A don't-know or no-response code is filled like a missing cell, and kept
  # when there is nothing to carry.
  for (col in names(rules)) {
    x <- df[[col]]
    if (grepl("^parent education", rules[[col]])) {
      fill <- carry_within(bp278_recorded(x), df$N_ID, df$survey_year)
      empty <- is.na(bp278_recorded(x)) & !is.na(fill)
      x[empty] <- fill[empty]
    } else {
      x <- bp278_carry_gap(x, df$N_ID, month)
    }
    df[[col]] <- x
  }
  df
}

#' The integrated codebook sheet, one row per code with its item filled down
bp278_read_codebook <- function(path) {
  codebook <- readxl::read_excel(path, sheet = 2, col_types = "text")
  names(codebook) <- c(
    "year",
    "var",
    "target",
    "label",
    "type",
    "t_code",
    "s_code",
    "value",
    "value_label"
  )
  codebook <- tidyr::fill(codebook, "year", "var")
  codebook$year <- as.integer(substr(codebook$year, 1, 4))
  codebook
}

#' Year sets written as ranges, e.g. 2010-2014, 2016
bp278_years <- function(years) {
  years <- sort(unique(years))
  runs <- split(years, cumsum(c(1, diff(years) != 1)))
  paste(
    vapply(
      runs,
      function(r) {
        if (length(r) == 1L) {
          as.character(r)
        } else {
          paste0(r[1], "-", r[length(r)])
        }
      },
      character(1)
    ),
    collapse = ", "
  )
}

#' Each distinct text with the years it was used, e.g. "a [2010-2014]; b [2015]"
bp278_by_year <- function(text, years, sep) {
  groups <- split(years, text)
  groups <- groups[order(vapply(groups, min, numeric(1)))]
  if (length(groups) == 1L) {
    return(paste0(names(groups), " [", bp278_years(groups[[1]]), "]"))
  }
  paste(
    paste0(names(groups), " [", vapply(groups, bp278_years, character(1)), "]"),
    collapse = sep
  )
}

#' Label each column with each year's respondent, wording and code meanings
bp278_label <- function(df, map, codebook) {
  # The codebook spells a few names in another case (`Ich17cbl` for `ICh17cbl`).
  map$key <- tolower(map$source)
  codebook$key <- tolower(codebook$var)
  entries <- dplyr::inner_join(
    map,
    codebook,
    by = c("year", "key"),
    relationship = "one-to-many"
  )
  items <- entries[!duplicated(entries[c("column", "year")]), ]
  # Question numbers and "wN generated" tags change every year; normalised so
  # a repeated item reads as one wording.
  wording <- gsub("\\s+", " ", trimws(items$label))
  wording <- gsub("[“”']", "\"", wording)
  wording <- sub("^[A-Z][0-9][0-9A-Za-z_-]*\\)?\\.\\s*", "", wording)
  wording <- sub("^[wW][0-9]+ \\S+\\.\\s*", "[derived] ", wording)
  items$text <- paste0("(", items$target, ") ", wording)
  codes <- entries[!is.na(entries$value), ]
  codes$value_label <- gsub("\\s+", " ", trimws(codes$value_label))

  for (col in setdiff(intersect(names(df), map$column), items$column)) {
    source <- map[map$column == col, ]
    attr(df[[col]], "label") <- paste0(
      "Not in the integrated codebook: ",
      paste(source$source, collapse = ", ")
    )
  }

  for (col in intersect(names(df), unique(items$column))) {
    item <- items[items$column == col, ]
    label <- bp278_by_year(item$text, item$year, "; ")
    code <- codes[codes$column == col, ]
    x <- df[[col]]
    if (nrow(code) == 0L || !is.numeric(x)) {
      attr(x, "label") <- label
      df[[col]] <- x
      next
    }
    values <- split(code, code$value)
    labels <- vapply(
      values,
      function(v) {
        if (length(unique(v$value_label)) == 1L) {
          v$value_label[1]
        } else {
          bp278_by_year(v$value_label, v$year, " / ")
        }
      },
      character(1)
    )
    df[[col]] <- haven::labelled(
      as.numeric(x),
      labels = stats::setNames(as.numeric(names(values)), labels),
      label = label
    )
  }
  df
}

#' Birth and infancy items from the dropped 2008-2009 waves, one row per child
bp278_baseline <- function(data, codebook) {
  pick <- function(tbl, year) {
    tbl <- tbl[bp278_attended(tbl, year), , drop = FALSE]
    cols <- grep(
      "^[A-L][A-Z][a-z][0-9]{2}(pnb|fed|hlt|smk|drn)",
      names(tbl),
      value = TRUE
    )
    tbl[c("N_ID", cols)]
  }
  w1 <- pick(data$pskc_wave1_2008, 2008L)
  w2 <- pick(data$pskc_wave2_2009_part1, 2009L)
  baseline <- dplyr::full_join(w1, w2, by = "N_ID", relationship = "one-to-one")
  map <- dplyr::bind_rows(
    tibble::tibble(
      column = names(w1)[-1],
      year = 2008L,
      source = names(w1)[-1]
    ),
    tibble::tibble(column = names(w2)[-1], year = 2009L, source = names(w2)[-1])
  )
  baseline <- baseline[c("N_ID", map$column)]
  bp278_label(baseline, map, codebook)
}
