#' T1/T2 column pairs for the repeated measures in BPIPD-728
bp728_measures <- function() {
  tibble::tribble(
    ~name         , ~t1             , ~t2             ,
    "POG"         , "POGPRESM"      , "POGPOSTM"      ,
    "GRVIDEO"     , "GRVIDEOPRE"    , "GRVIDEOPOST"   ,
    "ANS"         , "ANSSM"         , "ANS2M"         ,
    "DEP"         , "DEPSM"         , "DEP2M"         ,
    "HOS"         , "HOSPRETSM"     , "HOSPOST2M"     ,
    "MOT"         , "MOTSM"         , "MOT2M"         ,
    "INAT"        , "INATSM"        , "INAT2M"        ,
    "NPLA"        , "NPLASM"        , "NPLA2M"        ,
    "EXTRAV"      , "EXTRAVSM"      , "EXTRAV2M"      ,
    "CORDIALID"   , "CORDIALIDSM"   , "CORDIALID2M"   ,
    "RESPONSAB"   , "RESPONSABSM"   , "RESPONSAB2M"   ,
    "ESTABILIDAD" , "ESTABILIDADSM" , "ESTABILIDAD2M" ,
    "APERTURA"    , "APERTURASM"    , "APERTURA2M"    ,
    "ADAP"        , "ADAPSM"        , "ADAP2M"        ,
    "DESA"        , "DESASM"        , "DESA2M"        ,
    "ASOC"        , "ASOCSM"        , "ASOC2M"        ,
    "RESOL"       , "RESOLSM"       , "RESOL2M"       ,
    "AFEC"        , "AFECSM"        , "AFEC2M"
  )
}

#' Variable labels for BPIPD-728, from the variable docx and Marrero et al. (2021)
bp728_labels <- function() {
  c(
    num = "Participant number",
    wave = "Timepoint (T1 = September 2016; T2, June 2017, dropped: no screen item)",
    GEN = "Gender (0/1; the codes are not documented)",
    EDAD = "Age in years, reported at T1 only",
    COL = "School type (0 = public, 1 = private)",
    TVIDSEM_1 = paste(
      "Daily minutes playing video games on weekdays, T1;",
      "SPSS series-mean imputed, labelled SMEAN(TVIDSEM)"
    ),
    TVIDFIN_1 = paste(
      "Daily minutes playing video games at weekends, T1;",
      "SPSS series-mean imputed, labelled SMEAN(TVIDFIN)"
    ),
    POG = "PVP problematic video-game playing, criteria met (0-9)",
    GRVIDEO = "Gaming disorder case (PVP >= 5 criteria)",
    ANS = "HADS anxiety, 7 items scored 0-3 (0-21)",
    DEP = "HADS depression, 7 items scored 0-3 (0-21)",
    HOS = "SCL-90-R hostility, 8 items scored 0-4 (0-32)",
    MOT = "BIS-15 motor impulsivity, 5 items scored 1-4 (5-20)",
    INAT = "BIS-15 attentional impulsivity, 5 items scored 1-4 (5-20)",
    NPLA = "BIS-15 non-planning impulsivity, 5 items scored 1-4 (5-20)",
    EXTRAV = "TIPI extraversion, 2 items scored 1-7 (2-14)",
    CORDIALID = "TIPI agreeableness, 2 items scored 1-7 (2-14)",
    RESPONSAB = "TIPI conscientiousness, 2 items scored 1-7 (2-14)",
    ESTABILIDAD = "TIPI emotional stability, 2 items scored 1-7 (2-14)",
    APERTURA = "TIPI openness to experience, 2 items scored 1-7 (2-14)",
    ADAP = "Family APGAR adaptability, 1 item scored 0-2",
    DESA = "Family APGAR growth, 1 item scored 0-2",
    ASOC = "Family APGAR partnership, 1 item scored 0-2",
    RESOL = "Family APGAR resolve, 1 item scored 0-2",
    AFEC = "Family APGAR affection, 1 item scored 0-2"
  )
}

#' Tidier for BPIPD-728 (Marrero)
#'
#' One wide SPSS file holds T1 (`SM`/`PRE`) and T2 (`2M`/`POST`); the
#' timepoints are stacked and T2, which fielded no screen item, is dropped.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent at T1 (key `num` + `wave`)
tidy_BPIPD_728 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data[[1]])
  measures <- bp728_measures()

  # Gender, age and school type were recorded once, at T1.
  fixed <- c(num = "num", GEN = "GEN", EDAD = "EDAD", COL = "COL")

  t1 <- dplyr::select(
    df,
    dplyr::all_of(c(
      fixed,
      c(TVIDSEM_1 = "TVIDSEM_1", TVIDFIN_1 = "TVIDFIN_1"),
      stats::setNames(measures$t1, measures$name)
    ))
  )
  t2 <- dplyr::select(
    df,
    dplyr::all_of(c(fixed, stats::setNames(measures$t2, measures$name)))
  )

  out <- dplyr::bind_rows(T1 = t1, T2 = t2, .id = "wave") |>
    dplyr::relocate(dplyr::all_of(c("num", "wave")))

  # Drop-wave rule: the video-game items were asked at T1 only (variable docx).
  answered <- !is.na(out$TVIDSEM_1) | !is.na(out$TVIDFIN_1)
  out <- out[out$wave %in% out$wave[answered], , drop = FALSE]

  if (anyDuplicated(out[c("num", "wave")]) > 0) {
    stop(
      "BPIPD-728: `num` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # Value labels as given by `Data_ Name Variables.docx`.
  out$COL <- haven::labelled(out$COL, c(Public = 0, Private = 1))
  out$GRVIDEO <- haven::labelled(
    out$GRVIDEO,
    c("Without gaming disorder" = 0, "With gaming disorder" = 1)
  )

  labels <- bp728_labels()
  for (nm in names(labels)) {
    attr(out[[nm]], "label") <- labels[[nm]]
  }
  out
}
