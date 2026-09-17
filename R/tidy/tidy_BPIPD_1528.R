#' Tidier for BPIPD-1528 (HBSC)
#'
#' Four open-access HBSC waves, one CSV each, stacked long; the 2010 and 2014
#' files shout some of their column names and the 2014 file names several
#' shared items differently, so both are aligned with the earlier waves first.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per respondent per survey wave
tidy_BPIPD_1528 <- function(raw_dataset, spec) {
  needed <- c("data_2001", "data_2006", "data_2010", "data_2014")
  absent <- setdiff(needed, names(raw_dataset$data))
  if (length(absent) > 0) {
    stop(
      "BPIPD-1528: missing data resource(s): ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }

  data_2001 <- raw_dataset$data$data_2001

  data_2006 <- raw_dataset$data$data_2006

  data_2010 <- raw_dataset$data$data_2010 |>
    dplyr::rename_with(tolower)

  data_2014 <- raw_dataset$data$data_2014 |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(
      surveyyear = hbsc,
      subregion = reg_no,
      yearcollect = year,
      timeexce = timeexe,
      sleepdifficulty = sleepdificulty,
      sampleweights = m137,
      monhtcollect = month
    ) |>
    # 2001-2010 hold one `menarche` item coding 1 as "No, I have not yet begun
    # to menstruate" and the higher codes as the age at menarche (2001
    # codebook: menarche, range 1-17 with 0 declared missing; the 2006 and 2010
    # items run to 18 and 2006 also carries -9/-99, which a mapping of this
    # column would have to clear).
    # 2014 splits it into m136 ("Have you begun to menstruate (have periods)?",
    # whose only answer category is "No") and m136c ("AGE MENARCHE (136, 136a
    # and 136b combined)"), so the two are recombined onto the earlier coding.
    # m136c also carries implausible ages (its range runs from 0.17 years), so
    # only ages above 5 and below 19 are read as an age at menarche.
    dplyr::mutate(
      menarche = dplyr::case_when(
        m136 == 1 ~ 1,
        is.na(m136) & m136c > 5 & m136c < 19 ~ floor(m136c)
      )
    )

  df <- dplyr::bind_rows(data_2001, data_2006, data_2010, data_2014)

  # `uniqueid` is unique within a wave but repeats across waves, so the id is
  # the wave plus the source id. It is missing in some cases and those fall
  # back to a counter over the rows of their wave, tagged `_r`.
  df <- df |>
    dplyr::group_by(surveyyear) |>
    dplyr::mutate(
      participant_id = dplyr::if_else(
        is.na(uniqueid),
        paste0(surveyyear, "_", countryno, "_r", dplyr::row_number()),
        paste0(surveyyear, "_", sprintf("%.0f", uniqueid))
      ),
      .before = 1
    ) |>
    dplyr::ungroup()

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-1528: `participant_id` is not unique.", call. = FALSE)
  }

  # Family Affluence Scale: 2001-2010 carry the four-item FAS II (famcar,
  # bedroom, holidays, computers; 2001 codebook MQ47-MQ50) and 2014 carries the
  # six-item FAS III instead, so the two scales are scored separately and the
  # mapping picks whichever one its wave has. Each composite is ridit-scored
  # within country and wave so affluence is relative to the national sample,
  # matching HBSC's own relative FAS (validated against IRFAS / IRRELFAS_LMH in
  # the 2018 open-access file).
  df <- df |>
    dplyr::mutate(
      fas_sum = fasfamcar +
        fasbedroom +
        fascomputers +
        fasbathroom +
        fasdishwash +
        fasholidays,
      fas2_sum = famcar + bedroom + holidays + computers
    ) |>
    dplyr::group_by(surveyyear, countryno) |>
    dplyr::mutate(
      fas_ridit = ridit_scores(fas_sum),
      fas2_ridit = ridit_scores(fas2_sum)
    ) |>
    dplyr::ungroup()

  df
}
