#' Tidier for BPIPD-1528 (HBSC)
#'
#' Four open-access HBSC waves, one CSV each, stacked long. Some 2010/2014
#' column names are uppercase and 2014 renames several shared items; both are
#' aligned to the earlier waves first.
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
    # 2001-2010: one `menarche` item, 1 = not yet, higher codes = age (2001
    # codebook range 1-17, 0 = missing; 2006/2010 run to 18, 2006 also has
    # -9/-99, to clear if mapped).
    # 2014 splits it into m136 (begun to menstruate?, only answer category
    # "No") and m136c (age, combines 136/136a/136b); recombined onto the
    # earlier coding here.
    # m136c has implausible ages (down to 0.17), so only 5 < age < 19 is kept.
    dplyr::mutate(
      menarche = dplyr::case_when(
        m136 == 1 ~ 1,
        is.na(m136) & m136c > 5 & m136c < 19 ~ floor(m136c)
      )
    )

  df <- dplyr::bind_rows(data_2001, data_2006, data_2010, data_2014)

  # `uniqueid` repeats across waves, so id = wave + source id. Missing values
  # fall back to a within-wave row counter, tagged `_r`.
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

  # Family Affluence Scale: FAS II (4 items: famcar, bedroom, holidays,
  # computers; 2001 codebook MQ47-MQ50) in 2001-2010, FAS III (6 items) in
  # 2014; scored separately, mapping picks whichever the wave has. Ridit-scored
  # within country and wave, matching HBSC's own relative FAS (validated
  # against IRFAS/IRRELFAS_LMH in the 2018 OA file).
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
