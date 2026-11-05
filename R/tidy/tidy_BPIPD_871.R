#' Column names for the headerless Mplus data file
bp871_names <- function() {
  # The `NAMES` list of every `.inp` file, spelled as in the RI-CLPM model;
  # the file really does store `SM2` before `SM1`.
  c(
    "ID",
    "age",
    "sex",
    "school",
    "edu",
    "ethnicity",
    "ses",
    "pbt1_1",
    "pbt2_1",
    "pbt3_1",
    "pbt4_1",
    "pbt5_1",
    "expbp1_1",
    "expbp2_1",
    "pstbp1_1",
    "pstbp2_1",
    "recps1_1",
    "recps2_1",
    "recps3_1",
    "givps1_1",
    "givps2_1",
    "givps3_1",
    "expbp1_2",
    "expbp2_2",
    "pstbp1_2",
    "pstbp2_2",
    "recps1_2",
    "recps2_2",
    "recps3_2",
    "givps1_2",
    "givps2_2",
    "givps3_2",
    "pbt1_2",
    "pbt2_2",
    "pbt3_2",
    "pbt4_2",
    "pbt5_2",
    "expbp1_3",
    "expbp2_3",
    "pstbp1_3",
    "pstbp2_3",
    "recps1_3",
    "recps2_3",
    "recps3_3",
    "givps1_3",
    "givps2_3",
    "givps3_3",
    "pbt1_3",
    "pbt2_3",
    "pbt3_3",
    "pbt4_3",
    "pbt5_3",
    "filter",
    "BMI",
    "SM2",
    "SM1",
    "SM3",
    "expbp_1",
    "expbp_2",
    "expbp_3",
    "postbp_1",
    "postbp_2",
    "postbp_3",
    "recpos_1",
    "recpos_2",
    "recpos_3",
    "pstpos_1",
    "pstpos_2",
    "pstpos_3",
    "protbeh_1",
    "protbeh_2",
    "protbeh_3",
    "pbr_1",
    "pbr_2",
    "pbr_3"
  )
}

#' Tidier for BPIPD-871 (Positive Body & Sex Project)
#'
#' One headerless wide Mplus `.dat` file, named from the `.inp` files and
#' pivoted on its `_1`/`_2`/`_3` wave suffix.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant per wave they took part in
tidy_BPIPD_871 <- function(raw_dataset, spec) {
  wide <- tibble::as_tibble(raw_dataset$data[[1]])
  names(wide) <- bp871_names()

  # The social media columns are the only wave-specific ones without the
  # `_` separator before their wave number.
  wide <- dplyr::rename(wide, SM_1 = "SM1", SM_2 = "SM2", SM_3 = "SM3")

  wave_cols <- grep("_[123]$", names(wide), value = TRUE)

  long <- tidyr::pivot_longer(
    wide,
    cols = dplyr::all_of(wave_cols),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*)_([123])$"
  )

  # 999 is the study's missing code, left for the mapping; a wave that is 999
  # on every measured column was not attended.
  measured <- unique(sub("_[123]$", "", wave_cols))
  long <- long[rowSums(!is.na(long[measured]) & long[measured] != 999) > 0, ]

  if (anyDuplicated(long[c("ID", "wave")]) > 0) {
    stop(
      "BPIPD-871: `ID` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  # Age was asked at wave 1 only and the waves are five months apart
  # (Kvardova et al. 2025, Method).
  later <- long$wave != "1" & long$age != 999
  long$age[later] <- long$age[later] +
    (as.integer(long$wave[later]) - 1) * 5 / 12
  attr(long$age, "label") <-
    "Age (years) at wave 1; waves 2-3 filled as wave-1 age + 5 months per wave"

  # `ses` is released once per participant; waves 2-3 are 5 and 10 months
  # after wave 1, within the 12-month SES carry rule.
  attr(long$ses, "label") <-
    "SES (undocumented), released once; carried to waves 2-3 (gaps <= 10 months)"

  # BMI was measured at wave 1 only (Kvardova et al. 2025, section 2.2) and
  # is never carried, so the pivot's copies on waves 2-3 are cleared.
  long$BMI[long$wave != "1"] <- NA
  attr(long$BMI, "label") <-
    "BMI (kg/m2), measured at wave 1 only; NA on waves 2-3"

  # Track was asked at wave 1 (January 2020): wave 2 falls in the same school
  # year, wave 3 in the next, when pupils may have changed stage or left school.
  long$edu[long$wave == "3"] <- NA
  attr(long$edu, "label") <-
    "School track (1 general, 2 practical, 3 technical), asked at wave 1; carried to wave 2 (same school year); NA at wave 3 (next school year)"

  # The released `ID` appears to embed a date of birth, so a generated id, in
  # file order and the same across a participant's waves, replaces it here.
  long$ID <- sprintf("BP871-%04d", match(long$ID, unique(long$ID)))
  attr(long$ID, "label") <-
    "Generated participant id; replaces the released code, which appears to embed a date of birth"

  long
}
