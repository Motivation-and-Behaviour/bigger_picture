#' Tidier for BPIPD-948 (Günüç)
#'
#' One mother-reported cross-sectional file with no participant identifier; the
#' tidier drops repeated submissions and attaches the codebook's variable labels,
#' which the .sav lacks.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child
tidy_BPIPD_948 <- function(raw_dataset, spec) {
  # Rows identical on all 61 columns are one questionnaire submitted more than
  # once; keep the first copy (274 later copies dropped).
  df <- tibble::as_tibble(raw_dataset$data$data) |>
    dplyr::distinct()

  labels <- bp948_labels()
  for (nm in intersect(names(df), names(labels))) {
    attr(df[[nm]], "label") <- unname(labels[[nm]])
  }

  df
}

#' Variable descriptions transcribed from `codebook.docx`, which the .sav lacks
bp948_labels <- function() {
  c(
    stats::setNames(
      paste0("Conners Parent Rating Scale-Revised Short (CPRS-RS) item ", 1:27),
      paste0("h", 1:27)
    ),
    Family = "family structure",
    INCOME = "Monthly income",
    DELIVERY = "Method of delivery?",
    HardBirth = "Is there a history of difficult births?",
    INcubator = "Did your child stay in an incubator after birth?",
    oxygen = "Did your child suffer oxygen deprivation after birth?",
    MILK = "How many months did he/she breastfeed?",
    SMOKE = "Did you smoke during pregnancy?",
    ALCOHOL = "Did you drink alcohol during pregnancy?",
    Drug = "Did you use antidepressant medication during pregnancy?",
    MotherMariageAGE = "What is mother mariage age?",
    SleepProblem = "Did your child experience sleep problems between 0-36 months?",
    anyADHD = "Do you have anyone in your family or close relatives who has been diagnosed with ADHD?",
    FORlanguage = "How often was he/she exposed to digital screens for language skills?",
    FORplay = "How often did he/she use technological devices to play digital games?",
    TOeat = "How often was the child exposed to digital screens to get them to eat? (0-36 months)",
    TOquiet = "How often was the child exposed to digital screens to keep them quiet or distracted? (0-36 months)",
    TABLETPC = "Amount of tablet PC exposure in one day (0-36 months)",
    Subscale1CPRS = "child's oppositional behavior (CPRS-RS subscale total score)",
    Subscale2CPRS = "cognitive problems/inattention (CPRS-RS subscale total score)",
    Subscale3CPRS = "hyperactivity (CPRS-RS subscale total score)",
    Subscale4CPRS = "ADHD index (CPRS-RS subscale total score) (ADHD measure)",
    TotalScore = "CPRS-RS total score"
  )
}
