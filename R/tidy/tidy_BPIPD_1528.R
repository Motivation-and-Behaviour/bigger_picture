#' Template tidier for pre-harmonisation dataset shaping
#'
#' Use this step for study-specific table assembly before harmonisation, for
#' example joining multiple raw files, binding waves, filtering records, or
#' reshaping raw tables into one canonical analysis tibble.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble to be used as the harmonisation input
tidy_BPIPD_1528 <- function(raw_dataset, spec) {
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
      sleepdifficulty = sleepdificulty
    ) |>
    dplyr::mutate(
      menarche = if_else(is.na(m136) & m136c > 5 & m136c < 19, floor(m136c), NA)
    )

  df <- dplyr::bind_rows(data_2001, data_2006, data_2010, data_2014)

  # The built-in ID is not guaranteed to be unique, so we create a new one
  df <- df %>%
    dplyr::group_by(surveyyear, countryno) %>%
    mutate(
      partid = paste0(
        surveyyear,
        countryno,
        sprintf("%05d", dplyr::row_number())
      )
    ) %>%
    dplyr::ungroup()

  df
}
