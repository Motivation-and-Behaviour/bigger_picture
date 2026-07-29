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
tidy_BPIPD_232 <- function(raw_dataset, spec) {
  dfs <- raw_dataset$data

  df_2016 <- dfs$nsch_2016_data |>
    mutate(FORMTYPE = readr::parse_number(FORMTYPE)) |>
    rename_with(~ sub("_16$", "_nschyr", .x), ends_with("_16")) |>
    rename_with(tolower)
  df_2017 <- dfs$nsch_2017_data |>
    rename_with(~ sub("_17$", "_nschyr", .x), ends_with("_17")) |>
    rename_with(tolower)
  df_2018 <- dfs$nsch_2018_data |>
    rename_with(~ sub("_18$", "_nschyr", .x), ends_with("_18")) |>
    rename_with(tolower)
  df_2019 <- dfs$nsch_2019_data |>
    rename_with(~ sub("_19$", "_nschyr", .x), ends_with("_19")) |>
    rename_with(tolower)
  df_2020 <- dfs$nsch_2020_data |>
    rename_with(~ sub("_20$", "_nschyr", .x), ends_with("_20")) |>
    rename_with(tolower)
  df_2021 <- dfs$nsch_2021_data |>
    rename_with(~ sub("_21$", "_nschyr", .x), ends_with("_21")) |>
    rename_with(tolower)
  df_2022 <- dfs$nsch_2022_data |>
    rename_with(~ sub("_22$", "_nschyr", .x), ends_with("_22")) |>
    rename_with(tolower)
  df_2023 <- dfs$nsch_2023_data |>
    rename_with(~ sub("_23$", "_nschyr", .x), ends_with("_23")) |>
    rename_with(tolower)

  bind_rows(
    df_2016,
    df_2017,
    df_2018,
    df_2019,
    df_2020,
    df_2021,
    df_2022,
    df_2023
  )
}
