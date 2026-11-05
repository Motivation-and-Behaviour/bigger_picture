#' Tidier for BPIPD-938 (Kasap)
#'
#' One cross-sectional SPSS extract in which `case_number` already identifies a
#' row, so nothing is joined, stacked or constructed here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent
tidy_BPIPD_938 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$`IAT Prosocial Data`)

  bp938_label_time_columns(df)
}

#' Label the daily-use columns with the units the paper reports
bp938_label_time_columns <- function(df) {
  # The parents' columns are hours/day, not minutes as their names say: only
  # then do their means match Kasap et al. 2023, J Curr Pediatr 21:162-70, p.165.
  labels <- c(
    Total_internet_time_of_the_child_minute = "Adolescent's daily internet use (minutes/day)",
    Total_online_gaming_time_of_the_child_minute = "Adolescent's daily online gaming (minutes/day)",
    Total_internet_time_of_the_mother_minute = "Mother's daily internet use (hours/day)",
    Total_internet_time_of_the_father_minute = "Father's daily internet use (hours/day)"
  )
  for (nm in names(labels)) {
    attr(df[[nm]], "label") <- unname(labels[[nm]])
  }
  df
}
