bp_data_dir <- function() {
  "/data"
}

bp_dataset_dir_pattern <- function() {
  # In the format "BPIPD-[ID_number] - [Dataset_name]"
  "^BPIPD-\\d+\\s-\\s.+$"
}

bp_harmonisation_dir <- function() {
  "harmonisation"
}

bp_dataset_specs_dir <- function() {
  fs::path(bp_harmonisation_dir(), "datasets")
}

bp_dataschema_path <- function() {
  fs::path(bp_harmonisation_dir(), "dataschema.csv")
}

bp_harmonisation_dataset_dir <- function(dataset_id) {
  fs::path(bp_harmonisation_dir(), "datasets", paste0("BPIPD-", dataset_id))
}

bp_harmonisation_status_values <- function() {
  c(
    "identical",
    "compatible",
    "proximate",
    "tentative",
    "incompatible",
    "unavailable"
  )
}

bp_layout_step_types <- function() {
  c(
    "use_table",
    "transform",
    "left_join",
    "inner_join",
    "full_join",
    "bind_rows"
  )
}

bp_system_schema_variables <- function() {
  c("dataset_id", "dataset_name")
}

bp_schema <- function() {
  # Minimal default schema contract.
  list(
    required = c(
      "dataset_id",
      "dataset_name",
      "participant_id",
      "wave",
      "age_years",
      "sex"
    ),
    types = c(
      dataset_id      = "character",
      dataset_name    = "character",
      participant_id  = "character",
      wave            = "character",
      age_years       = "double",
      sex             = "character"
    )
  )
}
