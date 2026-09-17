bp_data_dir <- function() {
  dotenv::load_dot_env()
  dataset_path <- Sys.getenv("DATASET_PATH", unset = NA)
  if (is.na(dataset_path)) {
    stop(
      "DATASET_PATH environment variable is not set. Please set in .env file."
    )
  }
  dataset_path
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

bp_dataset_template_dir <- function() {
  fs::path(bp_harmonisation_dir(), "templates", "dataset")
}

bp_dataschema_path <- function() {
  fs::path(bp_harmonisation_dir(), "dataschema.csv")
}

bp_shared_lookup_dir <- function() {
  fs::path(bp_harmonisation_dir(), "lookups")
}

bp_harmonisation_dataset_dir <- function(dataset_id) {
  fs::path(bp_harmonisation_dir(), "datasets", paste0("BPIPD-", dataset_id))
}

bp_harmonisation_status_values <- function() {
  # Definitions from Maelstrom Research
  # `compatible`: the study variable can generate the target DataSchema variable
  # with an explicit, scientifically defensible transformation and without
  # meaningful loss of meaning.
  # `partial`: the study only supports part of the target construct, or the
  # harmonised value requires a simplification that should be interpreted with
  # caution.
  # `proximate`: a categorical subtype of partial; use when the study captures
  # a close but not fully equivalent category structure relative to the target
  # DataSchema variable.
  # `inferred`: the value is derived from study metadata or surrounding context
  # rather than being captured directly as a like-for-like study variable.
  # `incompatible`: related information exists, but it cannot be transformed
  # into a scientifically defensible version of the target DataSchema variable.
  # `unavailable`: the study does not collect the required information.
  # `in_progress`: the mapping has not yet been completed and should be skipped
  # by the harmoniser for now.
  c(
    "compatible",
    "partial",
    "proximate",
    "incompatible",
    "unavailable",
    "inferred",
    "in_progress" # default - tells harmoniser to skip
  )
}

bp_system_schema_variables <- function() {
  c("dataset_id", "dataset_name", "st_measure_id")
}

# The screen-time variables that describe a measure rather than quantify it.
# A `measure` block in variables.csv must map all three so the extra rows say
# what instrument and respondent they came from.
bp_measure_metadata_variables <- function() {
  c("st_measure_type", "st_measure_name", "st_responder")
}

# The variables a `measure` block in variables.csv may re-specify: everything
# in the screen_time domain except the system-injected id. Every other schema
# variable is copied from the dataset's primary rows.
bp_measure_scoped_variables <- function(dataschema) {
  if (!"domain" %in% names(dataschema)) {
    return(character(0))
  }
  in_domain <- !is.na(dataschema$domain) & dataschema$domain == "screen_time"
  setdiff(
    as.character(dataschema$variable_name[in_domain]),
    bp_system_schema_variables()
  )
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
      dataset_id = "character",
      dataset_name = "character",
      participant_id = "character",
      wave = "character",
      age_years = "double",
      sex = "character"
    )
  )
}
