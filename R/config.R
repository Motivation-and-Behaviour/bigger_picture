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

# The private funder report: its own repository, cloned or linked in here.
bp_private_report_dir <- function() {
  "DoEreport"
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

# Analysis registries (ANALYSIS_PLAN.md, Step 1): spec CSVs under analysis/.
bp_analysis_dir <- function() {
  "analysis"
}

bp_outcome_tree_path <- function() {
  fs::path(bp_analysis_dir(), "outcome_tree.csv")
}

bp_outcome_rows_path <- function() {
  fs::path(bp_analysis_dir(), "outcome_rows.csv")
}

bp_exposures_path <- function() {
  fs::path(bp_analysis_dir(), "exposures.csv")
}

bp_invariants_path <- function() {
  fs::path(bp_analysis_dir(), "invariants.csv")
}

bp_analysis_datasets_path <- function() {
  fs::path(bp_analysis_dir(), "datasets.csv")
}

bp_derivations_path <- function() {
  fs::path(bp_analysis_dir(), "derivations.csv")
}

bp_deviations_path <- function() {
  fs::path(bp_analysis_dir(), "deviations.csv")
}

# Schema domains whose every row must appear in outcome_rows.csv. Health rows
# are registered too, so negative controls and mediators carry a role.
bp_outcome_registry_domains <- function() {
  c(
    "academic",
    "behaviour",
    "cognition",
    "mental_health",
    "wellbeing",
    "health"
  )
}

# The vocabularies of the outcome registry. `node` takes a tree node, or one of
# the placeholders: `unassigned` before the freeze, `post_freeze` after it.
bp_outcome_registry_values <- function() {
  list(
    level = c("L0", "L1"),
    prereg_status = c("named", "expected", "added"),
    role = c("outcome", "measure", "negative_control", "mediator", "covariate"),
    informant = c("self", "parent", "teacher", "task", "mixed"),
    score_type = c("continuous", "ordinal", "binary", "nominal", "diagnosis"),
    direction = c("higher_better", "higher_worse", "none"),
    use = c("core", "report", "archive"),
    node_placeholder = c("unassigned", "post_freeze"),
    # Besides these, any `*_measure` schema row can be a standardisation key.
    standardise_key = c("dataset", "wave", "country", "age_band", "sex")
  )
}

bp_exposure_registry_values <- function() {
  list(
    content_class = c(
      "educational",
      "non_interactive",
      "interactive",
      "social_media"
    ),
    axis = c(
      "activity",
      "device",
      "subset",
      "context",
      "aggregate",
      "metadata"
    ),
    context = c("any", "school"),
    day = c("usual", "weekday", "weekend"),
    unit = c("hours", "index")
  )
}

bp_outcome_tree_columns <- function() {
  c("node", "parent", "label", "level", "prereg_status", "headline", "notes")
}

bp_outcome_rows_columns <- function() {
  c(
    "variable_name",
    "node",
    "informant",
    "role",
    "score_type",
    "direction",
    "standardise_by",
    "part_of",
    "synthesis",
    "use",
    "prereg_family",
    "schema_domain",
    "frozen",
    "notes"
  )
}

bp_exposures_columns <- function() {
  c(
    "exposure",
    "variable_name",
    "content_class",
    "axis",
    "context",
    "day",
    "unit",
    "include",
    "headline",
    "notes"
  )
}

bp_invariants_columns <- function() {
  c("variable_name", "notes")
}

bp_analysis_datasets_columns <- function() {
  c(
    "dataset_id",
    "baseline_wave",
    "team_contributed",
    "signed_off",
    "overlap_group",
    "risk_of_bias",
    "notes"
  )
}

bp_derivations_columns <- function() {
  c("target", "sources", "method", "weights", "dataset_id", "notes")
}

# The deviations ledger: where the analysis departs from the preregistration.
bp_deviations_columns <- function() {
  c(
    "id",
    "date_logged",
    "status",
    "prereg_plan",
    "deviation",
    "reason",
    "validity",
    "severity",
    "assessment",
    "decision_record"
  )
}

bp_deviation_statuses <- function() {
  c("made", "planned")
}

bp_deviation_ratings <- function() {
  c("improves", "no_change", "reduces", "unclear")
}

bp_derivation_methods <- function() {
  c("sum", "mean", "weighted_mean")
}
