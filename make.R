# Build the pipeline in sections, so that the work that loads whole datasets
# and the work that fits models never run at the same time.
#
#   Rscript make.R
#
# A plain `targets::tar_make()` builds the same targets and is safe, but
# keeps the model targets to four workers. Here the models section gets
# more, because nothing in it loads a harmonised dataset (see
# `bp_model_workers()` and `bp_build_sections()` in R/config.R).
#
# Each section is the per-dataset targets whose names start with its
# prefixes, and whatever they depend on; the last call builds the rest.

targets::tar_source()

sections <- bp_build_sections()
section_names <- function(prefixes) {
  paste0("^(", paste(prefixes, collapse = "|"), ")_BPIPD_")
}

message("== Section 1 of 3: datasets")
targets::tar_make(names = tidyselect::matches(section_names(sections$datasets)))

message("== Section 2 of 3: models")
Sys.setenv(BP_MODEL_WORKERS = bp_model_workers_wide())
targets::tar_make(names = tidyselect::matches(section_names(sections$models)))
Sys.unsetenv("BP_MODEL_WORKERS")

message("== Section 3 of 3: everything else")
targets::tar_make()
