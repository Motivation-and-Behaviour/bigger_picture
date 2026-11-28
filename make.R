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
# The models of the few large datasets are a section of their own, with
# fewer workers.

targets::tar_source()

sections <- bp_build_sections()
# The pattern is worked out here and handed over as text, because
# `tar_make()` reads `names` in a process of its own. `keys` keeps a section
# to some datasets; `skip` leaves some out.
build_section <- function(prefixes, keys = NULL, skip = NULL) {
  datasets <- if (is.null(keys)) "BPIPD_[0-9]+" else paste(keys, collapse = "|")
  pattern <- paste0(
    "^(",
    paste(prefixes, collapse = "|"),
    ")_(",
    datasets,
    ")$"
  )
  leave <- paste0("_(", paste(c(skip, "none"), collapse = "|"), ")$")
  targets::tar_make(
    names = tidyselect::matches(!!pattern) & !tidyselect::matches(!!leave)
  )
}

message("== Section 1 of 4: datasets")
build_section(sections$datasets)

# A worker fitting a dataset holds its whole prepared table, so the few
# large datasets get a smaller pool than the rest (see
# `bp_model_workers_heavy()` in R/config.R).
heavy <- bp_heavy_datasets()

message("== Section 2 of 4: models")
Sys.setenv(BP_MODEL_WORKERS = bp_model_workers_wide())
build_section(sections$models, skip = heavy)

message("== Section 3 of 4: models of the large datasets")
if (length(heavy) > 0) {
  Sys.setenv(BP_MODEL_WORKERS = bp_model_workers_heavy())
  build_section(sections$models, keys = heavy)
}
Sys.unsetenv("BP_MODEL_WORKERS")

message("== Section 4 of 4: everything else")
targets::tar_make()
