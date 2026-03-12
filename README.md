
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Bigger Picture

<!-- badges: start -->

<!-- badges: end -->

This repository contains the data infrastructure for the **Bigger
Picture** project: an individual participant data (IPD) meta-analysis
investigating how children’s and adolescents’ screen use relates to
learning, cognition, mental health, wellbeing, and behaviour.

The codebase is focused on: - discovering eligible datasets in a mounted
data directory - reading data from dataset-spec-defined sources -
tidying each dataset into one canonical study tibble - harmonising each
dataset into a common structure - combining harmonised outputs into an
analysis-ready table

The scientific background and preregistration are in `docs/prereg/`.

## Project Goals

The pipeline is designed to support reproducible, auditable
harmonisation across many studies with heterogeneous file formats and
layouts.

Key principles: - **spec-driven ingestion**: dataset inputs are
described in per-study YAML specs - **tidy first, harmonise second**:
each study is reshaped into one canonical tibble before variable-level
harmonisation - **table-driven harmonisation**: the shared schema,
per-study mappings, and optional lookup tables are stored as CSV files -
**inline recodes by default**: prefer readable `dplyr::recode()`
expressions in `variables.csv` for short one-off mappings - **pipeline
orchestration with `targets`**: rebuilds are dependency-aware and
reproducible

## Repository Structure

``` text
.
├── _targets.R                     # Main pipeline definition
├── R/
│   ├── config.R                   # Global config (data dir, naming pattern, schema helper)
│   ├── tidy/
│   │   ├── tidy_from_spec.R               # Study-specific tidying dispatcher
│   │   └── tidy_BPIPD_TEMPLATE.R          # Template tidier
│   ├── harmonise/
│   │   ├── harmonise_from_spec.R          # Harmonisation dispatcher
│   │   ├── harmonisation_engine.R         # Table-driven harmonisation engine
│   │   └── harmonisation_io.R             # Read/validate schema + study config
│   └── registry/
│       ├── list_datasets.R
│       ├── read_dataset_specs.R
│       ├── create_dataset_plan.R
│       ├── read_dataset_from_spec.R
│       ├── read_resource_table.R
│       ├── read_tabular_file.R
│       └── resolve_glob_paths.R
├── harmonisation/
│   ├── dataschema.csv             # Shared harmonised variable schema
│   ├── dataset.schema.json        # Dataset spec schema
│   ├── datasets/
│   │   └── BPIPD-*/               # Per-dataset dataset.yaml, variables.csv, optional lookups/
│   └── templates/
│       └── dataset/               # Template dataset spec + harmonisation metadata
└── docs/prereg/                   # Preregistration and supporting documents
```

## How the Pipeline Works

At a high level, `_targets.R` performs the following:

1.  Discover dataset folders under `/data` with names like
    `BPIPD-<id> - <name>`.
2.  Read all dataset spec YAML files in `harmonisation/datasets/`.
3.  Build a dataset plan by joining discovered datasets to dataset
    specs.
4.  For each planned dataset:
    - read the dataset spec
    - track the study tidier source file
    - track source data files
    - read raw resources into structured lists (`data`, `codebook`,
      `docs`, `meta`)
    - run the study tidier to produce one canonical study tibble
5.  For each dataset with harmonisation metadata:
    - track `variables.csv` and any lookup CSVs as target inputs
    - read and validate the shared `dataschema.csv`
    - derive harmonised variables from the tidied study tibble and
      `variables.csv`
6.  Combine all harmonised outputs into `analysis_data`.

## Data Discovery Convention

By default, datasets are expected under:

- `/data`

with directory names matching:

- `BPIPD-<numeric_id> - <dataset_name>`

This is configured in `R/config.R` and used by
`R/registry/list_datasets.R`.

## Dataset Spec Overview

Each dataset has a YAML file at
`harmonisation/datasets/BPIPD-<id>/dataset.yaml`, for example:

``` yaml
dataset_id: "21"
dataset_name: "Example Study"
status: "in_progress"

resources:
  - name: "data"
    role: "data"
    glob: "data.csv"
    reader: "csv"
```

Main fields: - `dataset_id`, `dataset_name`, `status` - `resources`
(dataset-level) - optional `waves` (wave-specific resources)

Supported `reader` values: - `csv`, `tsv`, `stata`, `spss`, `sas`,
`rds`, `parquet`, `excel`

The full contract is documented by `harmonisation/dataset.schema.json`.

## Harmonisation Contract

The default harmonisation path is table-driven.

Shared schema: - `harmonisation/dataschema.csv` - one row per harmonised
output variable

Per-dataset files: - `harmonisation/datasets/BPIPD-<id>/variables.csv` -
`harmonisation/datasets/BPIPD-<id>/lookups/*.csv` (optional, for large
recode tables)

Inputs to the engine: - `raw_dataset`: full ingestion bundle available
to the study tidier (`data`, `codebook`, `docs`, `meta`) -
`tidied_data`: one tidied tibble returned by `tidy_from_spec()` -
`spec`: parsed YAML spec - `dataschema`: shared schema table -
`variables.csv`: how each schema variable is derived - lookup CSVs:
optional explicit value recodes for mappings that are too large to keep
inline

Outputs: - `analysis_data`: one harmonised tibble per dataset

Use `harmonisation/templates/dataset/` as the starting point for new
datasets. Every dataset must provide `R/tidy/tidy_BPIPD_<id>.R`. Use
`R/tidy/tidy_BPIPD_TEMPLATE.R` as the starting point when creating a new
study tidier. Prefer inline `dplyr::recode()` expressions in
`variables.csv` for short one-off recodes. Use lookup CSVs only when a
value map becomes large enough that inline code is hard to read.

## Getting Started

### 1. Environment

Recommended workflow is the included devcontainer
(`.devcontainer/devcontainer.json`), which installs required R packages
and mounts an external dataset directory to `/data`.

### 2. Run the pipeline

From R:

``` r
targets::tar_make()
```

Useful commands:

``` r
targets::tar_visnetwork()
targets::tar_read(analysis_data)
targets::tar_read(tidied_BPIPD_21)
```

### 3. Add a new dataset

1.  Ensure dataset folder exists in `/data` and follows naming
    convention.
2.  Copy `harmonisation/templates/dataset/` to
    `harmonisation/datasets/BPIPD-<id>/`.
3.  Fill in `dataset.yaml` and `variables.csv`.
4.  Add a `lookups/` folder only if a mapping is too large to keep
    inline.
5.  Create `R/tidy/tidy_BPIPD_<id>.R`. The pipeline requires one tidier
    file per study and tracks that file as a target input.
6.  Run `targets::tar_make()`.

## Development Notes

- This repo uses `README.Rmd` as the source of truth; `README.md` is
  generated.
- `_targets/` is generated runtime state and should not be edited
  manually.
- Keep ingestion logic generic in `R/registry/`.
- Keep study-specific tidying in `R/tidy/tidy_BPIPD_<id>.R`; it should
  return one tibble.
- Keep study-specific harmonisation metadata in
  `harmonisation/datasets/BPIPD-<id>/`.
- Prefer inline `dplyr::recode()` for small dataset-specific value maps.
- Use lookup CSVs only when a recode table is large enough to justify
  separating it from `variables.csv`.

## Related Documentation

- Preregistration source:
  `docs/prereg/bigger_picture_preregistration.Rmd`
- Rendered preregistration outputs are in the same folder (`.pdf`,
  `.docx`, `.md`)
