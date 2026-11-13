# Analysis registries

The spec files that drive the analysis section of the pipeline (`_targets.R`, after `analysis_data`).
They hold dataschema variable names, node labels, dataset IDs and wave labels, which the dataset specs already publish, so they are safe for this public repository.
`_targets.R` tracks `outcome_tree.csv`, `outcome_rows.csv`, `exposures.csv` and `derivations.csv` as files and checks them against the dataschema when it reads them (`outcome_registry`, `exposure_registry`, `derivation_registry`).

| File | One row per | Checked by |
| --- | --- | --- |
| `outcome_tree.csv` | Node of the outcome tree | `validate_outcome_registry()` |
| `outcome_rows.csv` | Dataschema row in an outcome or health domain | `validate_outcome_registry()` |
| `exposures.csv` | Screen-time dataschema row | `validate_exposure_registry()` |
| `derivations.csv` | Rule for computing an outcome row from other rows | `validate_derivations()` |
| `invariants.csv` | Row the harmonisation keeps constant within a participant | `tests/testthat/test-analysis-registry.R` |
| `datasets.csv` | Dataset | `tests/testthat/test-analysis-registry.R` |
| `deviations.csv` | Departure from the preregistration | `tests/testthat/test-analysis-deviations.R` |

## When the dataschema changes

A new outcome or health variable is added to `outcome_rows.csv` by `sync_outcome_registry()`, under the placeholder node `unassigned` (or `post_freeze` once the registry is frozen), with only the columns the schema settles filled in.
A new screen-time variable needs its `exposures.csv` row added by hand.
The tests fail until every such variable is registered, so a new schema row cannot silently drop out of the analysis.

## The outcome tree

- `level` is `L0` for a family (the prereg's main outcomes) and `L1` for a specific outcome, the main reporting level.
- L2 is a specific outcome by informant and is not stored: it is a node plus a row's `informant`.
  L3 is the dataschema row itself.
- An `L1` node's `parent` is its family; a deeper chain (an `L1` under another `L1`) is allowed where instruments need it.
- `prereg_status` is `named` (a specific outcome the prereg names), `expected` (within the scope of a named outcome or its example measures) or `added`.
- `headline` marks the specific outcomes in scope for the initial report.

## Outcome rows

| Column | Values |
| --- | --- |
| `node` | An `L1` node; `unassigned` or `post_freeze` while waiting; blank for a row reported on its own (then `synthesis` is FALSE) |
| `informant` | `self`, `parent`, `teacher`, `task` (a performance test) or `mixed` (any respondent, as the source study used) |
| `role` | `outcome`, `measure` (an instrument description), `negative_control`, `mediator` or `covariate`; only outcomes take a node |
| `score_type` | `continuous`, `ordinal`, `binary`, `nominal` or `diagnosis` |
| `direction` | `higher_better`, `higher_worse` or `none`, for the score defined below |
| `standardise_by` | Pipe-separated keys: `dataset`, `wave`, `country`, `age_band`, `sex`, or a `*_measure` row |
| `part_of` | Pipe-separated rows this row is part of in the same instrument (a subscale and its total) |
| `synthesis` | FALSE keeps a row out of pooling; it is reported individually |
| `use` | `core` (pooled in a node with 3 or more datasets), `report` (reported, not pooled) or `archive` (not analysed this round); fixed at the freeze |
| `prereg_family` | The row's family, equal to its node's family when it has one |
| `frozen` | TRUE from the freeze on; `unassigned` rows are then forbidden |

The score a direction refers to:

- continuous rows use their values;
- ordinal and diagnosis rows use a level's position in the dataschema's `allowed_values`, the first level being 1;
- binary rows score `TRUE` or `Yes` as 1.

`analysis_data` stores categorical rows as factors with levels in byte order, so an ordinal model takes its level order from `allowed_values`, never from the factor.

When a dataset has both a part and its whole in the same node, the analysis keeps the whole.

## Exposures

| Column | Values |
| --- | --- |
| `exposure` | The exposure a row measures; the weekday, weekend and usual-day rows of one exposure share it |
| `content_class` | The prereg's content classes: `educational`, `non_interactive` (watching), `interactive` (gaming), `social_media`; blank otherwise |
| `axis` | `activity`, `device`, `subset`, `context`, `aggregate` or `metadata`; it says which rows a derived total may sum |
| `context` | `any`, or `school` for use while at school |
| `day` | `usual`, `weekday` or `weekend`; blank for metadata |
| `unit` | `hours`, or `index` for a study's index in its own metric |
| `include` | Analysed as an exposure in its own right this round |
| `headline` | In scope for the initial report |

## Derivations

`derivations.csv` says how an outcome row is computed from other dataschema rows where a dataset records the parts but not the whole (DR-0008).

- `target` is the row computed and `sources` the rows it needs, separated by `|`; a dataset needs all of them on the same participant and wave.
- `method` is `sum`, `mean` or `weighted_mean`, the last with one positive number per source in `weights`.
- A blank `dataset_id` makes the rule general, for instruments whose scoring the dataschema defines; a `dataset_id` limits it to that dataset's instrument, and takes precedence over a general rule.
- A derived value only fills a target the dataset has not mapped.
- Covariate derivations (`hlth_bmi_z`, `fam_siblings_any`) and `total_screen` arrive with Step 4.

A test warns about any outcome row noted 'Derive at analysis' that has no rule here.

## Invariants and datasets

`invariants.csv` lists the rows the harmonisation guide carries within a participant; the diagnostics check them, and nothing fills them a second time.
Its `rule` says what the guide promises: `constant` (one value per participant, such as sex and the design values fixed at sampling), `carried` (observed values stay and missing waves take the earliest one, so a participant may hold two answers but not a gap) or `onset` (carried except to waves before onset).

`datasets.csv` holds what the analysis needs to know about each dataset:

- `baseline_wave` is the panel's earliest wave in the data, a trial's pre-intervention wave, or blank;
- `wave_order` is blank for almost every dataset, whose waves are ordered by median collection year and then median age; where that gives the wrong order it lists the wave labels in time order, separated by `|`;
- `team_contributed`, `signed_off`, `overlap_group` and `risk_of_bias` are filled at later steps.

A dataset with no row reads as a blank one, so it has no baseline and is not signed off.
Dataset branches never edit this folder, so a new dataset gets its row on `main` after it merges; a test warns until it has one.
The row matters for panels and trials, which need `baseline_wave`.

## The analysis section of the pipeline

`analysis_targets()` (`R/analysis/analysis_targets.R`) adds everything downstream of harmonisation to `_targets.R`.
Each per-dataset target reads that dataset's harmonised table and its one-row `dataset_info`, so a new or changed dataset reruns only its own targets and the combined targets they feed.

| Target | What it holds |
| --- | --- |
| `dataset_info_<key>` | The dataset's `datasets.csv` row and whether it is included |
| `dataset_summary_<key>` | Every summary and check of the dataset, from one read of its rows: manifest line, grain, numeric and level summaries, units, per-unit counts, and the invariant and day checks; no row of data and no identifier |
| `data_manifest` | Rows, participants and a content hash per included dataset, and an overall hash |
| `grain_all`, `numeric_summary_all`, `level_summary_all`, `unit_summary_all`, `unit_counts_all` | Those summaries, combined across datasets |
| `coverage`, `missingness`, `dataset_flow` | Coverage of every dataschema row, sporadic against systematic missingness, and the dataset flow |
| `violations` | The checks against the dataschema and the guide, as a table of findings |
| `exceptions_<key>` | The dataset's analysis exceptions, if it has a file |
| `prepared_<key>`, `prepared_review_<key>` | The dataset prepared for modelling (below), and its summary, flags and audit |
| `analysis_row_audit`, `rows_dropped_all`, `prepared_summary_all`, `prepared_flags_all` | What preparation used, dropped and flagged |
| `development_cells` | The cells fitted before the freeze, from `development_cells.csv` |
| `stage1_<key>`, `stage1_all` | One model per unit and cell: the slope on screen use, with how it was fitted or why it was not |
| `stage2_development` | Each cell's slopes pooled across datasets |
| `onestage_rows_<key>`, `onestage_check` | The one-stage check on one cell |
| `descriptives_<key>`, `described_screen`, `described_week`, `described_relations`, `described_outcomes`, `described_units` | Per-unit descriptive statistics of screen use and of binary outcomes, weighted where a unit has weights |
| `screen_pooled`, `week_pooled`, `relations_pooled`, `outcomes_pooled` | Those statistics pooled so each dataset counts once |
| `series_breaks`, `screen_by_year`, `screen_by_year_share` | Recorded changes to a dataset's screen items, and screen use against collection year, between studies and within like series of one study |
| `year_moderation_development` | Whether the slopes differ by collection year, on the development cells |
| `dataset_table` | One row per dataset, by number, with its citation keys from `references/` |
| `dataset_outcomes`, `dataset_outcome_flow` | What became of every dataset the project sought, from hand-entered counts |
| `measured`, `work_scale` | What was measured, and the scale of the harmonisation work, counted from the repository and the pipeline |
| `report_data_health`, `report_harmonisation_diagnostics`, `report_preparation`, `report_descriptives`, `report_thin_slice` | Reports rendered beside their sources in `analysis/reports/`, which are gitignored |

A dataset is included until any dataset has `signed_off` set in `datasets.csv`; from then on only signed-off datasets are.

Only three targets per dataset read row-level data (`dataset_summary`, `prepared` and `prepared_review`).
The largest dataset takes about 10 GB in memory, and a target for each summary would load it once per worker at the same time.

### Prepared data

`prepare_analysis_data()` turns one dataset's harmonised rows into a list of tables that share one row order:

- `base`: one eligible row per participant and wave (aged under 19; in trials, baseline rows and control arms), with `Unknown` recoded to missing, and the derived columns `unit`, `wave_order`, `is_baseline`, `school_stage` and `survey_weight_norm`;
- `exposures`: screen-time rows in hours, values over 24 hours removed, plus `total_screen` with its source, rows and coverage, and `other_<exposure>` for the rest of a row's screen use (DR-0013);
- `outcomes` and `outcomes_z`: the score of each analysed outcome row and, for continuous rows, its standardised form;
- `measures`: the extra screen-time measure blocks for the same occasions;
- `row_audit` and `dropped`: what was used and what was not, with the reason;
- `repeats`, `nodes` and `derived`: repeated scores found, nodes assigned by exception, and derivation rules that ran.

`prepared_flags_all` lists what a person should look at before signing a dataset off.

The `derived_codebook` target lists every derived variable and its rule.

### Analysis exceptions

A decision about one dataset that the registries cannot express goes in an optional `harmonisation/datasets/BPIPD-<id>/analysis_exceptions.csv`, one row per decision:

| Column | Values |
| --- | --- |
| `variable_name` | The dataschema row |
| `wave` | A wave label, or blank for every wave; always blank for `node` and `standardise_by` |
| `action` | `exclude`, `keep_out_of_bounds`, `node` or `standardise_by` |
| `value` | The node, or the pipe-separated keys; blank otherwise |
| `reason` | Why, in a sentence |

The tests read every such file, and the pipeline tracks one per dataset, so a new file is noticed.

### Data manifests

`record_data_manifest("<label>")` writes the current `data_manifest` to `analysis/manifests/<label>.csv`: dataset IDs, counts and hashes, never names.
Record one for each delivered report and at the freeze, and commit it with that state of the code.
`compare_data_manifest()` lists what has been added, removed or changed since a record.

### The engine

Two-stage is the primary analysis (DR-0010).

- `fit_stage1()` fits one model per stage 1 unit and cell: a linear model of the standardised score for continuous rows and for ordinal rows with five or more levels, a proportional-odds model for ordinal rows with three or four, and a logistic model for binary rows.
  It uses the unit's weights where it has them, clusters standard errors on the sampling unit or the family, and adjusts for age and sex in the most flexible form the unit's ages allow.
  A unit that cannot be fitted returns a row saying why.
- `pool_stage2()` pools a cell's slopes with a multilevel random-effects model, units nested in datasets, and robust small-sample inference; it needs three datasets, and marks an estimate with fewer than four degrees of freedom as too few studies to say.
- `fit_onestage_check()` fits a single model across datasets for one cell and compares it with a two-stage estimate built the same way.

Until the freeze the engine runs only on the development cells in `development_cells.csv`: each line names an exposure row and either an outcome family or one outcome row, with a reason.
These are the only associations seen before the specification is frozen, and the deviations ledger records them.

### Dataset outcomes

`dataset_outcomes.csv` holds the top of the dataset flow, which the repository cannot count for itself: how many of the datasets the project sought ended in each outcome.
Type a whole number into `datasets` on each line; a line can stay blank until its number is known, and the reports then show "to be added".
The outcomes are fixed: `included`, `agreed_awaiting_data`, `contacting_authors`, `declined`, `non_response` and `cannot_access`, each defined in the file.
When `included` is filled in, the reports check it against the number of datasets in the analysis and say so if the two differ.

### Citations and acknowledgements

The dataset table carries each dataset's citation keys from `references/dataset_citations.csv` as a pandoc citation, and a report that lists `references/datasets.bib` as its bibliography formats them in its own style.
Required acknowledgements come from `references/dataset_acknowledgements.csv`.
Both are blank for a dataset until the references snapshot has it; nothing in `analysis/` holds a citation of its own.

## Deviations ledger

`deviations.csv` records every place the analysis departs from the [preregistration](../docs/prereg/bigger_picture_preregistration.md), started before any analysis code so that nothing is reconstructed afterwards.

- `status` is `made` for a departure already in effect and `planned` for one the analysis plan commits to.
- `prereg_plan` gives what the preregistration says, `deviation` what is done instead, and `reason` why.
- `validity` rates the effect on how well the analysis answers the research question, and `severity` the effect on how capable a test is of showing a claim to be wrong: `improves`, `no_change`, `reduces` or `unclear`.
  `assessment` explains the ratings and names what limits the damage.
- `decision_record` lists the decision records behind the entry, separated by `|`.

Add a row when a decision changes what the preregistration promised; do not edit an entry's history away, but update its `status` when a planned departure takes effect.
