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

`invariants.csv` lists the rows the harmonisation guide keeps constant within a participant; the diagnostics check them, and nothing fills them a second time.

`datasets.csv` holds what the analysis needs to know about each dataset:

- `baseline_wave` is the panel's earliest wave in the data, a trial's pre-intervention wave, or blank;
- `team_contributed`, `signed_off`, `overlap_group` and `risk_of_bias` are filled at later steps.

A dataset with no row reads as a blank one, so it has no baseline and is not signed off.
Dataset branches never edit this folder, so a new dataset gets its row on `main` after it merges; a test warns until it has one.
The row matters for panels and trials, which need `baseline_wave`.

## Deviations ledger

`deviations.csv` records every place the analysis departs from the [preregistration](../docs/prereg/bigger_picture_preregistration.md), started before any analysis code so that nothing is reconstructed afterwards.

- `status` is `made` for a departure already in effect and `planned` for one the analysis plan commits to.
- `prereg_plan` gives what the preregistration says, `deviation` what is done instead, and `reason` why.
- `validity` rates the effect on how well the analysis answers the research question, and `severity` the effect on how capable a test is of showing a claim to be wrong: `improves`, `no_change`, `reduces` or `unclear`.
  `assessment` explains the ratings and names what limits the damage.
- `decision_record` lists the decision records behind the entry, separated by `|`.

Add a row when a decision changes what the preregistration promised; do not edit an entry's history away, but update its `status` when a planned departure takes effect.
