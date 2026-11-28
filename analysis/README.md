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
| `models.csv` | Model family | `read_model_spec()` |
| `hypotheses.csv` | Preregistered prediction, by exposure and outcome family | `read_hypotheses()` |

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
- `team_contributed` is TRUE for a dataset the project's own team collected or supplied, for the preregistered sensitivity analysis that leaves those out; none in the report is.
- `signed_off`, `overlap_group` and `risk_of_bias` are filled at later steps.

A dataset with no row reads as a blank one, so it has no baseline and is not signed off.
Dataset branches never edit this folder, so a new dataset gets its row on `main` after it merges; a test warns until it has one.
The row matters for panels and trials, which need `baseline_wave`.

## Building

`Rscript make.R` builds the pipeline in three sections: the per-dataset targets that load whole datasets, then the per-dataset targets that fit models and count on prepared datasets, then everything else.
The two kinds of work never overlap, so the models section can use more workers (`bp_model_workers_wide()`) than is safe while whole datasets are loading.
A plain `targets::tar_make()` builds the same targets with four model workers.
`bp_build_sections()` lists what each section builds, and a test checks it against the pool each target runs in.

Functions take their settings as an argument, and the pipeline hands each stage only the settings it reads, so changing one setting reruns only the stages that read it.
A function's default goes through `default_analysis_settings()` and never names `analysis_settings()` itself, which the pipeline would count as a dependency of every function; `tests/testthat/test-analysis-settings.R` fails if a function that takes settings is called without them.

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
| `curves_<key>`, `curves_all`, `curves_development` | Each unit's curve of the outcome over hours of screen use for the development cells, and the pooled curve of each cell with its threshold duration |
| `curve_simulation`, `curve_simulation_summary` | Whether the curve method recovers curves built into the real hours of one cell: 200 simulated outcomes per scenario, with and without error inside the bands of banded items |
| `cell_counts_<key>`, `cell_counts_all`, `cell_units_all`, `feasibility` | Rows and participants with each exposure together with each outcome row, per unit, and what the signed-off datasets can therefore pool at each level of the tree, at each covariate tier, by moderator and over collection years; counts only, no model |
| `stage1_adjusted_<key>`, `stage1_adjusted_all`, `tiers_development` | The development cells fitted at covariate tiers 2 and 3 (DR-0015), and each tier's pooled slope beside tier 1 pooled over the same units |
| `slope_correlations_<key>`, `slope_correlations_all` | How strongly the slopes of two rows of one specific outcome share their error in a unit, estimated from the unit's rows |
| `tree_development` | The development cells' slopes pooled up the outcome tree: specific outcome by informant, specific outcome, and family |
| `mapping_status_<key>`, `mapping_status_all`, `moderation_development` | Each dataset's harmonisation status per mapped variable, and whether the development cells' slopes differ by informant, reporter, response format, instrument, score type, harmonisation status and the other stage 2 moderators, within each specific outcome |
| `onestage_rows_<key>`, `onestage_check` | The one-stage check on one cell |
| `descriptives_<key>`, `described_screen`, `described_week`, `described_relations`, `described_outcomes`, `described_units` | Per-unit descriptive statistics of screen use and of binary outcomes, weighted where a unit has weights |
| `screen_pooled`, `week_pooled`, `relations_pooled`, `outcomes_pooled` | Those statistics pooled so each dataset counts once |
| `series_breaks`, `series_breaks_unused`, `screen_by_year`, `screen_by_year_share` | Recorded changes to a dataset's screen items, the recorded changes that separate no units (none, when the file is right), and screen use against collection year, between studies and within like series of one study |
| `model_spec`, `hypotheses`, `model_cells`, `fit_cells` | The model list, the preregistered predictions, each model's exposure and outcome cells, and the distinct cells stage 1 fits; before the freeze these are the development cells only |
| `stage1_other_<key>`, `stage1_other_all`, `tree_other` | The content-class cells adjusted for the rest of the child's screen use, and pooled up the tree |
| `tree_tier2`, `tree_tier3` | The slopes at covariate tiers 2 and 3 pooled up the tree |
| `longitudinal_<key>`, `longitudinal_all`, `longitudinal_development` | The lagged, within-child and matched cross-sectional models per pair of waves (DR-0018), and each pooled per outcome row and up the tree |
| `imputed_<key>`, `imputation_review_<key>`, `imputation_units_all`, `imputation_levels_all`, `imputation_summary` | Each dataset's imputations of sporadically missing covariates, one branch per imputation (DR-0019); their hard checks; and imputed values set beside observed ones |
| `stage1_imputed_<key>`, `stage1_imputed_all`, `stage1_other_imputed_<key>`, `stage1_other_imputed_all` | Stage 1 fitted on each imputation, with each model's missing screen use imputed for that model, and combined by Rubin's rules, at covariate tiers 1 to 3 and adjusted for other screen use |
| `stage1_sex_imputed_<key>`, `stage1_sex_imputed_all`, `stage1_ses_imputed_<key>`, `stage1_ses_imputed_all` | The models of moderation by sex and by socioeconomic position on imputed data: per unit and cell, the difference between two groups' slopes, with the two slopes beside it |
| `tree_sex_imputed`, `tree_ses_imputed` | Those differences pooled up the outcome tree as slopes are; `label_moderation()` names each row's model in the results, where a difference is tested and never put in the impact bands |
| `stage2_imputed`, `tree_imputed`, `tree_imputed_tier2`, `tree_imputed_tier3`, `tree_other_imputed` | Those slopes pooled across datasets and up the outcome tree |
| `panel_pairs_<key>`, `panel_pairs_all` | For each panel and pair of successive waves, the children kept and how the screen use and age of those who stay differ from those who leave (DR-0018); no outcome is read |
| `results_development` | Every model built so far in one table: estimate, interval, p, tiered q with its K (DR-0017), the 1 against 4 hours contrast, the equivalence test and the preregistered prediction |
| `year_moderation_development` | Whether the slopes differ by collection year, on the development cells |
| `dataset_table` | One row per dataset, by number, with its citation keys from `references/` |
| `dataset_outcomes`, `dataset_outcome_flow` | What became of every dataset the project sought, from hand-entered counts |
| `measured`, `work_scale` | What was measured, and the scale of the harmonisation work, counted from the repository and the pipeline |
| `report_data_health`, `report_harmonisation_diagnostics`, `report_preparation`, `report_descriptives`, `report_thin_slice`, `report_feasibility` | Reports rendered beside their sources in `analysis/reports/`, which are gitignored |

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
| `wave` | A wave label, or blank for every wave; always blank for `node` and `standardise_by`, and always given for `own_series` |
| `action` | `exclude`, `keep_out_of_bounds`, `node`, `standardise_by`, `total_dedicated`, `part_of_total` or `own_series` |
| `value` | The node, or the pipe-separated keys; blank otherwise |
| `reason` | Why, in a sentence |

On a screen-time row an exception can `exclude` it, say that a total mapped as partial is a total of all screen use at a wave (`total_dedicated`, on `st_total`), or say that a content-class row is part of what the dataset's dedicated total measured (`part_of_total`); see DR-0013.
On an outcome row, `own_series` says the row was asked differently at one wave, so for the within-child model that wave is not a child's next wave from any other, and no pair starts from it to a wave asked the usual way; the row is still used at that wave in every other model (DR-0018).

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
  A slope is fitted only where at least 30 of the model's rows have a recorded exposure other than the commonest value and, for a binary outcome, at least 10 of each class are among them (DR-0022).
  With `moderator = "sex"` or `"ses"` the model also holds the product of screen use with the unit's groups, and the estimate is the first group's slope less the second's: girls less boys, the lowest socioeconomic group less the highest (DR-0021).
- `fit_stage1_imputations()` fits a group of a dataset's imputations and `combine_stage1_imputations()` combines them all by Rubin's rules; the pipeline fits the groups as separate targets (`stage1_imputed_fits_<key>` and the like), so a large dataset's fits spread over the workers.
- `pool_stage2()` pools a cell's slopes with a multilevel random-effects model, units nested in datasets, and robust small-sample inference; it needs three datasets, and marks an estimate with fewer than three degrees of freedom as too few studies to say.
- `fit_onestage_check()` fits a single model across datasets for one cell and compares it with a two-stage estimate built the same way.

Until the freeze the engine runs only on the development cells in `development_cells.csv`: each line names an exposure row and either an outcome family or one outcome row, with a reason.
These are the only associations seen before the specification is frozen, and the deviations ledger records them.

### Pooling up the outcome tree

`pool_tree()` takes the stage 1 slopes above the outcome row.
`tree_effects()` first puts them on one scale and one direction: a slope in log odds is divided by pi / sqrt(3) to give SD per hour, and a slope on a row where higher is worse changes sign, so a positive slope always means a better outcome with more screen use.
Rows with `synthesis` FALSE, no node or no direction do not pool; the result's `left_out` attribute counts them by row and reason.

- A specific outcome by informant (L2) and a specific outcome (L1) pool their rows with `pool_tree_group()`: the stage 2 model, with the covariance of slopes fitted on the same children and, where a unit gives several rows, variation between a unit's rows.
  An L2 group of log-odds rows of one direction also reports an odds ratio in the rows' own direction, as stage 2 does for a row.
- That covariance comes from `stage1_correlations()`, which estimates how strongly two slopes of one unit share their error, on the rows, terms and clusters stage 1 uses but treating every score as numeric.
  It sets the working model's weights, and the robust inference does not depend on it being right.
  A pair with too few shared rows takes `tree_default_correlation`, and correlations that cannot all hold at once are shrunk towards zero; both are counted in the result.
- A family (L0) is the plain average of its pooled specific outcomes, from `pool_tree_family()`, so each specific outcome counts once however many rows or datasets it has.
  Its standard error allows for any dependence among a dataset's slopes, so a score that sits in two specific outcomes adds nothing to the family's precision.
- A family is estimated whenever one of its specific outcomes pooled, and one that could not be pooled is named in `nodes_left_out`.
  A specific outcome with too few studies to say still enters the average; `k_nodes_thin` counts them.
- `headline` is TRUE for a family with no warning, and only evidence of disagreement warns (DR-0014): two specific outcomes with intervals on opposite sides of zero, or estimates that span more than the smallest effect of interest where a robust test rejects that they are equal.
  Uncertainty does not: one specific outcome only, a test that could not be run and too few studies to say are recorded in `note` and `status`.
  A specific outcome is warned in the same way where two of its informants have intervals on opposite sides of zero.
- The `_by_datasets` columns give the family weighted by each specific outcome's number of datasets, as a sensitivity analysis.
- A family averages every specific outcome that pools, whether or not the registry marks it `headline`: that column decides what a report leads with.

### Stage 2 moderators

`moderate_tree()` asks, within each specific outcome, whether the slopes differ between the levels of each label in `bp_tree_moderators()`; `tree_moderators()` says where each label comes from.
`moderate_tree_group()` fits the specific outcome's model with one mean per level and tests that the means are equal, with the robust small-sample test used for families.
A level needs the minimum number of datasets to enter, and a moderator with fewer than two such levels is reported as not comparable.

Most datasets have one level of a moderator, so a difference is mostly a difference between kinds of study, which also differ in country, age and instrument.
`k_datasets_several` counts the datasets with more than one of the compared levels, and `largest_dataset_share` the share of the compared slopes one dataset supplies.

Reading the table:

- `test_status` says whether the test of equal means rests on enough studies; under three degrees of freedom it is `too few studies to say`, whatever the p-value.
- `difference_of` names which level is taken from which.
- `same_as` marks a moderator that sorts a specific outcome's slopes exactly as an earlier one does: the two are one comparison under two names.
- P-values are not adjusted for the number of comparisons; the tiered procedure of Step 10 does that for the frozen model list.
- The comparison has little power: in designs like the development cells', a true difference of one smallest effect of interest was found about a third of the time, so a test that does not reject is not evidence of no difference.
- A row the analysis derives (the derived total, a derived outcome row) has no mapping of its own, so its harmonisation status is `derived`.

Not built yet from the plan's list (Step 7 task 4):

- age span and collection year are continuous and are handled apart (`moderate_by_year()`; age comes with Step 12);
- `total_coverage`, which is also continuous;
- within-study comparisons of response format and responder using tagged measure blocks: stage 1 fits the primary measure only, so those two moderators compare between datasets;
- the compatible-only subset where five or more datasets remain;
- `risk_of_bias` has no ratings yet, and `total_source` applies only to the derived total, which no development cell uses, so neither compares anything so far.

### Curves and the threshold duration

`fit_stage1_curves()` fits, in each unit, a natural cubic spline of screen use with knots fixed in hours (`curve_knots`, `curve_boundary`), so every unit estimates the same curve terms.
It is the linear stage 1 model at tier 1 with the curve in place of the slope, for rows that take a linear model.
`pool_curve()` pools the terms across datasets and reads the curve at each hour of `curve_grid` as the difference from the low-use reference (`curve_reference`), in SD of the outcome.

- A unit's curve needs more distinct values of screen use than the curve has terms, the minimum number of rows in every stretch between knots (`curve_interval_rows`), and more clusters than terms; a unit that falls short is not fitted and says why.
- The pooled model lets each term vary between datasets and, where there are more units than datasets, between a dataset's units.
- A unit supports an hour where it has the minimum number of rows within `curve_window` hours of it.
  A curve is interpretable at an hour inside the boundary knots that at least the minimum number of datasets support, counting only datasets that also support the reference; beyond the boundary knots the curve is a straight line continued and is not read.
- The threshold duration is read two ways, each the lowest interpretable hour above the reference that meets its rule (DR-0016).
  Conservative (`threshold_hours`): the interval lies wholly beyond the smallest effect of interest (`smallest_effect`) from the reference, with at least the minimum robust degrees of freedom there (`min_robust_df`).
  Central (`threshold_central_hours`): the estimate is beyond the smallest effect of interest, the interval excludes no difference and the degrees of freedom reach the minimum, at that hour and at the next hour read.
  Each has a direction, given as a better or worse outcome, and a flag saying whether the curve stays beyond at every later hour.
- `simulate_curve_recovery()` checks the method on real hours of screen use with a simulated outcome that follows a known curve (a plasmode simulation); `curve_truths()` lists the curves it builds in, and DR-0016 says what it found.

A conservative threshold depends on how precisely a curve is estimated as well as on the curve, so thresholds are not comparable between cells without their intervals.
In the simulation it was almost never reported where there was none, was usually reported an hour or more after the true one, and was never reported from five datasets.
The central threshold was on time on average, and was reported for a curve levelling off just short of the smallest effect in about a third of runs.

The breakpoint of the preregistered piecewise model is not built yet.
Also not built: response format as a moderator of the curve with a continuous-only check, a sensitivity analysis of how the top band is scored, and reading the derived total's curve within one source of the total.

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

### Changes to a screen item

`series_breaks.csv` lists where a repeated study changed how it asked about screen use: new wording, new response bands, another respondent, another instrument.
A trend over collection years is read only along units on the same side of every line, so a redesign is not read as change over time.
Each line names a `dataset_id`, an `exposure` and a `reason` taken from the study's own questionnaires and codebooks, and says where the changed item applies in one of two ways.
`from_year` gives the first collection year of the changed item, for a change that holds from then on.
`waves` lists the waves that used it, separated by semicolons and spelt as in the harmonised `wave` column, for a study whose waves share or straddle calendar years or whose item came and went.
Fill in one of the two and leave the other blank.
`day` limits a line to the `weekday`, `weekend` or `usual` row of the exposure; blank means all three.
`scope` is blank for a change to the item itself, or `trend` for a line that only separates series for trends over years, such as a wave whose values mix respondents: the models that compare a child's own waves compare respondents child by child and ignore such a line.
A list of waves is closed: when a study adds a wave that still uses the changed item, add the wave to every such list, or it is read as the old item.
The derived total is built from whichever items a dataset has, so every line of the dataset splits its series, whatever the exposure or day.
A line with no units that have the exposure on one of its two sides is listed in the `series_breaks_unused` target, which should be empty.

## The model list

`models.csv` has one row per model family, and everything the analysis fits is a row: a sensitivity analysis is a further row, never a further code path.
It is a draft until the freeze, when it is fixed with the registries.

| Column | Values |
| --- | --- |
| `model_name` | Lower case, unique |
| `step` | The step of the analysis plan that runs it, 10 to 14 |
| `tier` | The preregistered tier whose q-values it takes, 1 to 5; exploratory, descriptive and post-freeze models take 5 |
| `levels` | The levels of the outcome tree it is reported at, pipe-separated: `L0`, `L1`, `L2`, `L3` |
| `exposure_set` | Pipe-separated: `total` (the derived total), `content_classes`, `other_included`, `social_media`, `school_content` (the content-class rows asked about school), `school_other` (the other rows asked about school) |
| `outcome_set` | `core`, `report`, `negative_control`, or `core_rare_binary` (binary rows where more than half the units that measure the row have fewer than 10 events) and `headline_cells`, both settled from the data where their models are built |
| `moderator` | The moderators it tests, pipe-separated; blank for none |
| `covariate_tier` | `1`, `2`, `3` or `family` (DR-0015) |
| `sample` | `main`, or the subset of rows or datasets it uses |
| `engine` | How it is fitted, one of `bp_model_engines()` |
| `variant` | The one thing a secondary or sensitivity model changes; blank for a primary model |
| `analysis_class` | `confirmatory`, `prereg_secondary`, `post_freeze`, `exploratory` or `descriptive`; shown in every results table |
| `built` | Whether its code exists yet; code for Steps 11 to 14 may follow the freeze, its row may not |

`expand_model_cells()` turns the sets into exposure and outcome cells from the registries, so a new schema row joins a model without an edit here.
Until `bp_specification_frozen()` is TRUE, which the lead sets in the tagged commit that freezes the specification, a model's cells are only the development cells that fall in its sets.

`hypotheses.csv` gives the direction the preregistration predicts for an exposure and a family: `negative`, `positive` or `none`.
A prediction of `none` is tested for equivalence against the smallest effect of interest (DR-0017).
Estimates are aligned so that positive means a better outcome with more screen use, so `negative` is a negative estimate in every family.
