# DR-0019: Missing screen use and covariates are imputed within each unit

**Status:** Proposed  
**Date:** 2026-11-16  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration says missing screen time, covariates and moderators are imputed by chained equations, 50 times, and combined by Rubin's rules.
The deviations ledger already narrows that: imputation runs within each unit and fills sporadic missingness only.

Three things shape how it can be done.
Datasets differ in what they measure, so a model across datasets would impute values a study never collected.
Some datasets hold hundreds of thousands of rows per wave and dozens of outcome rows.
And the analysis estimates relations, between screen use and outcomes, that an imputation model must contain or it will weaken them in the imputed rows.

---

## Decision

**Where.**
Each stage 1 unit (dataset by wave by country) is imputed on its own, so every relation the imputation preserves is that unit's.

**What is imputed.**

- The exposures the models use: the usual-day rows, and the total where it is a dedicated item.
  A total that is a sum is worked out again from its imputed parts.
- Sex and the covariates of the further tiers.

Outcomes are not imputed for analysis: a row without an outcome is not analysed for it.

**Sporadic missingness only.**
A variable is imputed in a unit where it is known for at least half of the unit's rows.
Below that, and where a unit never measured it, it stays missing and the covariate tiers deal with it.

**The imputation model.**
Predictive mean matching for every variable, so an imputed value is always one the unit observed: a banded item stays in its bands and a category is a real category.
Each variable's model reads:

- the other exposures and covariates, where they correlate with it;
- age and sex, and the survey weight where the unit has one;
- the outcomes, as one score per outcome family, the mean of a row's standardised scores in that family;
- in a panel, the child's own mean on each exposure and outcome score over their other waves.

**Things that cannot change within a child** (sex, ethnicity, minority status) take one value per child in each imputation.

**How many.**
Five imputations while the engine is developed, 50 for the confirmatory run, ten iterations each.
The seed of a unit's imputation comes from the project seed, the unit and the imputation number, so any imputation can be made again alone and adding imputations changes none already made.

**Analysis.**
Each imputation is analysed with the stage 1 model, and each unit's slope is combined over imputations by Rubin's rules, with Barnard and Rubin's degrees of freedom.
Stage 2 then pools one slope per unit as before.
A slope that not every imputation could fit is not carried forward.
The models on complete cases are kept as a sensitivity analysis.

**Checks that stop the build.**
An imputed cell was missing before; only screen use and covariates are imputed; an imputed value is one its unit observed and within the plausible hours; nothing is imputed where too little was observed; values that cannot change within a child do not; and two imputations are not the same draw.

---

## Rationale

Imputing within the unit matches the analysis, which estimates each unit's slope on its own.
Predictive mean matching needs no assumption about the shape of a variable's distribution, which matters for hours of screen use reported in bands.

The outcomes have to inform the imputation of screen use, or the imputed rows would show no relation between them.
Using one score per family in place of every outcome row keeps a unit's model to a few dozen variables whatever the dataset holds, and those scores are rarely missing.

---

## Alternatives considered

- **Panels imputed in wide format, one column per variable and wave.** This is what the plan first said.
  It uses a child's other waves well, but it must impute whole waves a child did not attend, only to discard them, and it grows with the square of the number of waves; the longest panels have more than twenty.
  The child's mean over their other waves carries most of the same information in two columns.
- **Every outcome row in the imputation model.** Faithful to each outcome's own model, but too large for the bigger datasets, and each outcome's missing values would need imputing too.
- **A separate imputation for each exposure and outcome pair.** Exactly matched to each model, but tens of thousands of imputation runs.
- **Imputation trees or forests.** They capture interactions and curves without being told to, at many times the cost.
- **Imputing below half observed.** Where most of a variable is missing in a unit, the imputed values would rest on a minority and on the model.
- **Imputing a summed total directly.** It would disagree with its own parts.

---

## Consequences

- An imputed exposure is related to a specific outcome only through that outcome's family score, the other variables and, in panels, the child's other waves.
  For an outcome only loosely related to its family's score, the slope in the imputed rows is pulled a little towards zero, the more so the more of the exposure is missing.
  The complete-case sensitivity analysis shows the size of this.
- The imputation model is linear in its predictors.
  A curve of an outcome over hours of screen use, or a difference in slope between groups, is therefore pulled a little towards a straight line or towards no difference in the imputed rows.
  The curves are also run on observed exposure values only, as a sensitivity analysis already in the model list.
- Missingness is assumed to be at random given what the model reads.
- With five imputations the fraction of information missing is rough; it settles with 50.
- The longitudinal strand and the curves are on complete cases until they are wired to the imputations.

---

## Implementation

- `impute_dataset()` makes one imputation of a dataset, `apply_imputation()` fills a prepared dataset with it, and `fit_stage1_imputed()` fits and combines with `pool_rubin()`.
- `review_imputation()` holds the checks, and `analysis/reports/imputation.qmd` sets imputed beside observed values.
- Settings: `imputations`, `imputation_iterations`, `impute_min_observed`, `impute_min_correlation`.
- `tests/testthat/test-analysis-imputation.R` holds the known answers, among them a case where complete cases are biased and imputation is not.

---

## Related decisions

- DR-0010: two-stage analysis.
- DR-0015: what the models adjust for.
