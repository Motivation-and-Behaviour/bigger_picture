# DR-0019: Covariates are imputed within each unit, and screen use within each model

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
And the analysis estimates the relation between screen use and each outcome, which an imputation of screen use must contain or it will weaken that relation in the imputed rows.

A first build imputed screen use once per unit, with the outcomes summarised as one score per outcome family.
An independent review deleted observed screen use from real datasets and imputed it back: slopes came out 9 to 21% too small, worse than leaving the rows out, because an outcome is only loosely related to its family's score.
The same review found whole age groups imputed where a unit had not asked them the item.
This record is the design that replaced it.

---

## Decision

**Where.**
Everything is imputed within a stage 1 unit (dataset by wave by country), so every relation an imputation preserves is that unit's.

**Covariates** (sex and the covariates of the further tiers) are imputed once per unit and imputation, by chained equations with predictive mean matching.
Their models read each other, screen use, age, the survey weight, one score per outcome family, and in a panel the child's own mean on screen use and on those scores over their other waves.

**Screen use** is imputed for each model separately: among the unit's rows that have the model's outcome, a missing exposure is drawn by predictive mean matching from a model holding that outcome, age, sex, the tier covariates, the survey weight and, in a panel, the child's mean exposure over their other waves.
Where a model also adjusts for the rest of the child's screen use, the two are imputed together.

**Outcomes** are never imputed: a row without an outcome is not analysed for it.

**Sporadic missingness only.**
A variable is imputed where it is known for at least 70% of the rows concerned, and for a child only where it is known for at least half of the unit's children of the same age in whole years.
So an item not asked of an age group, a grade or a form is not imputed for it.
Elsewhere a missing value stays missing: a covariate is then dealt with by the covariate tiers, and a row without the exposure is left out of that model.

**Things that cannot change within a child** (sex, ethnicity, minority status) take one value per child in each imputation.
Sex is imputed as female or male; a child recorded as another sex keeps that record.

**How many.**
Five imputations while the engine is developed, 50 for the confirmatory run.
Each imputation's seed comes from the project seed, the unit, the model and the imputation number, so any imputation can be made again alone and adding imputations changes none already made.

**Analysis.**
Each imputation is analysed with the stage 1 model, and each unit's slope is combined over imputations by Rubin's rules, with Barnard and Rubin's degrees of freedom.
Stage 2 then pools one slope per unit as before.
A slope that not every imputation could fit is not carried forward.
The primary models on complete cases are kept as a sensitivity analysis.

**Checks that stop the build.**
An imputed covariate cell was missing before; its value is one its unit observed, or one observed for that child; nothing is imputed where too little was observed, in the unit or at the child's age; values that cannot change within a child do not; and two imputations are not the same draw.
An imputed exposure is checked as it is drawn: it lies within the values the rows observed.

---

## Rationale

Imputing an exposure inside each model makes the imputation model hold exactly what the analysis model estimates, for every outcome, at the cost of imputing the same child's screen use separately for each outcome.
That cost is small, because only the slope is carried forward, never the imputed value.

A row with a missing exposure adds little to a slope by itself.
The gain from imputing it comes from what else is known about the child, chiefly their other waves, and from removing the bias that arises when the exposure is missing more often for children with particular outcomes.
The larger gain is in the covariate tiers, where a row with screen use and an outcome is otherwise lost for want of one covariate.

Predictive mean matching needs no assumption about the shape of a variable's distribution, which matters for hours of screen use reported in bands.

---

## Alternatives considered

- **Screen use imputed once per unit, with outcomes as family scores.** Built first and rejected on the evidence above.
- **Screen use imputed once per unit with every outcome row in the model.** Each outcome's own gaps would need imputing, and the model grows with the number of outcomes.
- **Panels imputed in wide format, one column per variable and wave.** This is what the plan first said.
  It must impute whole waves a child did not attend, only to discard them, and it grows with the square of the number of waves.
  The child's mean over their other waves carries most of the same information.
- **Imputation trees or forests.** They capture interactions and curves without being told to, at many times the cost.
- **A lower or higher threshold than 70%.** At 50% a unit's values rest too much on the model; at 90% little is gained in the covariate tiers.
- **One threshold for the unit only.** It imputes a whole age group that was never asked.

---

## Consequences

- The same child can be given different imputed screen use in the models of different outcomes.
  Each model is valid by itself; the imputed values are not a dataset and are not stored.
- Covariates are imputed from family scores, not from each outcome, so the relation between a covariate and a specific outcome is a little weakened in the rows where the covariate was imputed.
  This affects how well a tier adjusts, not the slope directly.
- The imputation models are linear in their predictors.
  A curve of an outcome over hours of screen use, or a difference in slope between groups, is pulled a little towards a straight line or towards no difference in the imputed rows.
  The curves are also run on observed exposure values only, a sensitivity analysis already in the model list.
- Which covariate a tier uses is settled on the imputed data, so a tier can use a measure that the complete-case rule would have passed over.
- The survey weight is a predictor; strata and clusters are not, and the imputation models are unweighted.
- Missingness is assumed to be at random given what the models read.
- With five imputations the fraction of information missing is rough; it settles with 50.
- Study-level moderators are not imputed; the child-level ones (sex, the socioeconomic measure, age) are, or are never missing.
- The longitudinal strand and the curves are on complete cases until they are wired to the imputations, and no convergence diagnostics are kept yet.

---

## Implementation

- `impute_dataset()` makes one imputation of a dataset's covariates and `apply_imputation()` fills a prepared dataset with it.
- `impute_cell_exposure()` imputes a model's exposure, called from `fit_stage1()`.
- `fit_stage1_imputed()` fits each imputation and combines with `pool_rubin()`.
- `review_imputation()` holds the checks, and `analysis/reports/imputation.qmd` reports what was imputed.
- Settings: `imputations`, `imputation_iterations`, `impute_min_observed`, `impute_min_observed_age`, `impute_min_correlation`.
- `tests/testthat/test-analysis-imputation.R` holds the known answers, among them a case where complete cases are biased and imputation is not, and a case where an outcome is unrelated to the rest of its family and its slope is not weakened.

---

## Related decisions

- DR-0010: two-stage analysis.
- DR-0015: what the models adjust for.
