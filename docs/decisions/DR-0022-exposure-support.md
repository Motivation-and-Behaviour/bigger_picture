# DR-0022: A slope is pooled only where enough rows have screen use that varies

**Status:** Accepted  
**Date:** 2026-11-27  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

Stage 1 fits a slope of an outcome on hours of screen use in every unit and cell with at least 30 rows, at least 10 rows in each class of a binary outcome, and an exposure whose standard deviation is not exactly zero (DR-0010).
Those rules count rows, and a slope is learned only from the rows where the exposure differs.
Some kinds of screen use are rare in some studies: educational use recorded in a time-use diary, or social media among preschool children.
There, almost every child records none, and the slope rests on the few who record some.

The first run of the frozen specification showed what that does.
In one unit, none of the children with any educational use had the binary outcome, so the logistic slope had no finite estimate; the fit stopped at a slope more than a thousand times the typical size, with a standard error that made it look precise.
In another, two children with any social media use and an outcome score decided a slope more than a hundred times the typical size, again with a small standard error.
Both are known failures: a logistic model cannot estimate a slope under separation, and robust standard errors are too small when a few rows carry the slope.
Stage 2 weights by those standard errors, so the pooled estimates above such slopes had intervals several standard deviations wide, at the specific outcome and at the family above it.

This was found after the specification was frozen and after the first confirmatory results had been seen.
The rule below is therefore a deviation, recorded in the ledger (D21).

---

## Decision

**A slope is fitted only where enough of its model's rows have screen use that varies.**
Among the rows a unit's model keeps, after its weight, covariate, age and sex rules:

- at least 30 rows have a recorded exposure value other than the commonest recorded value (ties go to the lowest value); and
- for a binary outcome fitted by a logistic model, at least 10 rows of each outcome class are among those rows.

The two numbers are the ones stage 1 already uses, the minimum unit size and the minimum class size, applied to the rows a slope is learned from.
No new threshold is introduced.

**The numbers were fixed from counts alone.**
Before the rule was chosen, the rows away from the commonest value were counted for every fitted cell, and candidate minimums of 5, 10, 20, 30 and 50 were compared by how many cells each would leave out.
No estimate was read in that comparison, and the minimums were chosen because the specification already held them, not for what they did to any result.

**Recorded values only.**
An imputed exposure does not count towards either minimum, so imputation never makes a slope fittable, and a model's verdict is the same in every imputation.

**The rule is part of the unit's model.**
`fit_stage1_unit()` applies it with its other minimums, and a slope that fails returns a row saying which minimum it missed.
Every model built on that function is covered: the covariate tiers, the models adjusted for other screen use, the fits on imputed data, the longitudinal strand, where the exposure is the one that strand's model uses, and the models of moderation.
A model of moderation estimates a difference between two groups' slopes, so there the 30 rows are needed within each of the two groups compared as well (DR-0021).

Every model was refitted with the rule in place, in the run that also fitted the models of moderation.

---

## Rationale

The existing minimums were written to stop a slope resting on a handful of children; they miss the case where the handful is hidden inside a large unit.
Applying the same minimums to the rows that vary closes that gap with a rule the specification already contains.

A rule on counts treats every dataset alike and catches cases that the size of an estimate would not reveal: a sparse cell can give an ordinary-looking slope with a standard error that is still too small.

Leaving the slopes in would not be neutral.
They widen the intervals of the nodes and families above them and add to the estimated heterogeneity, so the report would say less about those outcomes than the data support.

---

## Alternatives considered

- **Leave the frozen specification as it is.** No deviation, but pooled results that include slopes the models could not estimate.
- **Name the affected datasets as exceptions.** Quick, but chosen by looking at results, and it would miss sparse cells whose slopes look ordinary.
- **Change the estimator: penalised logistic regression and small-sample standard errors.** Deals with separation and helps the standard errors, but does not help a slope that rests on two children, and changes every estimate.
- **Apply the rule to fitted slopes before pooling, from counts made beside the models.** Built first, because it needed no refit; it counted a cell's rows before the model's own row rules, and did not reach the longitudinal strand. Dropped once the refit was going ahead for the models of moderation.
- **A minimum on the standard deviation of the exposure.** Depends on the scale of each exposure and has no anchor in the specification.

---

## Consequences

- On the report's datasets, 308 of the 21,889 slopes fitted on imputed data at the first covariate tier before the rule are left out (1.4%), in 16 of the 87 datasets with fitted slopes: 193 for too few rows where the exposure varies and 115 for too few of one outcome class among them.
  Three datasets account for 251 of them.
  Every other slope is identical to the one fitted before the rule.
- In the longitudinal strand 139 of 12,718 rows are left out.
- The cells that count towards each family's number of comparisons are recounted by the frozen rule, so a count falls where a cell no longer has a slope to pool.
- An ordinal outcome with three or four levels is held to the 30-row minimum only.
  Stage 1's own minimum for such a model, five rows in each level, is not applied to the rows that vary.
- The curves keep their own, stricter rule of 30 rows in each stretch between knots (DR-0016).
- The count of cells stage 1 can use (`count_cells()`), which the feasibility report draws on, does not apply this rule, so it can call usable a cell whose slope is then left out.
- The rule was adopted after confirmatory results had been seen, and the publications report it as a deviation.

---

## Implementation

- `fit_stage1_unit()` and `exposure_varies()` in `R/analysis/fit_stage1.R`.
- Settings `support_min_rows` and `support_min_class`, passed to the fits by `bp_fit_settings()`.
- `tests/testthat/test-exposure-support.R`.

---

## Related decisions

- DR-0010: the two-stage analysis and stage 1's minimums.
- DR-0014: pooling up the outcome tree.
- DR-0016: curves and their own minimum.
- DR-0019: imputation, and which models have their exposure imputed.

---

## Related resources

- ADR concept: <https://adr.github.io/>
- Project documentation: `docs/`
