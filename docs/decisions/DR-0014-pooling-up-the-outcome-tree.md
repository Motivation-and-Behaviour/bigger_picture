# DR-0014: A family is the plain average of its specific outcomes, and only disagreement keeps it from the headline

**Status:** Accepted  
**Date:** 2026-11-13  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

DR-0008 arranges the outcome rows in a tree: a family (L0), a specific outcome (L1), a specific outcome by informant (L2) and the dataschema row (L3).
DR-0010 pools one row's slopes across datasets.
The preregistration also asks for a result per family, so slopes have to be combined above the row.

Three things make that harder than pooling one row.
Rows of one specific outcome fitted in one unit are fitted on the same children, so their slopes err together.
Specific outcomes differ widely in how many datasets measured them, so the way they are weighted decides what a family's estimate means.
And a family whose specific outcomes point different ways should not be reported as one finding.

---

## Decision

**Scale and direction.**
Before pooling above the row, a slope in log odds is divided by pi / sqrt(3) to give SD per hour, and a slope on a row where higher is worse changes sign.
A positive slope then always means a better outcome with more screen use.
Odds ratios are reported in a row's own direction.

**Specific outcomes.**
A specific outcome, and a specific outcome by informant, pool their rows with the stage 2 model of DR-0010, adding the covariance between slopes fitted on the same children and, where a unit gives several rows, variation between a unit's rows.
The covariance is estimated from each unit's own rows, as a robust standard error is, on the rows, terms and clusters stage 1 uses.
Inference stays robust to the dependence of a dataset's slopes, so it does not rest on that covariance being right.

**Families.**
A family's estimate is the plain average of its pooled specific outcomes.
Every specific outcome that pools enters it, whether or not the registry marks it for the headline, and including one with too few studies to say much on its own.
Its standard error allows for any dependence among a dataset's slopes, within and across specific outcomes, so a score that sits in two specific outcomes adds nothing to the family's precision.
For a family of two or more specific outcomes that standard error is the jackknife form of the robust variance (amended 17 November 2026).
With the form used for a single specific outcome, a simulation on the real design found a family's 95% interval covering the truth about 93% of the time, and 89% where two of its specific outcomes had a variance between datasets estimated at zero; the jackknife form gave 95 to 97%.
A later independent simulation agreed at the heterogeneity estimated from the data (92 to 93% against 95 to 96%).
The jackknife form is conservative where datasets differ little: where there was no variance between datasets at all, its 95% interval covered the truth 99% of the time.
Its degrees of freedom are also a little lower, so a family is somewhat more often one with too few studies to say.
A family of one specific outcome keeps that outcome's standard error exactly.
The average weighted by each specific outcome's number of datasets is reported beside it as a sensitivity analysis.

**The headline.**
A family is estimated and reported whenever at least one of its specific outcomes could be pooled.
Only evidence that its specific outcomes disagree keeps it from the headline:

- two specific outcomes have intervals on opposite sides of zero; or
- their estimates span more than the smallest effect of interest, and a robust test rejects that they are equal.

Uncertainty does not keep a family from the headline.
A family that rests on one specific outcome, one whose test could not be run, and one with too few studies to say are labelled as such and reported with their intervals.

---

## Rationale

A plain average gives the family one fixed meaning, the average association across its specific outcomes, which does not change as datasets are added and does not depend on which outcomes studies happen to measure most.
Nothing is given less weight for being uncertain.

Weighting by precision was tried on the development cells and rejected.
One specific outcome took 93% of a family's weight because its standard error was very small, and that standard error rested almost entirely on one dataset.
Precision weights reward exactly the estimates whose uncertainty is understated.

The headline rule asks for evidence of disagreement because estimates of equal effects differ by chance.
In simulation, ten specific outcomes with one true effect spanned more than the smallest effect of interest in most draws (between about 60% and 98%, depending on how many datasets each had), so a rule based on the span alone would usually have kept such a family from the headline.
With the test, it does so about 4% of the time.
Keeping a family from the headline for being uncertain would hide results for the reason they most need showing with their intervals.

---

## Alternatives considered

- **Pooling rows directly into the family.** Datasets that measure many rows would dominate.
- **A person-level composite score.** Its meaning changes with each study's mix of instruments.
- **Weighting specific outcomes by precision, or by random-effects weights.** Both lean on each specific outcome's standard error; see the rationale.
- **Weighting by number of datasets as the primary estimate.** The weights do not depend on the estimates, so it is kept as the sensitivity analysis, but the family would then lean towards whatever is measured most.
- **Averaging only specific outcomes with enough studies.** It excludes outcomes for being uncertain, and which ones count would change as datasets are added.

---

## Consequences

- The headline rule misses real disagreement more often than it finds it when the specific outcome that differs is a thin one: in simulation, one thin specific outcome off by twice the smallest effect of interest was flagged about a quarter of the time, and a well-measured one about three quarters.
  A family in the headline has not been shown to be coherent; it has not been shown to be incoherent.
- One thin specific outcome can set a family's precision: a family measured by many datasets can have few degrees of freedom.
  The by-datasets average shows how much that matters.
- The family's estimate is on the SD scale even where some of its rows are binary.
- The covariance between slopes treats every score as numeric, so it is approximate for binary and short ordinal rows.
- The default correlation, used where too few children have both scores, and the smallest effect of interest are settings fixed at the freeze.

---

## Implementation

- `tree_effects()`, `tree_covariance()`, `fit_tree_model()`, `pool_tree_group()`, `pool_tree_family()` and `pool_tree()` in `R/analysis/`.
- `stage1_correlations()` estimates the correlation between two slopes of one unit.
- `tests/testthat/test-analysis-tree.R` holds the known answers, including that a score in two specific outcomes counts once.

---

## Related decisions

- DR-0008: the outcome registry and tree.
- DR-0009: the estimand.
- DR-0010: two-stage analysis and the stage 2 model.
