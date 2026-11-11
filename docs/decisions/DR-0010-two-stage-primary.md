# DR-0010: Two-stage analysis is primary, with a multilevel random-effects second stage and no fallback

**Status:** Accepted  
**Date:** 2026-11-11  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

An individual participant data meta-analysis can be fitted in one stage, as a single mixed model over all rows, or in two stages, with a model per study followed by a meta-analysis of the estimates.
The preregistration describes the models but does not choose between these.
The methods literature finds that the two agree when their assumptions match, and that differences come from the assumptions and not from the number of stages.

The pooled table here has several hundred units (dataset by wave by country), a handful of which hold most of the participants.
Units differ in survey design, in which covariates they recorded and in how outcomes were scored.
An earlier plan fitted one-stage models with a random slope, and dropped the slope when a model failed to converge.

---

## Decision

1. **Two-stage is the primary analysis.**
   Stage 1 fits each outcome on screen use in hours per day plus covariates, separately in each unit, using the unit's own survey design where one is declared.
   Stage 2 pools the unit estimates.
2. **Stage 2 is a random-effects multilevel meta-analysis**, fitted by REML, with units nested in datasets and robust small-sample inference.
   It reports the mean, the between-dataset standard deviation and a prediction interval.
3. **A cell is pooled only with 3 or more datasets.**
   Below that it is reported as insufficient data, with no estimate.
   Where the pooled estimate has fewer than 4 degrees of freedom it is described as too few studies to say, and never as showing no relationship.
4. **There is no fallback to a simpler model.**
   Stage 2 always estimates between-dataset variation.
5. **One-stage is a pre-specified check** on the overall-screen-use results and the headline results by type of screen use, and is primary only for rare binary outcomes, where most units have too few events for a model of their own.
   A one-stage model that fails to converge is reported as failed.

---

## Rationale

- Each unit keeps its own survey weights, covariates and residual variance, which a single model across all units would have to force into one form.
- Stage 1 results are stored per dataset, so adding a dataset refits only that dataset and the pooled results it feeds.
- With a few datasets holding most participants, a second stage keeps each dataset's weight explicit and easy to inspect.
- One-stage random-slope models over millions of rows, many outcomes and repeated imputations were judged infeasible.
- Dropping a random slope on convergence failure turns a random-effects estimate into a common-effect one with a narrower interval, exactly where between-dataset variation is hardest to estimate, so different cells would answer different questions.

---

## Alternatives considered

- One-stage primary with two-stage as a diagnostic: rejected for the reasons above.
- One-stage with a fallback ladder (drop the random slope, then the random intercept): rejected, because it changes the estimand cell by cell.

---

## Consequences

### Positive consequences

- Between-dataset variation is a standard output of every model.
- A new dataset or a new outcome row adds fits without refitting the rest.

### Negative consequences or trade-offs

- Units below the minimum size cannot be fitted and drop out, where a one-stage model could have kept them.
- Cells near the minimum of 3 datasets have few degrees of freedom whichever method is used, so their intervals are wide.
- Prediction intervals are approximate, because dataset sizes vary widely.
- The one-stage check omits survey weights, so it is compared with an unweighted first stage.

---

## Implementation

```md
R/analysis/fit_stage1.R            one model per unit
R/analysis/pool_stage2.R           multilevel random-effects pooling
R/analysis/fit_onestage_check.R    the pre-specified check
```

These functions are written at the analysis plan's thin-slice step.

---

## Related decisions

```md
DR-0007: The analysis runs in this repository's pipeline
DR-0009: The estimand
```
