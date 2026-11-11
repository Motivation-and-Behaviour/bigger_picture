# DR-0009: The analysis estimates the average within-unit association per extra hour of screen use

**Status:** Accepted  
**Date:** 2026-11-11  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration asks how children's screen use relates to their outcomes, and speaks of "impact" and "effects".
It does not say exactly which quantity the models estimate, and several later choices depend on that: how outcomes are standardised, how datasets are pooled, what counts as a small effect, and how results are worded.

The pooled data cannot support every reading of the question.
Outcomes are measured with different instruments in different datasets, so scores are standardised within each dataset, wave and country, and a standardised score only says where a child sits among the other children in the same sample.
The data are observational, apart from the baseline waves and control arms of a few trials (DR-0001).

---

## Decision

A **unit** is one dataset in one wave in one country.

The estimand is the average, across units, of the within-unit association between an outcome and one extra hour of screen use per day, adjusted for the unit's covariate tier.
It is reported as its mean, the spread of the association between datasets, and a prediction interval.

Two limits apply to every claim:

- **Comparisons are within units only.**
  Results compare children with more and less screen use inside the same unit, and then average those comparisons.
  Nothing compares the level of screen use or of an outcome between datasets.
- **Results are associations.**
  Wording stays "associated with" or "linked with", and never states or implies that changing screen use would change an outcome.

Tiers 1 and 2 estimate this average slope.
Tier 3 estimates its shape over hours per day, and Tier 4 how it differs between groups of children.

---

## Rationale

- A within-unit slope needs no instrument to be comparable across datasets: only the direction and the standardised size of the association are pooled.
- Standardising within unit removes differences in scale between instruments, and with them any meaning in a comparison of levels between datasets, so the estimand has to exclude such comparisons.
- Hours per day is a unit readers understand, and it is the same in every dataset, where a standardised exposure would not be.
- Stating the limits before any model runs fixes the claim before results are seen.

---

## Alternatives considered

- The association across all children in the pooled data, ignoring which dataset they are in: rejected, because differences between datasets in instruments, eras and populations would be mixed into the estimate.
- A causal effect of screen use: rejected, because the design cannot identify one; negative-control outcomes and a further confounder tier are sensitivity analyses, not a route to a causal claim.

---

## Consequences

### Positive consequences

- Every model, table and plain-language statement can be checked against one stated quantity.
- Datasets with different instruments contribute to the same estimate without a crosswalk between instruments.

### Negative consequences or trade-offs

- A one standard deviation difference in an outcome is not the same amount of the underlying construct in every unit, because samples differ in how much children vary; age span and score type are examined as moderators for this reason.
- Outcome levels are not compared between datasets, so the analysis cannot say that children in one study or country have poorer outcomes than those in another.
  How the association differs between groups of children within datasets is estimated in Tier 4.
- The estimate is an average over the units that happen to exist, weighted by their precision, and not over any defined population of children.

---

## Implementation

```md
analysis/outcome_rows.csv    `standardise_by` sets the grouping keys for each outcome row
R/analysis/                  stage 1 fits one model per unit; stage 2 pools the slopes
analysis/deviations.csv      records where this departs from the preregistration's wording
```

---

## Related decisions

```md
DR-0001: Experimental designs
DR-0010: Two-stage analysis is primary
DR-0013: Total screen time for the analysis
```
