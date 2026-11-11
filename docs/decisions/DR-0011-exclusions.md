# DR-0011: Primary analyses exclude only implausible values; the 3-MAD rule is a sensitivity analysis

**Status:** Accepted  
**Date:** 2026-11-11  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration says: "For the primary analyses we will exclude data which exceeds 3 absolute deviations from the median", and also excludes implausible values such as screen time above 24 hours a day, with sensitivity analyses that keep the excluded values.
It does not say whether the deviation is scaled, at what level the rule applies, or what happens to an excluded value.

Checked on the harmonised data of 30 October 2026 (53 datasets), the rule does not behave as intended:

- **Screen use.**
  Many screen measures are mostly zero or come from response bands, so the median absolute deviation (MAD) is often zero, and then every child who differs from the median is flagged.
  The MAD was zero in 38% of dataset-waves for social media, 33% for gaming and 21% for educational use.
  In the worst dataset-waves the rule flagged about half of all children.
  It also removes the heaviest users, which are the children the threshold question (RQ3) is about.
- **Outcomes.**
  On symptom scales the flagged values were all in the severe tail, between 5% and 6% of children on the three scales checked.
  In simulation, excluding on the outcome also pulled the estimated association towards zero.

---

## Decision

In the primary analyses:

- screen-use values above 24 hours a day are excluded, and nothing else is excluded on the exposure;
- outcome values are kept when they lie within the bounds the dataschema gives for the row, and a score form with documented bounds of its own is handled by a recorded exception;
- an excluded value stays excluded and is not imputed.

The preregistered 3-MAD rule runs as two sensitivity analyses:

- on screen use, among non-zero values;
- on outcome scale scores, where the MAD is above zero, within unit, using the scaled MAD.

A third sensitivity analysis keeps implausible values, as the preregistration promises.

---

## Rationale

- A rule that flags everyone who uses a screen at all, in a sample where most do not, removes real data and not errors.
- The severe tail of a symptom scale is the group the outcome exists to detect.
- Plausibility limits remove values that cannot be true without shaping the distribution.
- Keeping the preregistered rule as a sensitivity analysis shows whether the departure changes any conclusion.

---

## Alternatives considered

- The rule as preregistered: rejected for the reasons above.
- The 3-MAD rule on outcomes only: first adopted, then rejected once the flagged values were found to be the severe tail.

---

## Consequences

### Positive consequences

- High screen use and severe outcome scores stay in the primary analyses.
- The exclusion rule is the same in every unit and does not depend on a unit's distribution.

### Negative consequences or trade-offs

- This is a deviation from the preregistration, recorded in `analysis/deviations.csv`.
- Recording errors inside the plausible range are not removed; the diagnostics report lists extreme values by dataset for review.
- Fewer than half of the numeric outcome rows have bounds in the dataschema, so on the others nothing is excluded.

---

## Implementation

```md
harmonisation/dataschema.csv    `min_value` and `max_value` give the plausibility limits
R/analysis/                     the preparation step applies them and records what it dropped
analysis/models.csv             the sensitivity analyses, as rows
```

---

## Related decisions

```md
DR-0009: The estimand
```
