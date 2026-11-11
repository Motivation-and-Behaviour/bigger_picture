# DR-0012: Age eligibility is decided row by row, keeping rows aged under 19

**Status:** Accepted  
**Date:** 2026-11-11  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration's eligibility criteria ask that a dataset "have a mean sample age younger than 18 years", meaning a sample of mostly pre-school or school-aged children and adolescents.
That criterion was used to select datasets.
It does not say what happens to individual rows, and harmonisation deliberately leaves eligibility to the analysis.

Some eligible datasets follow children into adulthood, and some household surveys include adults alongside children.
A strict under-18 rule would also remove students in their final year of school, many of whom are 18.

---

## Decision

The analysis keeps a row when the participant is aged under 19 at that wave, and drops it otherwise.
A row with no recorded age is dropped, because it cannot be confirmed as under 19.
A dataset's mean age plays no part in the analysis.

---

## Rationale

- The research questions are about children and adolescents, so the population is defined by each child's age when measured, not by the average age of the study they are in.
- Keeping 18-year-olds retains final-year school students, who belong to the school-aged population.
- A dataset with mostly adult rows still contributes its children, and a dataset of children contributes nothing from waves where they have grown up.

---

## Alternatives considered

- Apply the mean-age criterion to each dataset and keep all of its rows: rejected, because it would bring adults into the analysis.
- Keep rows aged under 18: rejected, because it removes final-year students.
- Keep rows with no recorded age: not chosen for now, because they cannot be confirmed as under 19; this is a cautious default and can be revisited.

---

## Consequences

### Positive consequences

- One rule, applied the same way to every row, with the dropped rows counted per dataset.

### Negative consequences or trade-offs

- This reads the preregistered dataset-level criterion as a row-level rule, and is recorded in `analysis/deviations.csv`.
- In panels, later waves drop out as participants reach 19, so the longitudinal strand sees fewer waves.
- Rows with missing age are lost even where the dataset's design implies the participant was a child.

---

## Implementation

```md
R/analysis/     the preparation step applies the rule and records the rows dropped
```

---

## Related decisions

```md
DR-0009: The estimand
```
