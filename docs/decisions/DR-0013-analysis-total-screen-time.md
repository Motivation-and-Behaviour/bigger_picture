# DR-0013: The analysis derives total screen time, and the harmonised row stays dedicated items only

**Status:** Accepted  
**Date:** 2026-11-11 (amended 2026-11-12)  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration's first tier of hypotheses is about overall screen use, and says that where a study has not calculated a total, "we will calculate it as the sum of time spent on different devices or types".

DR-0002 and DR-0004 restrict the harmonised `st_total` rows to an item that asks about all screen use, because a sum of separately asked activities counts concurrent use twice and is not comparable with a directly reported total.
That keeps the harmonised row clean, but few datasets ask a dedicated item, and one dataset holds almost all the participants who have one.
On dedicated items alone, overall screen use could not be pooled for some outcome families at all.

---

## Decision

DR-0002 and DR-0004 keep governing the harmonised rows: `st_total` holds dedicated items only.

The analysis derives its own variable, `total_screen`, in the preparation step, and never writes it back to the harmonised data.
It is decided unit by unit (dataset, wave and country), so every row of a unit has a total built the same way:

- where the unit has a dedicated item, `total_screen` is that item;
- otherwise it is the sum of the unit's usual-day rows for the four content classes (watching, gaming, social media and educational use), for participants with all of the rows summed;
- a sum needs watching and gaming at minimum;
- a content class asked of only part of a unit is left out of that unit's sum, so that it does not remove the total from everyone else: it must be present on at least half of the unit's rows that have watching and gaming;
- the narrower activity rows (messaging, video chat, email, shopping, creating, work) are never added, because they overlap the content classes;
- device rows are never added to activity rows, because time on a phone already includes gaming on a phone, and rows that are a subset of another row, or a context such as school, are never added;
- a sum above 24 hours is set to 24 and flagged.

Each value carries its source (dedicated or summed), the rows that went into it and how many content-class rows the unit measured.
Source and coverage are examined as moderators when results are pooled, and hours of `total_screen` are never compared, or read against a threshold, across datasets that differ in them.

For the models that adjust one content class for the rest of a child's screen use, the rest is the sum of the unit's other summed classes, or, where the total is a dedicated item, the total minus that class.

**Amended on 12 November 2026.**
As first accepted, the sum took every activity row a dataset had.
On the data that gave implausible totals wherever a study asked about many activities: in the largest summed dataset the median was 9.1 hours a day, with about 27,000 values at the 24-hour cap, against 4.3 hours and none capped when only the four content classes are summed.
A class asked of a subsample also removed the total from the rest of its unit, in one dataset for half its rows.
The sum is therefore of the content classes only, decided per unit, with the half-of-rows rule above.

---

## Rationale

- It delivers the preregistered analysis of overall screen use, by the method the preregistration proposed, for most datasets instead of a handful.
- Keeping the sum out of the harmonised data means `st_total` still answers one question in every dataset, and the harmonisation rules need no exception.
- Rules against adding overlapping rows remove the double counting that can be removed.
- Carrying source and coverage with each value lets the analysis test whether summed totals behave differently, and lets a reader see what any estimate rests on.

---

## Alternatives considered

- Dedicated items only: rejected, because overall screen use could then be pooled in too few datasets, with one dataset dominating.
- Relax DR-0004 and allow sums on the harmonised `st_total` row: rejected, because the harmonised row would again mix two different measures.
- An average of the associations for each type of screen use, in place of a summed exposure: kept as a sensitivity analysis, because it answers a slightly different question.

---

## Consequences

### Positive consequences

- Overall screen use can be analysed in every outcome family.
- The harmonised data and the analysis each keep a single, stated rule.

### Negative consequences or trade-offs

- A summed total still counts concurrent use twice, because no dataset records which activities overlapped.
- The level of a sum depends on how many content classes a unit measured, so summed hours mean different things in different datasets.
- Screen use outside the four content classes is not in a summed total.
- Where a class was asked of only some ages in a unit, the total exists only for those ages.
- Where a dedicated item and a sum exist together, they agree only moderately in some datasets.
- A participant missing any one activity row has no summed total.

Sensitivity analyses repeat the overall-screen-use models on dedicated items only, and with sums above 24 hours excluded in place of capped.

---

## Implementation

```md
analysis/exposures.csv     `axis` says which rows a derived total may sum
R/analysis/                the preparation step derives `total_screen` and its source and coverage
harmonisation/dataschema.csv   the `st_total` note points here
```

---

## Related decisions

```md
DR-0002: Screen-time summation
DR-0004: Total screen time comes only from a dedicated item
DR-0009: The estimand
```
