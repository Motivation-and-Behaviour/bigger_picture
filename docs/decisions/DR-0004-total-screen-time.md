# DR-0004: Total screen time comes only from a dedicated item

**Status:** Accepted
**Date:** 2026-09-24
**Decision makers:** Taren
**Related issues:** NA

---

## Context

The preregistration lists total screen time as an aggregated measure and says that, where a study has not calculated it, "we will calculate it as the sum of time spent on different devices or types".
DR-0002 says broad categories (e.g. TV and video games) should not be summed, because children use several screens at once and the sum overstates real time.
A cross-study check of the harmonised data found `st_total` built three ways: from a single item on all screen use, from a total the study computed by adding its own activity items, and from sums the mapping built from separately asked categories.
Summed totals ran well past what a day allows; in one study some weekday totals exceeded 90 hours.

---

## Decision

`st_total`, `st_total_wd` and `st_total_we` take only an item that asks about all screen use, or items the instrument defines as mutually exclusive parts of one such item (for example the same all-device question split by location, by purpose, or by using screens alone versus with a parent).

- A sum of separately asked activity or device items is incompatible on these rows, whoever did the summing, including a total the study published.
- Internet-only and single-device items go to their own rows and are incompatible on the total rows.

This departs from the preregistration's plan to sum devices or types where a study has no total item.

---

## Rationale

A total built by adding concurrent activities does not measure time and cannot be compared with a total that respondents reported directly.
Keeping only dedicated items means every value in `st_total` answers the same question.

---

## Alternatives considered

- Accept study-published totals as partial, and drop only the sums built during mapping: rejected, because a published sum of concurrent activities has the same problem.
- Exempt `st_total` from DR-0002 and allow sums everywhere: rejected for the same reason.

---

## Implementation

The `st_total*` notes in `harmonisation/dataschema.csv` state the rule, and the harmonisation guide repeats it under "Harmonisation conventions".
Datasets whose total rows were sums are marked incompatible.

---

## Related decisions

Applies DR-0002 (screen-time summation) to the total rows.
