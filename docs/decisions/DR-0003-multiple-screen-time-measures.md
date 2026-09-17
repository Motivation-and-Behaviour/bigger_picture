# DR-0003: One harmonised row per participant, wave and screen-time measure

**Status:** Accepted
**Date:** 2026-09-17
**Decision makers:** Taren
**Related issues:** NA

---

## Context

Some studies measure screen use more than once on the same occasion.
We had originally assumed that the harmonised table would have one row per participant per wave, but may studies provide multiple measures.
For example, The Millennium Cohort Study uses a questionnaire item at every sweep and also collected a time-use diary at sweep 6.
Other studies have both a parent report and a child report.
Screen measure is likely to be a useful and interesting moderator of effects, so ideally all measures would be retained.
The harmonised table previously had one row per participant per wave, so only one measure could be put in and the rest were dropped.

---

## Decision

The harmonised table has one row per participant, wave and screen-time measure, identified by a variable `st_measure_id` that the harmoniser fills.

- A dataset's untagged `variables.csv` rows are its primary measure and they have `st_measure_id = "primary"`.
- An additional measure is a block of `variables.csv` rows tagged in a `measure` column with a categorical indicator (`diary`, `parent`, `child`, `device`).
  The harmoniser evaluates the tagged rows against the same tidied table and producesone extra row per participant-wave with `st_measure_id` set to the tag.
- Only screen-time variables can be tagged.
  Everything else is copied from the primary rows.
  If another measure has multiple responders, this is generally described in the variable name (e.g., `..._self`).

Tidiers keep one row per participant-wave and expose additional measures as extra columns.

---

## Rationale

Keeping the measure as a mapping-level concept keeps tidiers simple, keeps each measure's expressions reviewable as a block, and makes the measure an explicit key in the pooled table.
Filtering to `st_measure_id == "primary"` reproduces the previous one-row table exactly.

---

## Alternatives considered

- Exporting this information as part of the tidying process — rejected because we want minimal changes to the data in the tidiers, preferring for changes to more transparently be reported in the harmonisers.
- Wide schema variables per instrument (`st_watch_wd_diary`, ...) — rejected because it multiplies the screen-time domain per instrument.

---

## Implementation

`harmonise_from_tables()` evaluates measure blocks.
`validate_harmonisation_vars()` enforces the rules
`sync_harmonisation_vars()` keeps tagged rows after the primary rows.
Documented in the harmonisation guide under "Several screen-time measures".

---

## Related decisions

DR-0002 (screen-time summation) applies within each measure.
