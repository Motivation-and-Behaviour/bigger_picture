# DR-0005: Data year falls back to the earliest publication

**Status:** Accepted  
**Date:** 2026-10-20  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

`data_year` records the calendar year in which each row's data were collected.
Most datasets release a per-row interview date or a survey year label, or their documentation dates the fieldwork.
Some contributed datasets date nothing: the data hold no collection date, and neither the supplied documents nor the publications give fieldwork dates.
The previous rule never took a publication year, so these datasets were left NA on `data_year` and dropped out of any analysis that uses it.
For other datasets, the timing of fieldwork is documented by the study's investigators in a source outside the supplied files, such as a project description or a funding report, which the previous rule did not mention.
Some publications also print when the manuscript was received or submitted, which shows that collection had ended by then without saying when it took place.

---

## Decision

`data_year` takes the first of these that is available:

1. The per-row interview or visit year, or the survey's own year label (compatible).
2. A year assigned per wave from wave labels or the supplied fieldwork documentation, as the modal fieldwork year or the midpoint of a multi-year window (inferred).
3. Fieldwork timing documented by the study's investigators outside the supplied documents (e.g. a project description or funding report), each wave taking its documented year (inferred).
4. Otherwise, the year of the earliest publication reporting the data, as one constant for every row (inferred), with that publication cited in the notes.

Where a publication reporting the data documents an upper bound on when collection ended, such as a received or submission date, in a year before the constant in step 4, that constant takes the earliest such bound's year instead, and the notes cite the bound.
A bound limits only when collection ended, so it never dates the data at steps 2 or 3.
`data_year` is NA only when no document or publication gives a year.

---

## Rationale

`data_year` serves only to examine shifts in effects over time, at the resolution of calendar years across many studies.
At that resolution a small error, such as data collected a year or two before the first paper appeared, is acceptable, and it costs far less than leaving a dataset out of those analyses.
Publication years were previously excluded because they come after collection; keeping every documented source above them means the fallback is used only where nothing dates the data more closely.
A received or submission date in a year before the earliest publication year shows that collection had ended by then, so taking the bound's year brings the fallback closer to collection wherever a publication prints one.

---

## Alternatives considered

- Keep `data_year` NA wherever no fieldwork documentation exists: rejected, because whole datasets would drop out of analyses over time for an error that is small at the resolution those analyses use.
- Use only documentation supplied with the dataset: rejected, because the investigators' own description of their fieldwork dates the data more closely than a publication year.
- Always take the earliest publication year, ignoring a documented bound on collection: rejected, because where a publication shows that collection had ended in an earlier year, the earliest publication year is known to fall after that bound.

---

## Consequences

### Positive consequences

- Every dataset with a publication can contribute to analyses of change over time.
- Every inferred year cites the document, publication or bound it came from.

### Negative consequences or trade-offs

- A publication year, or the year of a received or submission date, is no earlier than the end of collection and often a year or more later, so rows dated this way are biased towards later years.
- Only some publications print a received or submission date, so the gap between collection and a fallback year differs between datasets dated this way.
- A single constant gives every wave of a dataset the same year, so change between its waves is not represented.
- Documented and publication-based years are both inferred, so rows dated by the fallback are identified only by their notes.

---

## Implementation

The `data_year` notes and missing rule in `harmonisation/dataschema.csv` state the order, and the harmonisation guide repeats it under "Age and time".
A dataset left NA under the previous rule takes its earliest publication year, or the year of an earlier documented bound, when it is next mapped or reviewed.

---

## Related decisions

NA
