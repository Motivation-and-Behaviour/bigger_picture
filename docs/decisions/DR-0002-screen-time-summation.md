# DR-XXXX: Short descriptive title

**Status:** Proposed
**Date:** 2026-03-19  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

We deliberately chose to only include studies which disaggregated screen time.
We did this because the evidence clearly supports that it is the type (or content) of screen time which is likely associated with outcomes, rather than the just the raw level.

The issue is if and how to combine these disaggregated measures.
Since children can engage in more than one behaviour at time, simply summing categories risks creating 'total' scores which don't represent actual time.
At the same time, some studies have more nuanced measures than others (e.g., 'gaming' vs 'gaming on console' and 'gaming on computer').
Not combining categories reduces the set of harmonisable data.

---

## Decision

There are two sub-rules:

* Broad categories (e.g., TV and Video Games) should not be summed.
* Overlapping categories ('gaming on console' and 'gaming on computer') can assumed to be independent and summed to a broader category ('gaming').
  Decisions to sum categories should always be documented in each studies harmonisation files.

Ideally, the more nuanced categories would still be retained for potential sub-analyses.

---

## Rationale

This option retains the most amount of usable information in the study, while aiming to reduce additional noise.

---

## Alternatives considered

* Retaining distinct categories – rejected because it reduces harmonisable data.
* Summing overlapping categories – rejected because it risks creating vastly overestimated screen use estimates.

---

## Implementation

Methods for combining overlapping categories should be documented in each study's `variables.csv` file.

---

## Related decisions

NA
