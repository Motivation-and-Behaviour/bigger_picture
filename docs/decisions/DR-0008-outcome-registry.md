# DR-0008: The outcome registry decides which outcomes go forward and how they combine

**Status:** Accepted  
**Date:** 2026-11-11  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

Harmonisation was deliberately broad, so the dataschema holds hundreds of outcome rows, many of them in only a few datasets.
The preregistration names five outcome families and a set of specific outcomes, with example measures, and says the outcomes will include "at least" these.
The schema's domains do not always match the preregistration's families: rated attention, emotion regulation, the BRIEF, the SDQ total and the HBSC symptom checklist each sit in a different place in the two.

Without masking, the choice of which outcomes go forward, and how rows combine into outcomes, has to rest on content and coverage and be fixed before results are seen.
The registry also has to survive schema changes, which add rows in batches.

---

## Decision

**Levels.**
Outcomes have four levels: a family (L0, the preregistration's main outcomes), a specific outcome (L1, the main reporting level), a specific outcome by informant (L2) and the dataschema row (L3).
`analysis/outcome_tree.csv` holds the families and specific outcomes, and `analysis/outcome_rows.csv` places every outcome and health row of the dataschema.
Stage 2 pools effects upward through the tree; scores are never merged into a composite.

**Families follow the preregistration where it is explicit.**

- Rated attention, hyperactivity and ADHD symptom scales are behaviour; attention tasks are cognition (executive function).
- Emotion regulation is mental health, as the preregistration lists an emotion regulation questionnaire there.
- Rated executive function, the BRIEF and the other rating scales, is behaviour (self-regulation), as the preregistration lists the BRIEF there; executive-function tasks stay in cognition.
- The SDQ total difficulties and other total-problems scores are behaviour, as the preregistration lists the SDQ there.
- The HBSC psychological-complaints items are mental health (psychosomatic complaints), as items of the symptom checklist.
- Substance use, truancy and bullying are behaviour, as outcomes the preregistration's "at least" allows.

**Rows that do not pool.**
Broadband totals (internalising, externalising, total problems) are their own specific outcomes and never pool with their subscales.
A row marked `part_of` a whole from the same instrument gives way to the whole where a dataset has both in one node.
Diagnosis rows, problematic screen use, temperament, emotion-regulation strategies, body image and homework time are reported individually and stay out of pooling.

**Which outcomes go forward.**
Each row has a `use`, set from content and coverage only and fixed at the freeze: `core` for a pooled row in a specific outcome with 3 or more datasets, `report` for a row reported but not pooled, and `archive` for rows not analysed this round.

**Direction.**
A continuous row's direction refers to its values.
A categorical row is scored by its level's position in the dataschema's `allowed_values` (first level 1), and a binary row scores `TRUE` or `Yes` as 1; its direction refers to that score.

**Headline scope for the initial report.**
The specific outcomes the preregistration names, plus any other specific outcome with 15 or more datasets, and five exposures: total screen time, watching, gaming, social media and educational use.

**Derived rows count.**
A dataset counts towards an outcome or exposure if the row is mapped or can be derived from rows the dataset has.
`analysis/derivations.csv` holds the rules: general ones where the dataschema defines an instrument's scoring (the SDQ composites, the PedsQL summaries), and per-dataset ones where the scoring belongs to that dataset's instrument.
A derived value only fills a row the dataset has not mapped, and needs all its parts on the same participant and wave.
Total screen time counts where a dataset has a dedicated item or both watching and gaming, the minimum for a summed total.
Wholes that cannot be rebuilt from their rows are not derived: normed composites such as the NIH Toolbox, and checklists whose items the dataschema holds only in part.
The CBCL has no rule yet: its syndrome weights depend on the form, and no dataset with all the syndromes lacks the released composite.

**Schema changes.**
`sync_outcome_registry()` adds new dataschema rows under the placeholder node `unassigned`, or `post_freeze` after the freeze, and a test fails if any outcome or health row is missing, so a new row cannot silently drop out of the analysis.
Rows added after the freeze cannot enter a family or specific outcome of the frozen analysis.

---

## Rationale

- Following the preregistration where it is explicit settles each conflict once, by a rule that predates the data.
- Pooling effects rather than scores keeps each instrument's meaning, and letting a whole replace its parts avoids counting one score twice.
- Setting `use` from content and coverage, and fixing it at the freeze, keeps the choice of outcomes independent of results.
- A sync and a completeness test keep the registry in step with the schema without hand-editing.
- At 3 or more datasets the headline scope would cover 47 of 48 specific outcomes; 15 keeps the outcomes with the widest coverage, each of the five it adds over a cut-point of 20 having at least 5 datasets with every headline exposure.
- The harmonisation leaves a composite unmapped when its parts are mapped, so counting mapped rows alone would understate what the analysis can use: internalising and externalising would have 10 and 12 datasets rather than 28 and 29.
- A derived row applies one instrument's own scoring to its own subscales, so it is not the cross-instrument composite this record rejects.

---

## Alternatives considered

- Families from the schema prefix: rejected, because the schema's domains and the preregistration's families disagree for several rows.
- A person-level composite per specific outcome: rejected, because composites change meaning with each study's mix of instruments.
- A headline scope of every specific outcome with 3 or more datasets, or with 20 or more: rejected as too wide and too narrow.
- An explicit level-order column for categorical rows: rejected as more to maintain than the dataschema's own order.
- Counting only mapped rows towards the headline scope: rejected, because it penalises studies whose composites are left to the analysis.

---

## Consequences

### Positive consequences

- The tree structures the models, the reports and the tiers of the multiple-comparison procedure in one place.
- New schema rows arrive in the registry automatically, waiting for a decision.

### Negative consequences or trade-offs

- A frequency item listed from "Always" to "Never" scores higher for less often, so a symptom item can be `higher_better`; the direction is right but reads oddly.
- `analysis_data` stores categorical levels in byte order, so ordinal models must take level order from `allowed_values`, never from the factor.
- Every schema change that adds an outcome, health or screen-time row also needs the registries synced.
- The headline flags reflect the coverage of the build they were set from (11 November 2026), and six specific outcomes sit at 13 or 14 datasets, so later merges can add to them.

---

## Implementation

```md
analysis/outcome_tree.csv, analysis/outcome_rows.csv, analysis/derivations.csv, analysis/README.md
R/analysis/sync_outcome_registry.R, outcome_row_defaults.R
R/analysis/read_outcome_registry.R, validate_outcome_registry.R
R/analysis/read_derivations.R, validate_derivations.R
tests/testthat/test-analysis-registry.R
targets: outcome_tree_file, outcome_rows_file, outcome_registry, derivations_file, derivation_registry
```

---

## Related decisions

```md
DR-0004: Total screen time comes only from a dedicated item
DR-0007: The analysis runs in this repository's pipeline
```
