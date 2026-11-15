# DR-0017: The tiered false discovery rate, written out as a rule

**Status:** Proposed  
**Date:** 2026-11-15  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration controls false discoveries with a tiered procedure: Benjamini and Hochberg's adjustment, applied cumulatively, so that a test carries the number of comparisons that led to it.
It gives the tiers and the counts $k_0$, $k_{1,f}$ and $k_{2,f}$, but not a rule exact enough to compute from.
Two readings give different q-values, so the rule has to be fixed before any confirmatory result is seen.

The preregistration also predicts no association for some content classes and families, and a test of a difference from zero cannot support such a prediction.

---

## Decision

**Layers and paths.**
Tests are arranged in layers, each leading into the next.
A test's layer and every layer above it form its path.

| Test | Path |
| --- | --- |
| Overall screen use and a family (Tier 1) | families |
| A content class and a family (Tier 2) | families, then that family's content classes |
| A specific outcome | the path of its family's test for the same exposure, then that exposure's specific outcomes in the family |
| A specific outcome by informant, or an outcome row | the specific outcome's path, then its informants, or its rows |
| A secondary model of Tier 1 or 2, such as a content class adjusted for other screen use | the primary test's path at the same level, then a layer for that model |
| A threshold (Tier 3) | families, the family's content classes, then the family's thresholds |
| A moderation test (Tier 4) | the Tier 3 path, then the family's moderation tests |
| An exploratory or sensitivity analysis (Tier 5) | the Tier 4 path, then the family's exploratory tests |

**The rule.**
Tests that share a path are one group.
Within a group, q-values are Benjamini and Hochberg's with the number of comparisons set to K: the group's own tests plus the tests in every layer above it on the path.
The ranks come from the group's own p-values only.

- A primary test is one from a confirmatory model fitted as the main two-stage model with nothing varied; every other model of Tier 1 or 2 is secondary.
- The first layer always counts five, the five outcome families, whether or not each can be estimated.
- A layer with no test adds nothing.
- A test that could not be run has no p-value and is not counted.
- A test with too few studies to say keeps its p-value and is counted.

So Tier 1 has K = 5.
A content class in a family has K = 5 plus the content classes tested in that family.
A specific outcome under a content class adds the specific outcomes tested for that class in that family.

**What a q-value means.**
Tier 1 q-values control the false discovery rate among the five families.
Lower-layer q-values are a descriptive penalty and not a guaranteed error rate, and the reports say so.
Every estimate shows its raw p and its q.
Only families and specific outcomes of the confirmatory models are described as findings; informant-level and row-level estimates are labelled exploratory.

**Predictions of no association.**
Where the preregistration predicts no or a negligible association, the estimate is tested for equivalence: two one-sided tests against the smallest effect of interest, 0.10 SD on the contrast of 1 against 4 hours a day, using the robust standard error and its degrees of freedom.
The equivalence p-values of a group are adjusted among themselves with the group's K.
The test of a difference from zero is still reported for those rows, with its q-value.
An estimate is called equivalent only where it was pooled from enough studies to say.

**When K is counted.**
The rule is frozen with the model list.
K is worked out from the rule on the datasets in each run, so it can differ between the report to the funder and the publications, and the reconciliation lists each K that changed.

---

## Rationale

Ranking within a group and counting along the path is the reading that penalises a deeper test for the testing above it.
The other reading, one adjustment over every test down to a tier, lets very small p-values in Tier 1 lower the q-values of Tier 2, the opposite of the penalty the preregistration describes.
Take two families with Tier 1 p-values near zero and two content classes in one family with p-values of 0.015 and 0.04.
One adjustment over the four tests gives the content classes q-values of 0.02 and 0.04.
This rule gives them K = 5 + 2 = 7 and q-values of 0.105 and 0.14.

Fixing the first layer at five means a family that cannot be estimated does not make the others easier to pass.

---

## Alternatives considered

- **One Benjamini and Hochberg adjustment over all tests down to a tier.** Rejected for the reason above.
- **A hierarchical procedure that tests a layer only where its parent was rejected.** It has a guaranteed error rate, but it is not what was registered, and it reports nothing below a family that fails.
- **Counting K from the registries and not from the tests run.** K would include cells no dataset measures.
- **Sensitivity analyses unadjusted.** The preregistration places them in Tier 5.
- **The equivalence p-value in place of the difference p-value in the group's ranking.** The two answer different questions, and mixing them in one ranking would let one kind of test set the other's q-value.

---

## Consequences

- A specific outcome's q-value depends on how many specific outcomes its family has for that exposure, so families with many specific outcomes are penalised more.
- K grows with depth, but so does the number of tests ranked together, and Benjamini and Hochberg's adjustment multiplies a p-value by K over its rank.
  In a large group only the smallest p-values pay close to the full K, so a deeper test can take a smaller q-value than a shallower one with the same p-value.
  This is a property of the registered procedure.
  It is one reason lower-layer q-values are called a descriptive penalty, and a sensitivity analysis is read by whether the estimate moves, not by its q-value.
- Only the first layer has a fixed count, so a content class that cannot be estimated in a family lowers K for that family's other classes.
- Tiers 3, 4 and 5 each form one layer per family across exposures, levels and models, and K is counted over the models assembled in one table, so every model of a run is assembled together.
- An equivalence test needs a narrow interval, so few predictions of no association will be confirmed where datasets are few.

---

## Implementation

- `adjust_tiered_fdr()` applies the rule to p-values and paths; `tiered_fdr_path()` gives each test its path.
- `test_equivalence()` is the equivalence test.
- `assemble_results()` puts every model's pooled estimates in one table with p, q, K, the prediction and the equivalence test.
- `analysis/hypotheses.csv` holds the preregistered predictions by exposure and family.
- `tests/testthat/test-analysis-models.R` holds hand-worked examples.

---

## Related decisions

- DR-0009: the estimand.
- DR-0014: pooling up the outcome tree.
