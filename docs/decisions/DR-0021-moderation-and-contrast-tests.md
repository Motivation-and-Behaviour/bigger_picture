# DR-0021: How the registered moderation and contrast hypotheses are tested

**Status:** Proposed  
**Date:** 2026-11-24  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

Four registered hypotheses are tested by models that are listed in the model list and not yet built: that content type moderates the association (H2), that location does not (H4), that the association with social media is stronger for girls (H5b), and that associations are stronger for children from lower socioeconomic backgrounds (H5c).
The models are built after the specification is frozen, so the statistic each one tests has to be fixed first.
The model list names each model's exposures, families and tier; this record fixes the estimand, the test and what counts as support.

---

## Decision

**What the four have in common.**
Each is estimated within a unit, as a difference between two slopes fitted to the same children, and the differences are pooled across datasets exactly as slopes are: up the outcome tree, with robust inference clustered on dataset (DR-0010, DR-0014).
Differences are aligned as slopes are, positive where the first-named slope is better for the child, and are in SD per hour a day.
A difference is estimated in a unit only where each of the two slopes would be fitted on its own, and a pooled difference needs the datasets and degrees of freedom a pooled slope needs.
Each takes the covariates of the first tier.
Whether a prediction is supported is judged on the family estimate, with its q-value from the tiered procedure (DR-0017).

**H5b, sex.**
The model is the stage 1 model with an interaction between screen use and sex, fitted to girls and boys; a child recorded as another sex is left out of this model.
The estimand is the girls' slope less the boys' slope.
A unit needs the minimum number of rows among girls and among boys.
The prediction is supported where the family estimate is negative, so the association is more harmful for girls, with a q-value below 0.05, for social media and each of the mental health and wellbeing families.

**H5c, socioeconomic status.**
Children are put in three groups by the unit's socioeconomic measure, the one the second covariate tier uses in that unit: thirds of its distribution within the unit where it is ordered with five or more values, and its own lowest, middle and highest categories where it has fewer.
The model is the stage 1 model with an interaction between screen use and the group, scored −1, 0 and 1 from lowest to highest, and without the socioeconomic covariate itself beyond that score.
The estimand is the slope in the lowest group less the slope in the highest, which is minus twice the interaction coefficient.
A unit needs the minimum number of rows in the lowest group and in the highest.
The prediction is supported where the family estimate is negative, so the association is more harmful in the lowest group, with a q-value below 0.05, for each of the learning, mental health and behaviour families.
The slopes in the three groups are reported beside it.

**H2, content type.**
For each pair of content classes a unit measures on the same children, one model holds both classes, and the estimand is the difference between their two slopes, each adjusted for the other, with its standard error from that model.
The pairs are pooled separately.
For each family, the hypothesis that content type moderates the association is tested once, by a robust small-sample test that the pooled differences from one reference class (watching, or the first class the family has) are all zero, in the datasets that measure two or more classes.
The prediction is supported where that test's q-value is below 0.05.
The pairwise differences are reported with their intervals and are not separate confirmatory tests.

**H4, location.**
In a unit that asks about a content class at school and about the same content class anywhere on weekdays, of the same children, the two slopes are estimated together on those children, each without the other in its model, with their joint covariance clustered on the child.
The estimand is the school slope less the any-location slope.
It is a part against the whole it is part of, because no dataset records use at home alone.
The prediction of no moderation is tested for equivalence against the smallest effect of interest on the difference, 0.10 SD on the contrast of 1 against 4 hours a day.
It is reported as insufficient evidence where fewer than three datasets have both rows.

**Elsewhere.**
The same models fitted to other exposures and families (`moderation_sex_other`, `moderation_ses_other`, `moderation_location_other`) use the same estimands and are exploratory.

---

## Rationale

A difference between two slopes estimated on the same children within a unit is not confounded by what differs between studies, which a comparison of pooled slopes across datasets would be.
Pooling the differences as slopes are pooled keeps one method, one set of thresholds and one multiple-testing rule.

Three groups within a unit make socioeconomic position relative to the study's own children, since the measures differ between studies and cannot be put on one scale.
A linear score across the groups gives one test of a graded difference where two separate contrasts would give two.

One test per family for content type matches a hypothesis that says content type matters, without naming which pair differs.

---

## Alternatives considered

- **Comparing pooled slopes between subgroups of studies.** Confounded by everything that differs between studies.
- **Socioeconomic status as a continuous moderator.** The measures are not on one scale across studies, and many are categories.
- **A separate confirmatory test for every pair of content classes.** Six tests per family for one hypothesis.
- **Location as home against school.** No dataset records use at home alone.

---

## Consequences

- Few datasets measure two content classes, or a class both at school and anywhere, on the same children, so these tests will often have too few studies to say.
- The H4 comparison exists for one content class, educational use, and is likely to be reported as insufficient evidence.
- An interaction needs enough children in each group within a unit, so smaller units drop out of these models that are in the main ones.
- The difference between a part and its whole is smaller than the difference between two separate settings would be, which makes the H4 prediction of no difference easier to meet; the report says so.

---

## Implementation

To be built after the freeze, as rows `moderation_sex`, `moderation_ses`, `tier2_content_contrast` and `moderation_location` of `analysis/models.csv`, with their `_other` partners.

---

## Related decisions

- DR-0010: stage 2 pooling.
- DR-0014: pooling up the outcome tree.
- DR-0017: the tiered false discovery rate and equivalence tests.
- DR-0020: rating bands and when a prediction is supported.
