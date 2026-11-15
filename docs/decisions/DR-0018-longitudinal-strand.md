# DR-0018: A longitudinal strand of lagged and within-child models

**Status:** Proposed  
**Date:** 2026-11-15  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The primary models are cross-sectional: each wave of each dataset is a unit, and the slope compares children with one another at one time.
They cannot say whether screen use comes before an outcome, and a slope may reflect standing differences between children and their families that the covariates miss.

Some datasets follow the same children over several waves.
The preregistration says longitudinal studies are included, and registers no longitudinal model.

---

## Decision

A secondary strand is fitted in datasets that measure a child at two or more waves.
For each exposure and outcome, and each pair of successive waves in which the outcome was measured, three models are fitted on children seen at both waves and pooled across datasets as the cross-sectional slopes are.
A pair starts from one unit of the earlier wave, so countries stay apart as in the primary models.

- **Lagged.** The outcome at the later wave on screen use at the earlier wave, adjusted for the outcome at the earlier wave and the covariates.
  Outcomes are standardised within their own wave.
- **Within-child.** The change in the outcome on the change in screen use between the two waves.
  Anything about a child that does not change between the waves cannot explain this slope.
  The change in the outcome is in units of its SD at the earlier wave, because scores standardised within each wave lose the change in the mean; the change in screen use is in hours.
  It also needs screen use at the later wave, so it rests on fewer children than the other two.
  A pair of waves is left out where the screen item changed between them, or where the data name the outcome's instrument and it differs.
- **Matched cross-sectional.** The primary model at the earlier wave, on the children the lagged model uses, so those two are compared on the same children.

The strand is labelled exploratory, takes Tier 5 q-values, and its models are rows of the model list fixed at the freeze.
It is fitted for outcomes modelled linearly; binary and short ordinal rows are not in it.

**How it is reported.**

- The three slopes side by side for each family and specific outcome, with the number of datasets and the typical gap between waves.
- The wording stays "associated with".
  A lagged or within-child slope removes some explanations and not others: something that changes over time and affects both screen use and the outcome is still there.
- A within-child slope smaller than the cross-sectional one is not by itself evidence that the cross-sectional slope is confounded.
  Error in measuring screen use pulls a slope of changes towards zero more than it pulls a slope of levels.
- The outcome at the earlier wave is itself measured with error, so adjusting for it removes only part of what it stands for, and a lagged slope keeps some of the cross-sectional association for that reason alone.
  The lagged model does not adjust for screen use at the later wave.
- Gaps between waves differ across datasets from months to years, and the slopes are reported against the gap.

---

## Rationale

The strand answers the most likely criticism of a pooled cross-sectional analysis with the data already in hand, in the same pipeline and on the same scale, so the comparison is direct.
Two simple models that every panel can support are preferred to one elaborate model that few can.

---

## Alternatives considered

- **Random-intercept cross-lagged panel models.** They need three or more waves and a structural model per dataset, which few datasets support and which would not pool cleanly.
- **Growth models over all waves.** They answer a different question, about trajectories.
- **Fixed-effects models over all of a child's waves.** With two waves this is the within-child model; with more it assumes one slope across unequal gaps and changing items.
- **Leaving longitudinal analysis to a later paper.** The comparison with the cross-sectional results is most useful beside them.

---

## Consequences

- The strand rests on a minority of the datasets, and they are not a random subset: panels differ from one-off surveys in country, period and age.
  The matched cross-sectional model shows how far that matters.
- Children who left a study before the later wave are not seen, so attrition is not accounted for.
- Most outcome rows do not record which instrument was used, so for those a change of instrument between waves goes undetected in the within-child model.
  The lagged model is not affected, because each wave is standardised on its own.
- A dataset's successive pairs of waves share children.
  The pooled interval allows for that, but with few panels its degrees of freedom are few, and many results will be marked as having too few studies to say.
- A wave with no screen item is not in the harmonised data, so a gap is the gap between waves that have one.
- The models are not preregistered, which the deviations ledger records.

---

## Implementation

- `fit_stage1_longitudinal()` fits the three models per pair of waves; `pool_longitudinal()` pools each.
- Model rows `longitudinal_lagged`, `longitudinal_within`, `longitudinal_matched`, and two not yet built: the slopes by gap between waves, and a check using imputed outcomes.
- `tests/testthat/test-analysis-models.R` holds a known-answer test in which a standing trait inflates the cross-sectional slope and the within-child slope recovers the effect built in.

---

## Related decisions

- DR-0009: the estimand.
- DR-0010: two-stage analysis.
- DR-0015: what the models adjust for.
