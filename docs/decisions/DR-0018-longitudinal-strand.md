# DR-0018: A longitudinal strand of lagged and within-child models

**Status:** Accepted  
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
For each exposure and outcome, and each pair of waves in which the outcome was measured, three models are fitted on children seen at both waves and pooled across datasets as the cross-sectional slopes are.
A pair starts from one unit of the earlier wave, so countries stay apart as in the primary models.

- **Lagged.** The outcome at the later wave on screen use at the earlier wave, adjusted for the outcome at the earlier wave and the covariates.
  Outcomes are standardised within their own wave.
- **Within-child.** The change in the outcome on the change in screen use between the two waves.
  Something about a child that does not change between the waves cannot explain this slope, unless its influence on screen use and the outcome itself changes with age.
  The change in the outcome is in units of its SD at the earlier wave, because scores standardised within each wave lose the change in the mean; the change in screen use is in hours.
  It also needs screen use at the later wave, so its pairs can span further than the other two models' and it rests on fewer children.
  A pair of waves is left out where the screen item changed between them, where the data name the outcome's instrument and it differs, or where the dataset's exceptions record that the outcome was asked differently at one of the two waves; and a child is left out of a pair where their screen use was reported by a different respondent, or in a different response format, at the two waves.
- **Matched cross-sectional.** The primary model at the earlier wave, on the children the lagged model uses, so those two are compared on the same children.

A pair is one unit of the earlier wave and a later wave, on the children of the unit seen again there.
Each child goes to their own next wave: the first later wave at which the child has what the model needs (amended 18 November 2026: the first version chose one next wave for the whole unit, which dropped the children of a cohort or a subsample followed up at another wave).
Children of a unit who share a next wave are one pair where there are at least 100 of them (amended 20 November 2026: the first minimum was the 30 a unit needs, and pairs that skip a wave were then small groups of the children who missed it, with slopes several times noisier), so a unit can have more than one pair from the same earlier wave, no child is in two of them, and a pair can skip waves.
Children whose next wave too few of the unit share are left out and counted, and a unit with no pair is recorded as having none.

The strand is labelled exploratory, takes Tier 5 q-values, and its models are rows of the model list fixed at the freeze.
It is fitted for outcomes modelled linearly; binary and short ordinal rows are not in it.

**How it is reported.**

- The three slopes side by side for each family and specific outcome, with the number of datasets and the typical gap between waves.
- The wording stays "associated with".
  A lagged or within-child slope removes some explanations and not others: something that changes over time and affects both screen use and the outcome is still there.
- A within-child slope smaller than the cross-sectional one is not by itself evidence that the cross-sectional slope is confounded.
  Error in measuring screen use pulls a slope of changes towards zero more than it pulls a slope of levels.
- **A lagged slope smaller than the cross-sectional one is not evidence that screen use comes first.**
  The outcome at the earlier wave stands in, only in part, for whatever lasting differences between children lie behind both screen use and the outcome.
  In simulations with a lasting trait and no effect of screen use, the lagged slope kept between a twentieth and nine tenths of the cross-sectional slope at a gap of one wave, depending on how stable and how reliably measured the outcome is: the less the outcome at one wave predicts it at the next, the more is kept.
  These are examples and not bounds.
  So the ratio of the two slopes says nothing by itself, and a lagged slope at about half the cross-sectional one with a within-child slope near zero is among the things lasting confounding with no effect would give.
- **Each model is blind to some truths, and none of them alone settles direction.**
  If screen use affects the outcome only at the same time, the lagged slope is roughly that effect times how stable screen use is, less a part that grows with how stable the outcome is: it can be most of the effect, near zero, or of the opposite sign.
  A null lagged slope is therefore not evidence of no effect, and a lagged slope is not evidence of a delayed one.
  If screen use affects the outcome only later, the within-child slope can take the wrong sign, and the lagged slope is too small where the outcome or screen use is stable.
  If the outcome affects later screen use, the within-child and cross-sectional slopes can have opposite signs.
  Something that changes over time and affects both screen use and the outcome biases the within-child slope too, and can bias it more than the cross-sectional one.
  The three slopes are read together.
- The lagged model does not adjust for screen use at the later wave.
- Gaps between waves differ across datasets from months to years, and each family and specific outcome is reported with its typical gap and the shortest and longest dataset's, taken as the median over a pair's children, from their ages where those are recorded finely and from collection years otherwise.
  The ages are used where they are recorded finely, or where every child's age moves by the same part of a year; a pair whose ages and collection years disagree by more than 0.3 years is marked.
  Where an outcome is measured more often than screen use, the lagged and within-child slopes of one dataset can sit on different gaps.
- The three slopes do not rest on the same children or the same datasets.
  The matched cross-sectional slope is on the lagged model's children.
  The within-child slope needs screen use at the later wave, the same item at both waves and the same respondent, so it has fewer datasets (a child whose respondent is recorded at one of the two waves only is left out; where a dataset records none, the respondent cannot be checked), and in a panel that moves from parent report to self report as children grow it rests on the younger children.
- A child is in a pair only where they have what the model needs at a later wave, and children who left the study are not seen.
  Each stage 1 row counts the unit's children at the earlier wave, those paired, those left out because too few shared their next wave, and those with no later wave, who left the study or were not asked the outcome again.
  For the within-child model a later wave at which the screen item or the outcome is recorded as changed is not a child's next wave: the child goes on to the next comparable one (amended 20 November 2026), so the pair that skips such a wave is on everyone seen after it and not on the few who missed it.
  Separately, for each panel and each pair of successive waves that ask about screen use, how many children have the item again and how their screen use and age differ from the others' is computed per dataset (`panel_pairs_all`).
  That table describes follow-up of the screen item and not the models' pairs: a child without the item at the later wave may have left, aged out of it, or belong to a cohort or a subsample not asked it.

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
- Most outcome rows do not record which instrument was used, and the check compares the instrument's label and not its items, so a change of instrument or of an item's wording between waves can go undetected in the within-child model.
  The lagged model is not affected, because each wave is standardised on its own.
- A dataset's successive pairs of waves share children.
  The pooled interval allows for that, but with few panels its degrees of freedom are few, and many results will be marked as having too few studies to say.
- A few panels carry most of each family's estimate, and the item-change checks leave out about a fifth of the within-child pairs.
- The lagged model's later wave needs the outcome and not the screen item, so its gap can run to a wave that did not ask about screen use.
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
