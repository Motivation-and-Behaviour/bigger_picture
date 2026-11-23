# DR-0020: Pooled effects are put in the evidence platform's impact bands, read from the interval

**Status:** Accepted  
**Date:** 2026-11-23  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The report describes each pooled effect in plain words, and the evidence platform rates the size of a relationship in bands (see [Rating the evidence](../platform/rating-the-evidence.qmd)).
The bands and the words for them have to be fixed before the confirmatory models are run, so that how a result is described does not depend on what it turns out to be.
The platform's rubric is still provisional: it was used for the first published ratings and awaits the team's sign-off.

The rubric was written for rating published reviews, and its lowest band is "below 0.10, or not statistically significant".
For one pooled analysis that puts two different results in the same band: an effect shown to be negligible, and an effect too uncertain to say anything about.

---

## Decision

**The bands are the rubric's, as they stand.**
They are applied to the effect on the contrast of 1 against 4 hours of screen use a day, in SD of the outcome.

| Rating | Words | Size on the contrast |
| --- | --- | --- |
| 0 | Negligible or no relationship | shown to be within 0.10 |
| ±1 | Small | 0.10 to 0.39 |
| ±2 | Moderate | 0.40 to 0.69 |
| ±3 | Strong | 0.70 or more |

The sign is the direction for the child: negative is harmful and positive is beneficial.
The lower edge of the small band is the smallest effect of interest, 0.10 SD (DR-0017).
A band holds its lower edge, so an estimate of exactly 0.40 is moderate, and bands are read from the estimate as computed, not as rounded for display.
The contrast is three times the linear slope per hour, not a reading of the pooled curve (DR-0016).
Binary and short ordinal outcomes enter on the SD scale they are pooled on (DR-0014).

**A rating is read from the estimate and its interval, not from a p-value alone.**

- **0, negligible or no relationship:** the equivalence test shows the effect lies within the smallest effect of interest.
  That test is passed where the 90% interval lies within 0.10 either side of zero, so the 95% interval shown beside the estimate can reach a little beyond it.
- **±1 to ±3:** the estimate is at least 0.10 and its 95% interval excludes no difference; the band is taken from the size of the estimate.
  The rubric calls this band "no clear relationship"; here an effect can be clearly different from zero and still be shown to be negligible, so the words say both.
- **Not rated:** anything else.
  The label says why: too uncertain to say, too few studies to say, not estimated, or a family whose specific outcomes disagree.
- **The longitudinal strand is not rated.**
  Its slopes are other estimands, and a null lagged or within-child slope is not evidence of no effect (DR-0018).

Ratings are given to families, specific outcomes and specific outcomes by informant.
An estimate for a single outcome row is in that row's own direction and is not rated.

**The words.**
Plain text follows the platform's writing guide: "is linked with" or "goes with" for these associations, the size given as small, moderate or strong, and "significant" never used.
A result that is not rated is described as too uncertain to say, never as no relationship.

The bands are frozen as provisional.
If the team's sign-off changes them, the change is a deviation recorded in the ledger, and the report says which bands it used.

---

## Rationale

Reading the band from the interval keeps apart what the rubric's lowest band joins.
With the equivalence tests already in the specification, the lowest band can be reserved for effects the data show to be negligible, and the rest left unrated.

The contrast of 1 against 4 hours is not a standardised difference between two groups, so applying the rubric's bands for standardised differences to it is a convention.
It is a coherent one: the lowest band's edge is the preregistered smallest effect of interest on the same contrast.

---

## Alternatives considered

- **The rubric's lowest band as written**, with every non-significant result rated 0. It would report an imprecise estimate as no relationship.
- **Bands from a published scheme of effect sizes.** The lowest band would no longer be the smallest effect of interest, and the platform's published ratings use the rubric.
- **Bands on the slope per hour.** The same bands divided by three; the contrast is what the report shows.

---

## Consequences

- Most pooled effects in this field are small, so most rated results will be ±1, and many results will not be rated.
- **The rating describes size, and the q-value decides support.**
  A registered directional prediction is supported where the family estimate of its primary model is in the predicted direction with a q-value below 0.05; a prediction of no association is supported where that estimate is shown equivalent with its adjusted p-value (DR-0017).
  A result that is rated with a q-value at or above 0.05 is described as a link of that size that does not hold once the number of comparisons is allowed for.
- A result can have a q-value below 0.05 and not be rated, where its estimate is under 0.10 and it does not pass the equivalence test; and a result can be rated and have a q-value above 0.05, because the rating reads the 95% interval and the q-value carries the tiered count.
  The report gives both.
- The rating uses the unadjusted equivalence test; whether a registered prediction of no association is confirmed uses the adjusted one (DR-0017).

---

## Implementation

- `rate_effect()` gives the rating and its words; `assemble_results()` adds them to the results table as `rating` and `rating_label`, with `prediction_supported` for the family estimate of each primary model.
- `analysis_settings()` holds the bands as `rating_bands`.
- `tests/testthat/test-analysis-models.R` holds worked examples.

---

## Related decisions

- DR-0017: the tiered false discovery rate, the smallest effect of interest and the equivalence tests.
- DR-0014: pooling up the outcome tree.
