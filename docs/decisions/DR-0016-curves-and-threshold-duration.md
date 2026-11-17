# DR-0016: Curves are pooled from splines with fixed knots, and the threshold duration is read from the pooled curve in two ways

**Status:** Accepted  
**Date:** 2026-11-13  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration asks how much screen use is too much, and proposes a piecewise model.
The analysis plan defines two quantities: the breakpoint of that model, and the lowest duration at which the pooled curve's interval lies beyond the smallest effect of interest relative to a low-use reference.
It also asks that the method be settled by a simulation on real hours of screen use before the specification is frozen, and allows the breakpoint to follow the freeze if the simulation has not settled it.

Three features of the data make this hard.
Many datasets ask about screen use in bands.
Few children report long hours, so some units have almost nothing to say about them.
And most exposure and outcome pairs are measured by few datasets.

---

## Decision

**The curve.**
Each unit fits a natural cubic spline of hours of screen use, with interior knots at 1, 2 and 4 hours and boundary knots at 0 and 6, in the linear stage 1 model at covariate tier 1.
The knots are fixed in hours, so every unit estimates the same terms.
A unit is fitted only with at least 30 rows in every stretch between knots, more distinct values of screen use than the curve has terms, and more clusters than terms.

The terms are pooled across datasets in a multivariate random-effects model, each term varying between datasets and between a dataset's units, with inference robust to the dependence of a dataset's units.
The pooled curve is read every half hour as the difference from one hour a day, in SD of the outcome.

**Where a curve is read.**
A unit supports an hour where it has at least 30 rows within half an hour of it.
The curve is interpretable at an hour between the boundary knots that at least three datasets support, counting only datasets that also support the reference.
Beyond six hours the curve is a straight line continued, and is not read.
Rows above six hours still help to place the curve below it: in one cell they are 3% of rows, and leaving them out or capping them moved a threshold by half an hour to an hour, so that is a sensitivity analysis to add.

**The threshold duration** is read from the pooled curve in two ways, and both are reported.
Each is the lowest interpretable hour above the reference that meets its rule.

- **Conservative:** the 95% interval lies wholly beyond 0.10 SD from the reference, in either direction, with at least three robust degrees of freedom at that hour.
- **Central:** the estimate itself is beyond 0.10 SD from the reference, the 95% interval excludes no difference and there are at least three robust degrees of freedom, at that hour and at the next half hour too (amended 17 November 2026: the first version read it from one hour alone, with no floor on the degrees of freedom).

Each is reported with its direction (a better or a worse outcome) and with whether the curve stays beyond at every later hour: for the conservative reading, whether the whole interval does.

**How they are reported.**

- The pooled curve with its interval, and the number of datasets supporting each hour, is the primary result.
- The conservative threshold is the one to rely on: it reads "by this many hours", never as the point at which harm begins, because it is usually later than the true crossing.
- The central threshold is an alternative reading, given beside it: an estimate of where the curve crosses 0.10 SD, which is reported for about a quarter of curves that in truth level off just short of it, and can sit half an hour either side of the true crossing.
- Where neither is reported, the report says that no threshold could be established, not that there is none.
- Thresholds are not compared between exposures or outcomes without their curves and intervals, because the same curve gives a later conservative threshold where datasets are fewer or differ more.
- Curves with a sharp local feature, a bend or a dip, are described cautiously near that feature, where the interval covers the truth less often than it claims.

**The breakpoint** of the piecewise model is built after the freeze, as the plan allows.

---

## Rationale

The method was tested by replacing the outcome of one real cell with a simulated one that follows a known curve, keeping the real hours of screen use, ages, sexes and sizes of its 85 units in 14 datasets, 59 of them with banded items.
Datasets were made to differ in the strength of the curve, in where its shape sits along the hours, and by a slope of their own.
Eight curves were built in: flat, a straight line, a bend at two hours and at three, curves levelling off just short of and just past 0.10 SD, a dip that recovers, and a rise then a fall.
Each was run 200 times, with and without error inside the bands, with two knot placements, with and without the rule on rows per stretch, and with all of the cell's datasets or five drawn at random from those the rule leaves.
With the rule on rows, nine of the cell's 14 datasets have a unit to pool, so "all" below is nine datasets.
The truth each pooled curve is scored against is the mean of the datasets' curves.

The figures below were corrected on 17 November 2026 after an independent review: the first version of this record misquoted three of them, scored bends against the curve as written and not the mean curve the simulation generates, and called "five datasets" a scenario that pooled about three.

What it showed, with three interior knots and the rule on rows:

- **The curve is recovered.** For the flat, straight and levelling-off curves the interval covered the truth 84 to 98% of the time at every hour from half an hour to six, lowest at half an hour, below the reference.
  For bends at two and three hours it covered 88 to 98%, and for a rise then a fall 87 to 96%.
- **A dip is smoothed.** On the way down into a dip coverage fell to 44% at an hour and a half and 71% at two hours, with errors of up to 0.04 SD, and was 93% or more from its lowest point on.
- **A false conservative threshold is rare.** One was reported where none exists, or more than a quarter of an hour early, in at most 2% of runs of any scenario (a rise then a fall, with error inside the bands), and for a flat curve in none of the runs with nine datasets and one in 200 with five.
- **A real threshold is found late by the conservative rule, or not at all.** With nine datasets, a curve bending at two hours had its threshold found in 95% of runs, a median hour late.
  A straight line was found in 63%, an hour and a half late.
  A curve levelling off past 0.10 SD was found in 14%.
- **Five datasets give a conservative threshold about half as often.** With the floor of three robust degrees of freedom, five datasets found the bend at two hours in 53% of runs and the straight line in 25%, as late as nine datasets did.
  Their curves' intervals covered the truth 88 to 99% of the time away from a dip.
  Under the first version of the rule, with a floor of four, five datasets never gave one.
- **The central threshold is near the true crossing on average, and is wrong often enough to matter.**
  With nine datasets it was found in 74 to 99% of runs of every curve with a threshold, with a median at the true crossing or within half an hour after it.
  It was reported more than a quarter of an hour early in 4 to 22% of those runs, and in up to 38% with error inside the bands.
  It was reported for a flat curve in 2% of runs, and for a curve that levels off at 0.08 SD in 28%.
  From five datasets it was found in 30 to 77% of runs, with a flat curve reported in 1% and the curve levelling off at 0.08 SD in 14%.
  Asking for two half hours in a row and a floor on the degrees of freedom halved the reports for a flat curve and did not help the curve that levels off just short: it sits beside the line for hours, and an independent simulation found it reported about a quarter of the time whether five, ten or twenty datasets were pooled.
- **Three interior knots follow a bend better than two** (coverage at a bend at two hours 94% against 70%).
  For a dip the two placements each do better at some hours and worse at others.
- **The rule on rows per stretch** costs units (9 of the cell's 14 datasets remain) and changed coverage little in the simulation, but without it a unit with a handful of children at long hours is fitted with a standard error far too small.
- **Error inside the bands** moved the curve by 0.02 SD or less at most hours.

So the curve can carry the result, and the threshold duration is a conservative summary of it that will be silent for most pairs.

---

## Alternatives considered

- **Two interior knots, at 1 and 3 hours.** One term fewer, but it cannot follow a bend near two hours.
- **Fitting every unit a curve can be fitted in.** More datasets, at the cost of curve terms resting on a few children.
- **Reading the curve to eight hours.** Beyond the last knot the curve is extrapolated.
- **One threshold only.** The conservative rule alone is silent for most pairs; the central rule alone overstates how well the crossing is known.
- **A threshold from the point estimate crossing 0.10 SD, with no condition on the interval.** In an earlier run of the simulation it reported a threshold for a flat curve about twice as often as the central rule.
- **Waiting for the breakpoint before the freeze.** The plan allows it to follow.

---

## Consequences

- Few exposure and outcome pairs will report a conservative threshold: it needs more than five datasets, in practice about ten, and a curve that clearly passes 0.10 SD.
  More will report a central one, and a reader must not take the two as equally firm.
- The simulation used one cell, unweighted fits and normal outcomes, and the mild kind of band error: recorded hours that are right on average.
  It did not test weights or clustered errors, outcomes on five-point scales, answers that misreport hours, or confounding by age that is not smooth.
- The knots, the reference hour and the 30-row rules are settings fixed at the freeze.

---

## Implementation

- `fit_stage1_curves()`, `curve_basis()` and `pool_curve()` in `R/analysis/`.
- `simulate_curve_recovery()` and `summarise_curve_recovery()` run the simulation; `curve_truths()` lists the built-in curves.
- `tests/testthat/test-analysis-curves.R` holds the known answers.
- Not yet built: the breakpoint, response format as a moderator of the curve, the sensitivity analysis of top-band scoring, and curves of the derived total within one source of the total.

---

## Related decisions

- DR-0009: the estimand.
- DR-0010: two-stage analysis.
- DR-0015: what the models adjust for.
