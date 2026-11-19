# DR-0016: Curves are pooled from splines with fixed knots, and the threshold duration is read from the pooled curve in two ways

**Status:** Accepted  
**Date:** 2026-11-13 (amended 2026-11-17, 2026-11-19 and 2026-11-20)  
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
A unit is fitted only with at least 30 rows in every stretch between knots, the last stretch running from four hours to six, more distinct values of screen use than the curve has terms, and at least ten clusters where its errors are clustered.

The terms are pooled across datasets in a multivariate random-effects model, each term varying between datasets and between a dataset's units, with inference robust to the dependence of a dataset's units.
The robust standard error at an hour is never taken below the working model's (amended 17 November 2026): with few datasets it was sometimes smaller than the units' own sampling error allows, and intervals there covered the truth about four times in five.
The pooled curve is read every half hour as the difference from one hour a day, in SD of the outcome.

**Where a curve is read.**
A unit supports an hour where it has at least 30 rows within half an hour of it.
A curve is pooled only from five or more datasets (amended 17 November 2026): from three or four, the curve itself moved by up to 0.10 SD with the choice of working model.
The curve is interpretable at an hour between the boundary knots that at least three datasets support, counting only datasets that also support the reference.
Beyond six hours the curve is a straight line continued, and is not read.
Rows above six hours still help to place the curve below it: in one cell they are 3% of rows, and leaving them out or capping them moved a threshold by half an hour to an hour, so that is a sensitivity analysis to add.

**The threshold duration** is read from the pooled curve in two ways, and both are reported.
Each is the lowest interpretable hour above the reference that meets its rule.

- **Conservative:** the 95% interval lies wholly beyond 0.10 SD from the reference, in either direction, with at least three robust degrees of freedom at that hour.
- **Central:** the estimate itself is beyond 0.10 SD from the reference, the 95% interval excludes no difference and there are at least three robust degrees of freedom, at that hour and, on the same side, at the next interpretable hour too (amended 17 November 2026: the first version read it from one hour alone, with no floor on the degrees of freedom).
  It therefore cannot fall on the last interpretable hour.

Each is reported with its direction (a better or a worse outcome), with whether the curve's estimate stays beyond 0.10 SD on the same side at every later hour, and with whether its interval goes on meeting the rule there.

**How they are reported.**

- The pooled curve with its interval, and the number of datasets supporting each hour, is the primary result.
- The conservative threshold is the primary threshold, and the only one in the main text: it reads "by this many hours", never as the point at which harm begins, because it is usually later than the true crossing.
- The central threshold is descriptive and goes in an appendix, with its caveats beside it (amended 17 November 2026: it was first given beside the conservative one).
  It estimates where the curve crosses 0.10 SD.
  It is reported for about a quarter of curves that in truth level off just short of that, however many datasets are pooled, it is more than a quarter of an hour early for up to a fifth of curves that do cross, and it can turn on a difference in the third decimal place.
  It is never described as a finding, never compared between exposures or outcomes, and takes no part in the confirmatory tests.
- Where no conservative threshold is reported, the report says that no threshold could be established, not that there is none.
- Thresholds are not compared between exposures or outcomes without their curves and intervals, because the same curve gives a later conservative threshold where datasets are fewer or differ more.
- Curves with a sharp local feature, a bend or a dip, are described cautiously near that feature, where the interval covers the truth less often than it claims.

**The breakpoint** of the piecewise model is built after the freeze, as the plan allows.

---

## Rationale

The method was tested by replacing the outcome of one real cell with a simulated one that follows a known curve, keeping the real hours of screen use, ages, sexes and sizes of its 85 units in 14 datasets, 59 of them with banded items.
Datasets were made to differ in the strength of the curve, in where its shape sits along the hours, and by a slope of their own.
Eight curves were built in: flat, a straight line, a bend at two hours and at three, curves levelling off just short of and just past 0.10 SD, a dip that recovers, and a rise then a fall.
Each was run 200 times, with and without error inside the bands, with two knot placements, with and without the rule on rows per stretch, and with all of the cell's datasets or five drawn at random from those the rule leaves.
With the rule on rows, eight of the cell's 14 datasets have a unit to pool, so "all" below is eight datasets.
The truth each pooled curve is scored against is the mean of the datasets' curves.

The figures below are from the run of 19 November 2026, made after two independent reviews and the changes that followed them: the truth is the mean curve the simulation generates, the five-dataset scenario pools five, the interval is never narrower than the working model's, the floor on degrees of freedom is three, and the central reading needs the next interpretable hour to be past the line too, on the same side.
The first version of this record misquoted several figures and they should not be relied on.

What it showed, with three interior knots and the rule on rows, and no error inside the bands unless said:

- **The curve is recovered.** For the flat, straight and levelling-off curves the interval covered the truth 92 to 100% of the time at every hour from half an hour to six, with eight datasets and with five.
  For bends at two and three hours it covered 90 to 100%, and for a rise then a fall 88 to 99%.
- **A dip is smoothed.** On the way down into a dip coverage fell to 57% at an hour and a half with eight datasets (74% with five), with errors of up to 0.05 SD on its way back up (0.055 with five datasets), and was above 90% at its lowest point.
- **A false conservative threshold is rare.** One was reported where none exists, or more than a quarter of an hour early, in at most 1% of runs of any scenario with the rules used here, and in at most 1.5% under any rule tried, with or without error inside the bands, and with eight datasets or five.
- **A real threshold is found late by the conservative rule, or not at all.** With eight datasets, a curve bending at two hours had its threshold found in 90% of runs, a median hour and a half late.
  A straight line was found in 53%, an hour and a half late.
  A curve levelling off past 0.10 SD was found in 9%.
- **Five datasets give a conservative threshold about half as often as eight:** the bend at two hours in 51% of runs and the straight line in 24%.
- **The central threshold is near the true crossing on average, and is wrong often enough to matter.**
  With eight datasets it was found in 63 to 99% of runs of every curve with a threshold, with a median between ten minutes before the true crossing and a little over half an hour after it.
  It was reported more than a quarter of an hour early in up to 19% of those runs, and in up to 34% with error inside the bands.
  It was reported for a flat curve in 2% of runs (6% with error inside the bands), and for a curve that levels off at 0.08 SD in 21%.
  From five datasets it was found in 29 to 73% of runs, with a flat curve reported in under 1% and the curve levelling off at 0.08 SD in 12%.
  An independent simulation found a curve levelling off just short reported about a quarter of the time whether five, ten or twenty datasets were pooled.
- **Whether a threshold holds.** For every curve that does stay beyond, the estimate was said to hold in 92 to 100% of thresholds found.
  For a dip, which comes back, the estimate was still said to hold in a third of the conservative thresholds found and an eighth of the central ones, because the pooled curve smooths its return; the interval was said to go on meeting the rule in almost none.
  For a curve levelling off just past 0.10 SD the interval went on meeting the conservative rule in two thirds, so a FALSE there does not mean the curve came back.
  The interval's statement applies the floor on degrees of freedom at the later hours too.
  Neither statement can be read alone: the estimate's says a dip holds too often, the interval's says a plateau does not, and both are read beside the plotted curve.
  Where a threshold falls on the last interpretable hour there is nothing later to judge, and neither is given.
- **Three interior knots follow a bend better than two** (coverage at a bend at two hours 99% against 70%).
  For a dip the two placements each do better at some hours and worse at others.
- **The rule on rows per stretch** costs units (8 of the cell's 14 datasets remain) and changed coverage little in the simulation, but without it a unit with a handful of children at long hours is fitted with a standard error far too small.
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

- Few exposure and outcome pairs will report a conservative threshold: it needs five or more datasets and a curve that clearly passes 0.10 SD, and from five datasets it is found about half as often as from eight.
  More will report a central one, and a reader must not take the two as equally firm.
- The simulation used one cell, unweighted fits and normal outcomes, and the mild kind of band error: recorded hours that are right on average.
  It did not test weights or clustered errors, outcomes on five-point scales, answers that misreport hours, or confounding by age that is not smooth.
- The interval is conservative: with the floor on the standard error an independent simulation found it covering the truth 96 to 98% of the time for smooth curves, and more with five or six datasets or banded items.
  The floor makes some conservative thresholds half an hour to an hour later than the robust standard error alone would.
- More datasets do not help a sharp feature: a dip is smoothed whatever their number, and the interval around the smoothed curve narrows, so coverage on the way into a dip falls as datasets are added.
- The rule on rows per stretch removes many units, among them every unit whose item stops at four hours or less, so some pairs with five or more datasets have no curve.
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
