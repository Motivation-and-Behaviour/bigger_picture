# DR-0016: Curves are pooled from splines with fixed knots, and the threshold duration is read from the pooled curve

**Status:** Proposed  
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

**The threshold duration.**
It is the lowest interpretable hour above the reference whose 95% interval lies wholly beyond 0.10 SD from the reference, in either direction, with at least four robust degrees of freedom at that hour.
It is reported with its direction (a better or a worse outcome) and with whether the curve stays beyond 0.10 SD at every later hour.

**How it is reported.**

- The pooled curve with its interval, and the number of datasets supporting each hour, is the primary result.
- A threshold duration is reported as "by this many hours", never as the point at which harm begins: in simulation it was usually found an hour or more after the true crossing.
- Where no threshold is reported, the report says that none could be established, not that there is none.
- Thresholds are not compared between exposures or outcomes without their curves and intervals, because the same curve gives a later threshold where datasets are fewer or differ more.
- Curves with a sharp local feature, a bend or a dip, are described cautiously near that feature, where the interval covers the truth less often than it claims.

**The breakpoint** of the piecewise model is built after the freeze, as the plan allows.

---

## Rationale

The method was tested by replacing the outcome of one real cell with a simulated one that follows a known curve, keeping the real hours of screen use, ages, sexes and sizes of its 85 units in 14 datasets, 59 of them with banded items.
Datasets were made to differ in the strength of the curve, in where its shape sits along the hours, and by a slope of their own.
Eight curves were built in: flat, a straight line, a bend at two hours and at three, curves levelling off just short of and just past 0.10 SD, a dip that recovers, and a rise then a fall.
Each was run 200 times, with and without error inside the bands, with two knot placements, with and without the rule on rows per stretch, and with all 14 datasets or five drawn at random.

What it showed, with three interior knots and the rule on rows:

- **The curve is recovered.** For the flat, straight and levelling-off curves, the interval covered the truth 88 to 98% of the time at every hour from half an hour to six.
- **Sharp features are smoothed.** Coverage fell to 82% at a bend at two hours, 88% at a bend at three, and 68 to 86% around a dip, with errors of 0.02 to 0.06 SD.
- **A false threshold is rare.** One was reported where none exists, or more than a quarter of an hour early, in at most 1.5% of runs of any scenario, and never for a flat curve.
- **A real threshold is found late, or not at all.** With 14 datasets, a curve bending at two hours had its threshold found in 95% of runs, a median hour late. A straight line was found in 66%, an hour and a half late. A curve levelling off past 0.10 SD was found in 16%.
- **Five datasets never give a threshold,** because the degrees of freedom are too few, although their curves' intervals still cover the truth 86 to 98% of the time.
- **Three interior knots follow a bend or a dip better than two** (coverage at a bend at two hours 82% against 40%; at a dip's lowest point 81% against 35%) and are no worse elsewhere.
- **The rule on rows per stretch** costs units (9 of the cell's 14 datasets remain) and changed coverage little in the simulation, but without it a unit with a handful of children at long hours is fitted with a standard error far too small.
- **Error inside the bands** moved the curve by 0.02 SD or less at most hours.

So the curve can carry the result, and the threshold duration is a conservative summary of it that will be silent for most pairs.

---

## Alternatives considered

- **Two interior knots, at 1 and 3 hours.** One term fewer, but it cannot follow a bend near two hours.
- **Fitting every unit a curve can be fitted in.** More datasets, at the cost of curve terms resting on a few children.
- **Reading the curve to eight hours.** Beyond the last knot the curve is extrapolated.
- **A threshold from the point estimate crossing 0.10 SD.** It would be found more often and earlier, and would be reported for curves that level off short of it by chance.
- **Waiting for the breakpoint before the freeze.** The plan allows it to follow.

---

## Consequences

- Few exposure and outcome pairs will report a threshold duration: it needs more than five datasets and a curve that clearly passes 0.10 SD.
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
