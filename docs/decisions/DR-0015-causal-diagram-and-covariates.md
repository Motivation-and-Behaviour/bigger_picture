# DR-0015: What the models adjust for, and what they leave alone

**Status:** Proposed  
**Date:** 2026-11-13  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration speaks of the "impact" of screen use, and the dataschema now holds rows that sit at different places between screen use and an outcome.
Adjusting for a row that screen use acts through changes what the slope means, and leaving out a common cause leaves the slope confounded.
The analysis needs one agreed picture of which rows are which before the specification is frozen.

The estimand (DR-0009) is the average within-unit association per extra hour of screen use.
The wording of every result stays "associated with".

---

## Decision

The diagram below is the working picture.
An arrow means "may affect".

```
age, sex                      -> screen use ; -> outcome
family SES                    -> screen use ; -> outcome
ethnicity or minority, urban  -> screen use ; -> outcome
family environment            -> screen use ; -> outcome
child's health conditions     -> screen use ; -> outcome
screen-specific parenting     -> screen use
screen use                    -> sleep, physical activity, body mass -> outcome
screen use                    -> outcome
outcome                       -> screen use              (reverse direction)
country, year, instrument     -> the size of every arrow (study level)
```

**Adjusted for, in tiers.**
Each estimate records its tier.

1. Age and sex.
2. Tier 1 and a socioeconomic proxy: household SES, else parental education, else area SES.
3. Tier 2 and ethnicity or minority status, and urbanicity.

**A further tier, as a sensitivity analysis.**
Tier 3 and the family environment and the child's health conditions, where a unit records them: parental mental health and stress, parenting warmth, harshness and consistency, family functioning and conflict, family structure, siblings, the home learning environment, chronic conditions and disability.
These are likely common causes in some datasets, but few datasets record them and some may also respond to the child's difficulties, so they do not enter the primary models.

**Never adjusted for.**

- Sleep, physical activity and body mass: screen use plausibly acts through them, so adjusting would remove part of the association the analysis is estimating.
- Screen-specific parenting (screen rules, mediation, co-viewing, a screen in the bedroom, the parent's own screen time, age at first phone): these act on the outcome through screen use, so adjusting for them removes none of the confounding and can make what remains worse.
- The parent and child relationship rows: they may be affected by screen use, or be part of the outcome.
- Pubertal status: it follows age, which is adjusted for.

**Study-level characteristics** (country, collection year, instrument, response format, who reported) are constant within most units and are stage 2 moderators.

**Negative-control outcomes.**
Birth weight, preterm birth, prenatal smoking and breastfeeding come before any screen use, so current screen use cannot cause them.
They are run as outcomes where three or more datasets support them, and an association there indicates confounding the tiers have not removed.

**The reverse direction** cannot be separated in cross-sectional models.
The longitudinal strand addresses it, and the reports say so.

---

## Rationale

The tiers follow what most datasets record, so that the primary estimate rests on as many studies as possible and each further tier shows what a fuller adjustment changes on the studies that allow it.
Rows on the path from screen use to the outcome are left alone because the preregistered question is about the whole association, not the part that bypasses sleep or activity.

---

## Alternatives considered

- **Adjust for everything a dataset records.** It would adjust for mediators in some datasets and not others, so the pooled slope would have no single meaning.
- **Family environment in the primary models.** Too few datasets record it, and the direction of several of its rows is unclear.
- **Mediation analysis of sleep and activity.** A separate question, outside this analysis.

---

## Consequences

- A unit that does not record a tier's covariates does not contribute to that tier, so later tiers rest on fewer datasets; the feasibility report counts them.
- Residual confounding remains in every tier, and the negative-control outcomes are the check on how much.

---

## Implementation

- The tier is an argument of stage 1, recorded on each estimate, and a column of the model list.
- Not yet built: tiers 2 and 3, the sensitivity tier and the negative-control outcomes.

---

## Related decisions

- DR-0009: the estimand.
- DR-0010: two-stage analysis.
- DR-0014: pooling up the outcome tree.
