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

A unit that does not record a tier's covariates is not fitted at that tier.
A covariate counts as recorded where it is known for a minimum share of the unit's rows, and the rows with all of the tier's covariates must reach that share too.
Until missing values are imputed, rows with an unknown covariate are left out, so a further tier is fitted on fewer rows as well as fewer units; each further tier therefore also gives the tier 1 model on its own rows, and the change from adjusting is read against that.
Groups of a covariate too small to estimate are merged into one, which then only adjusts and describes no group.

**A further tier, as a sensitivity analysis.**
The family environment, where a unit records it: parental mental health and stress, parenting warmth, harshness and consistency, family functioning and conflict, family structure, siblings and the home learning environment.
These are likely common causes in some datasets, but few datasets record them and some may also respond to the child's difficulties, so they do not enter the primary models.

**Never adjusted for.**

- Sleep, physical activity and body mass: screen use plausibly acts through them, so adjusting would remove part of the association the analysis is estimating.
- Screen-specific parenting (screen rules, mediation, co-viewing): these act on the outcome mainly through screen use, so adjusting for them removes little confounding and can make what remains worse.
- Age at first phone and at first screen use: these are the child's history of the exposure.
- The parent and child relationship rows: they may be affected by screen use, or be part of the outcome.

**Study-level characteristics** (country, collection year, instrument, response format, who reported) are constant within most units and are stage 2 moderators.

**Negative-control outcomes.**
Birth weight, preterm birth, prenatal smoking and breastfeeding come before any screen use, so current screen use cannot cause them.
They are run as outcomes where three or more datasets support them, and an association there indicates confounding the tiers have not removed.

**The reverse direction** cannot be separated in cross-sectional models.
The longitudinal strand addresses it, and the reports say so.

---

## For the lead to decide

This record is a proposal.
The earlier rulings set the three tiers, named sleep and activity as mediators, and made the family rows a sensitivity tier.
Everything else here is the drafter's suggestion, and these points in particular need a decision:

- **Body mass** as a mediator that is never adjusted for.
- **A screen in the bedroom and the parent's own screen time:** left out above with screen-specific parenting, but they may also stand for general parenting, or act on sleep directly.
- **Chronic conditions and disability:** possible common causes, but where the condition is a mental-health or neurodevelopmental one, adjusting for it adjusts for the outcome.
- **Pubertal status and maturity:** timing varies at a given age and may affect both screen use and outcomes, so they are candidates for the sensitivity tier.
- **Parental education** is used only when household SES is not recorded, though many datasets record both and could adjust for both.
- **Breastfeeding** as a negative-control outcome.
- **Whether the sensitivity tier is added to tier 3 or to tier 2:** few datasets reach tier 3.
- **Rows not yet placed:** immigrant background and country of birth, home language, Indigenous status, maternal and paternal education, maternal employment and age, school sector, track and grade, region, parental monitoring and physical punishment.

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

- `fit_stage1()` fits the tiers asked for and records the tier, the measures used and the tier 1 slope on the tier's rows; `compare_stage2_tiers()` sets each further tier beside tier 1.
- Not yet built: the sensitivity tier and the negative-control outcomes.

---

## Related decisions

- DR-0009: the estimand.
- DR-0010: two-stage analysis.
- DR-0014: pooling up the outcome tree.
