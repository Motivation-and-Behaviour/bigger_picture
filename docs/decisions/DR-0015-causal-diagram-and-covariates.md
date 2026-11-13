# DR-0015: What the models adjust for, and what they leave alone

**Status:** Accepted  
**Date:** 2026-11-13  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The preregistration speaks of the "impact" of screen use, and the dataschema holds rows that sit at different places between screen use and an outcome.
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
family environment, puberty   -> screen use ; -> outcome
screen-specific parenting     -> screen use
screen use                    -> sleep, physical activity, body mass -> outcome
screen use                    -> outcome
outcome                       -> screen use              (reverse direction)
country, year, instrument     -> the size of every arrow (study level)
```

**Adjusted for, in tiers.**
Each estimate records its tier.

1. Age and sex.
2. Tier 1 and a socioeconomic measure: household SES, else parental education, else area SES.
   Parental education is used only where household SES is not recorded.
3. Tier 2 and ethnicity, or else minority status, and urbanicity.

A unit that does not record a tier's covariates is not fitted at that tier.
A covariate counts as recorded where it is known for a minimum share of the unit's rows, and the rows with all of the tier's covariates must reach that share too.
Until missing values are imputed, rows with an unknown covariate are left out, so a further tier is fitted on fewer rows as well as fewer units; each further tier therefore also gives the tier 1 model on its own rows, and the change from adjusting is read against that.
Groups of a covariate too small to estimate are merged into one, which then only adjusts and describes no group.

**A further tier, as a sensitivity analysis.**
Tier 2 and, where a unit records them, the family environment and the child's physical maturity:

- parental mental health and stress;
- parenting warmth, harshness and consistency;
- family functioning and conflict;
- family structure and siblings;
- the home learning environment;
- pubertal status and maturity.

These are likely common causes in some datasets, but few datasets record them and some may also respond to the child's difficulties, so they do not enter the primary models.
The tier builds on tier 2 because few datasets reach tier 3.
Pubertal timing varies at a given age and may affect both screen use and outcomes, so age alone does not stand for it.

**Never adjusted for.**

- Sleep, physical activity and body mass: screen use plausibly acts through them, so adjusting would remove part of the association the analysis is estimating.
- Screen-specific parenting (screen rules, mediation, co-viewing, a screen in the bedroom, the parent's own screen time): these act on the outcome mainly through screen use, so adjusting for them removes little confounding and can make what remains worse.
- Age at first phone and at first screen use: these are the child's history of the exposure.
- The parent and child relationship rows: they may be affected by screen use, or be part of the outcome.
- Chronic conditions and disability: where the condition is a mental-health or neurodevelopmental one, adjusting for it adjusts for the outcome.

A row not named in this record is not adjusted for in any tier.
That covers immigrant background and country of birth, home language, Indigenous status, maternal and paternal education, maternal employment and age, school sector, track and grade, region, parental monitoring and physical punishment.
Placing one of them in a tier is an amendment to this record.

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
- **Household SES and parental education together in tier 2.** Many datasets record both, but a tier that needs both would rest on fewer of them.
- **The sensitivity tier on top of tier 3.** Too few datasets reach tier 3 for it to be estimated.
- **Chronic conditions and disability as confounders.** They may be common causes, but they overlap with the outcomes.
- **Mediation analysis of sleep and activity.** A separate question, outside this analysis.

---

## Consequences

- A unit that does not record a tier's covariates does not contribute to that tier, so later tiers rest on fewer datasets; the feasibility report counts them.
- Residual confounding remains in every tier, and the negative-control outcomes are the check on how much.
- A bedroom screen and the parent's own screen time may also stand for general parenting, or act on sleep directly; leaving them out accepts that.

---

## Implementation

- `fit_stage1()` fits the tiers asked for and records the tier, the measures used and the tier 1 slope on the tier's rows; `compare_stage2_tiers()` sets each further tier beside tier 1.
- The feasibility report counts the datasets each tier keeps and the datasets with a negative-control row.
- The four negative-control rows have the role `negative_control` in the outcome registry.
- Not yet built: the sensitivity tier and the models of the negative-control outcomes.

---

## Related decisions

- DR-0009: the estimand.
- DR-0010: two-stage analysis.
- DR-0014: pooling up the outcome tree.
