# BPIPD-2387 (CHIS) - tidier notes

## Study summary

CHIS is the California Health Interview Survey, a population-based random-digit-dial telephone survey of California households run by the UCLA Center for Health Policy Research, fielded every other year since 2001 (CHIS 2009 Child PUF data dictionary, s2.1 Overview).
It is a **repeated cross-section, not a panel**: each cycle draws a fresh sample and no participant is followed between cycles.
Within each sampled household one adult (18+) is selected at random, and if the household contains adolescents (12-17) and/or children (0-11) one of each is sampled; the adolescent is interviewed directly and the adult most knowledgeable about the child's health (MKA) completes the child interview (CHIS 2009 Child PUF data dictionary, s2.1-s2.2).
So the **child file is adult-proxy report about a child aged 0-11 and the teen file is adolescent self-report aged 12-17**, confirmed in the tidied table, where `srage`/`srage_p` runs 0-11 for `cohort == "child"` and 12-17 for `cohort == "teen"`.

Country: United States (California only).
The spec declares four cycles and the folder holds exactly those four.

| Cycle | Field period | Source |
|---|---|---|
| 2001 | November 2000 - October 2001 | CHIS 2001 Child PUF data dictionary, s2.1 |
| 2005 | July 2005 - April 2006 | CHIS 2005 Child PUF data dictionary, s2.1 |
| 2007 | July 2007 - early March 2008 | CHIS 2007 Child PUF data dictionary, s2.1 |
| 2009 | September 2009 - April 2010 | CHIS 2009 Child PUF data dictionary, s2.1 |

Sampling is geographically stratified (56 strata from 2005, 47 in the first two cycles), but the PUFs exclude the geographic design information and ship the final weight `rakedw0` plus 80 replicate weights `rakedw1`-`rakedw80` for variance estimation (CHIS 2009 Child PUF data dictionary, s1 sample design and Section P, Full Design and Replicate Weight Series).
Interviews were conducted in English, Spanish, Chinese (Mandarin and Cantonese), Vietnamese and Korean (CHIS 2009 Child PUF data dictionary, s2.3); the tidied table keeps `intvlang` for the 2005-2009 cycles.

## Tidied table

Grain: **one row per child or adolescent per CHIS cycle**.
Design: repeated cross-section with no linkage between cycles, so participants = observations.
Row key: `puf_id` alone, unique across all rows (50,009 distinct values in 50,009 rows), because CHIS encodes the cycle and the component in the id's leading digits (`13xxxxxx` 2001 child, `12xxxxxx` 2001 teen, `53/52` 2005, `73/72` 2007, `93/92` 2009).
No `participant_id` was constructed; the mapping row can be `as.character(puf_id)`.

Size: 50,009 rows x 1,068 columns (412 Mb in memory).

Rows per cycle and component:

| Cycle | child | teen | cycle total |
|---|---|---|---|
| 2001 | 12,802 | 5,858 | 18,660 |
| 2005 | 11,358 | 4,029 | 15,387 |
| 2007 | dropped (9,913) | 3,638 | 3,638 |
| 2009 | 8,945 | 3,379 | 12,324 |
| **total** | **33,105** | **16,904** | **50,009** |

The kept counts match the completed-interview counts the data dictionaries report for 2005 (11,358 child / 4,029 adolescent, Table 1-2), 2007 (3,638 adolescent, Table 1-2) and 2009 (8,945 / 3,379, Table 1-2) exactly.
2001 is the exception: its Table 1-2 reports 13,276 completed child and 6,058 completed adolescent interviews against 12,802 and 5,858 rows in the PUF (see Open questions).

How it was assembled:

1. For each cycle x component, the main `.dta` file (`CHILD.dta` / `TEEN.dta`) takes its companion imputation-flag file (`CHILDF.dta` / `TEENF.dta`) by `left_join(by = "puf_id", relationship = "one-to-one")`; the flag file's shared columns are dropped from the right-hand side first, so there are no `.x`/`.y` pairs. CHIS 2001 shipped no flag file.
2. A `cohort` column (`"child"` / `"teen"`) is added to each part, because whether a row is proxy or self-report is not otherwise recoverable from the bound table.
3. **Screen rule:** a cycle x component part is kept only if someone answered a single-device screen item, the computer-for-fun hours items (`cg9_p`, `cg11_p`, `te13_p`, `te15_p`, `cg9`, `cg11`, `te13`, `te15`). The TV item bundles watching with video games, so it is not a disaggregated item. This drops the 2007 child part (9,913 rows), whose questionnaire has no TV, video-game or computer item (CHIS 2007 Child Questionnaire v5.2, Sections A-H, checked by text search; the 2007 Child PUF has no `cg8`-`cg11`). Every other part has at least one answered computer item.
4. The seven kept parts are `bind_rows()`ed: child and teen rows are different participants, so they stack rather than join.
5. Value labels are reconciled before the bind (see "Done in the tidier that is arguably harmonisation").

Values carried or filled across waves: **none**. CHIS is a repeated cross-section, so there is no within-participant carry, and the final weight `rakedw0` is released on every row.

## Files

**Used**: all 14 data files the spec declares:
`chis01_child_stata/Data/CHILD.dta`, `chis01_teen_stata/chis01_teen_stata/Data/TEEN.dta`,
`chis05_child_stata/chis05_child_stata/{CHILD,CHILDF}.dta`, `chis05_teen_stata/chis05_teen_stata/{TEEN,TEENF}.dta`,
`chis07_child_stata/Child STATA/{CHILD,CHILDF}.dta`, `chis07_teen_stata/Teen STATA/{TEEN,TEENF}.dta`,
`chis09_child_stata/chis09_child_stata/{CHILD,CHILDF}.dta`, `chis09_teen_stata/chis09_teen_stata/{TEEN,TEENF}.dta`.
The 2007 child pair is read but its rows are dropped by the screen rule, so its 43 columns found only in that part no longer appear.

The `*F.dta` files are **imputation-flag files**, one row per participant keyed on `puf_id`, whose columns are named `<var>_x` and labelled e.g. "IMPUTATION FLAG FOR CA6"; the CHIS 2009 Child dictionary lists `childf.sas7bdat/.sav/.dta` as the "Imputation flag file" in its accompanying-files list (s1.1).
They contribute 306 `_x` columns to the tidied table.
They were kept because the spec declares them as `role: data` and because whether `povll`, `aheduc` or `srage` was imputed matters to a pooled analysis.

**Ignored**: none.

**Undeclared files found in the study folder**: none; the eight questionnaire PDFs and eight data-dictionary PDFs are all declared (`role: docs` and `role: codebook`).

**Spec edits**: none.

## Decisions to confirm

- **2007 child dropped under the screen rule, grouping by cycle x component.** The child and teen files are separate instruments and samples within a cycle, so the rule was applied to each component as the unit that fields the item, as country-cycles are elsewhere. Confirm this grouping.
- **The TV-or-video-games item was not counted as disaggregated.** It pools watching with gaming; had it counted, no part would change, because every part that has it also has a computer item.
- **Child and teen rows are stacked into one table.** They are different people with different respondents and largely disjoint item sets, so most columns are populated in only some cycle x component combinations; this is expected.
- **The 2001 PUF-recode screen items were deliberately NOT renamed onto the later cycles' names**; they are top-coded differently (see Pointers).
- **`cheduca` was split into `cheduca_4lvl` (2001) and `cheduca_11lvl` (2005-2009)** because the same codes mean different things (code 3 is "some college" in 2001 and "grade 12 / H.S. diploma" from 2005).
- **The imputation-flag files were joined in** rather than left out.

## Rows dropped

- 2007 child component, 9,913 rows: no single-device screen item was fielded (screen rule).
- No other filter; there is no age filter, and no attendance filter is needed because each PUF row is one completed interview.

## Done in the tidier that is arguably harmonisation

Two things, both about labels rather than data; no value is changed, recoded, rescaled or set to `NA` anywhere in the tidier.

1. **Value labels are reconciled across the seven parts before binding.** CHIS re-words its labels between cycles and occasionally re-codes an item outright, and `bind_rows()` would silently attach the first part's labels to every cycle. `bp2387_align_value_labels()` therefore:
   - leaves a column alone when every part's map is identical;
   - unifies the map when only the *wording* of a code differs, taking the latest cycle's wording; negative codes are CHIS's missing reserve and their wording never counts as a disagreement;
   - **removes the value labels entirely** from a column whose non-negative codes carry different meanings in different parts, keeping the variable label, so that no wrong label is asserted.

   After the bind, `bp2387_restore_labels()` puts back the variable labels `bind_rows()` drops when their wording differs between parts, taking the latest cycle's wording.

   38 columns lost their value labels this way, and the mapping must read the relevant cycle's data dictionary for them:
   `acmdnum, ak1, ca12b, catribe, cc5, cd7, cf1a, cf21, cg3g, cg6, ch18, dnvst, eligprg3, err, ia21, ma2_p, ma7_p, pmarit, racecn_a, racecn_p, racedo_a, racedo_p, racehp_a, racehp_p, rsn_nomc, rsn_unin, srtenr, tb7, tc6, tc6a, te19, tf14, tf15, tf5, th6a, ur_clrt, usual5tp, yrus`.
   Notable members: `ur_clrt` (5 urbanicity levels in 2001, 4 from 2005; use `ur_clrt2`), `srtenr` (3 tenure levels in 2005, 2 from 2007), `yrus` (child files band code 4 as "10-11 years" while teen files from 2005 band it as "10-14 years"), and the race recodes `racecn_*`/`racedo_*`/`racehp_*` (2001 codes "other single race" where 2005+ codes "PI/other single race").

   Six columns are on a hand-checked wording-only allowlist (`bp2387_same_meaning`): `agegrp_a`, `cg10`, `cg11`, `fv5day`, `sch_typ`, `te14`.
   `te14`'s entry is load-bearing: the CHIS 2005 Teen PUF labels its code 93 "DOESN'T HAVE ACCESS TO A PC", but `te14` is the weekend TV/video-game item and its questionnaire item QT05_E16 offers "DOESN'T HAVE TV", as 2007 and 2009 both label it, so the tidied `te14` carries "DOESN'T HAVE TV" for code 93.

2. **`cheduca` renamed per coding** (see Decisions to confirm).

## Pointers for harmonisation

`.wave` / `.wave_label` are `"2001"`, `"2005"`, `"2007"`, `"2009"` and are the CHIS cycle.

| Dataschema target | Source column(s) | Notes |
|---|---|---|
| `participant_id` | `puf_id` (zero-padded, e.g. `01200001`) | unique across all rows; `as.character(puf_id)` |
| `wave` | `.wave` | repeated cross-section, so the four-digit survey year |
| `data_year` | `.wave` | the survey's own year label; field periods straddle years (2001 = Nov 2000-Oct 2001, 2005 = Jul 2005-Apr 2006, 2007 = Jul 2007-Mar 2008, 2009 = Sep 2009-Apr 2010) |
| `age_years` | `srage` (2001) / `srage_p` (2005-2009) | completed years, so +0.5; child 0-11, teen 12-17; 2001 child `srage` labels code 0 "UNDER 1 YEAR OLD" |
| `sex` | `srsex` | 1=MALE, 2=FEMALE; all seven parts |
| `country` | constant "United States" | California only |
| `survey_weight` | `rakedw0` | the final (core release) cross-sectional weight, every row; the replicate weights `rakedw1`-`rakedw80` are not harmonised |
| `survey_strata`, `survey_cluster` | none | geographic design information is excluded from the PUFs (2009 Child dictionary, Section P) |
| `ses_household` | `povll` (1=0-99% FPL, 2=100-199%, 3=200-299%, 4=300%+ FPL; all parts); `povll2_p`, `povgwd_p` (2005-2009 only) | income-to-needs bands; apply the Household SES cuts by ridit within cycle |
| `parental_education` | `aheduc` (2005-2009, 1=grade 1-8 ... 10=Ph.D., 91=no formal education); `ahedu` (2001, 4 levels) | education of the randomly selected household adult, who need not be the child's parent; two scales |
| `parental_education` (child component only) | `cheduca_11lvl` (2005, 2009); `cheduca_4lvl` (2001) | "education level of the most knowledgeable adult", the proxy respondent; `cheduca_4lvl` carries an unlabelled code 91 (no formal education) |
| `ethnicity` | `racecn_p`, `racedo_p`, `racecn_a`, `racedo_a`; single-race flags `sraa`/`srai`/`sras`/`srw`/`srch`/`srph`/`sraso`; `racehp_a`, `racehp_p` (2001, 2005); `latintp`, `asian4tp` (2001) | the `race*` recodes lost their value labels; read the cycle data dictionary; a Hispanic-origin item sets Hispanic in this US sample |
| `born_in_country` | `chcntrys` (2001 child), `cntrys` (2001 teen, 2005-2009 both) | "COUNTRY BORN IN", 1=UNITED STATES, 2-7 regions abroad; the child's own birthplace (2009 Child dictionary, DD-86); `citizen2` 1=US-BORN CITIZEN is an alternative |
| `immigrant_background` | `cntrys` + `cntrym` + `cntryf` (2001 child: `chcntrys`, `chcntrym`, `chcntryf`) | same 1-7 coding; `citiz2_m` / `citiz2_f` (parents' citizenship and immigration status) are an alternative |
| `urbanicity` | `ur_clrt2` (2 levels, consistent across cycles) | prefer this over `ur_clrt` and the other `ur_*` recodes, whose level counts change |
| `family_structure` | `hhsize_p` (all parts); `pmarit` (teen, all cycles); `famtp_p` (2001 only) | `pmarit` lost its labels (code 2 recoded between 2001 and 2005) |
| `school_grade` | not collected | `sch_typ` (public/private, 2005-2009) and `ta4`/`ta4c` (attending school, teen) only |

### Screen time

All screen-time items are **hours per day entered as a whole number**, with two special codes: **93 = doesn't have a TV / doesn't have access to a PC** and **94 = more than zero but less than one hour**; negative codes are the CHIS missing reserve.
Responder is the MKA proxy for `cohort == "child"` and the adolescent for `cohort == "teen"`.

| Column | Component | Cycles | Question | Day |
|---|---|---|---|---|
| `cg8_p` | child | 2001 | hours/day watching TV or playing video games, Mon-Fri (PUF recode) | weekday |
| `cg9_p` | child | 2001 | hours/day using a computer for fun, Mon-Fri (PUF recode) | weekday |
| `cg10_p` | child | 2001 | hours/day watching TV or playing video games, Sat-Sun (PUF recode) | weekend |
| `cg11_p` | child | 2001 | hours/day using a computer for fun, Sat-Sun (PUF recode) | weekend |
| `cg8` | child | 2005 | same as `cg8_p` | weekday |
| `cg9` | child | 2005 | same as `cg9_p` | weekday |
| `cg10` | child | 2005, 2009 | same as `cg10_p` | weekend |
| `cg11` | child | 2005, 2009 | same as `cg11_p` | weekend |
| `te12_p` | teen | 2001 | hours/day watching TV or playing video games, Mon-Fri (PUF recode) | weekday |
| `te13_p` | teen | 2001 | hours/day using a computer for fun, Mon-Fri (PUF recode) | weekday |
| `te14_p` | teen | 2001 | hours/day watching TV or playing video games, Sat-Sun (PUF recode) | weekend |
| `te15_p` | teen | 2001 | hours/day using a computer for fun, Sat-Sun (PUF recode) | weekend |
| `te12` | teen | 2005, 2007 | same as `te12_p` | weekday |
| `te13` | teen | 2005, 2007 | same as `te13_p` | weekday |
| `te14` | teen | 2005, 2007, 2009 | same as `te14_p` | weekend |
| `te15` | teen | 2005, 2007, 2009 | same as `te15_p` | weekend |
| `ctv3` / `ttv3` | child / teen | 2001 | derived binary "TV/video games 3+ hours: weekdays" (1=yes, 2=no) | weekday |

How the conventions apply:

- **TV or video games** is watching pooled with gaming, so under the guide's bundling rule it goes to `st_total` (partial) and `st_watch` / `st_game` are incompatible with the item named; confirm against DR-0004, which admits to `st_total` only an item on all screen use.
- **Computer for fun** is a single device restricted by purpose, so it maps to `st_comp` as partial.
- **Never add the TV/video-game and computer items together** into `st_total` or any row (DR-0002, DR-0004).
- **Code 94** ("more than zero, less than one hour") is a sub-hour band and takes 0.5; **code 93** (no TV / no PC access) is 0 under the Non-users rule.
- Open top bands take their stated lower bound, per cycle: `cg8_p` "11+ HOURS" = 11, `cg9_p` and `cg11_p` "6+ HOURS" = 6, `te12_p` and `te13_p` and `te15_p` "10+ HOURS" = 10, `te14_p` "15+ HOURS" = 15, `cg10_p` "11+ HOURS" = 11; write each top-band value into the row's notes.
- The 2005-2009 items are open integers (observed maxima 16-20); `coalesce(cg8, cg8_p)` per cycle is fine as long as the 2001 top-coding is noted.
- The unsuffixed rows are (5 x wd + 2 x we) / 7 only where both are mapped: 2001, 2005 and 2007 teen. **2009 has weekend items only**, so it fills `_we` alone.
- **Child age gate**: in 2001 the whole block was asked of ages 4-11 (ages 0-3 coded -1 SKIPPED, 3,857 rows; 2001 Child questionnaire, routing before CG7).
  From 2005 the TV/video-game items `cg8`/`cg10` were asked of ages 2-11 (ages 0-1 coded -1: 1,999 rows in 2005, 1,296 in 2009) and the computer items `cg9`/`cg11` of ages 4-11 (ages 0-3 coded -1: 3,929 rows in 2005, 2,760 in 2009).
  Sources: 2005 Child questionnaire programming notes QC05_C5 (CAGE > 1) and QC05_C7 (CAGE > 3); the 2009 note QC09_C17 says CAGE >= 3, but no child aged 3 in the 2009 PUF has an answer to `cg11`.
  These children were not asked, so NA, not 0.

### Outcome instruments present

| Construct | Columns | Component / cycles | Notes |
|---|---|---|---|
| Psychological distress, **Kessler 6** | `tg11` (nervous), `tg12` (hopeless), `tg13` (restless), `tg14` (depressed), `tg15` (everything an effort), `tg16` (worthless), past 30 days, 1=ALL ... 5=NOT AT ALL | teen, 2007 + 2009 (n=7,017) | plus CHIS-derived `distress` and `dstrs30`; `mh_distress_k6_self` |
| 2005 CES-D-style block | `cesd8`, items `td6`-`td13` (days in past 7: enjoyed life, could not shake sad feelings, felt depressed, happy, lonely, like a failure, sad, didn't want to do usual activities) | teen, 2005 only (n=4,029) | check the 2005 Teen dictionary before mapping to a CES-D row |
| 2001 MHI-5-style block | `td1`-`td5` (time nervous / down / calm and peaceful / downhearted and sad / a happy person, past 4 weeks; 1=ALL ... 5=NOT AT ALL) | teen, 2001 only (n=5,858) | not K6 |
| Child behaviour and emotion (5 items) | `cg28` (obedient), `cg29` (often seems worried), `cg30` (often unhappy, depressed, tearful), `cg31` (gets along better with adults), `cg32` (good attention span) | child, 2005 + 2009 (n=20,303) | proxy report; SDQ-derived items, not a full SDQ subscale |
| Difficulties impact | `cf30`, `cf31`, `cf32` | child, 2005 + 2009 | proxy report |
| ADHD | `adhd`, `ca11a` | child, 2001 only | |
| Academic | none | - | no grades, tests or engagement items |

Newly added dataschema variables the study can inform:

- `wb_generalhealth_parent`: `ca6` "GENERAL HEALTH CONDITION", 1=EXCELLENT ... 5=POOR, MKA proxy report, child 2001, 2005, 2009; the teen's own rating `tb1` (all cycles) is the self-rated counterpart.
- `beh_alcohol_self`: `te22` "ever had more than a few sips of an alcoholic drink", 1=YES 2=NO, teen all cycles; lifetime recall, so partial.
- `beh_tobacco_self`: `smkcur` current smoker (derived, teen 2005-2009); `tc38` ever smoked cigarettes (2005-2009) and `te17` ever smoked regularly (2001) are lifetime, so partial.
- `mh_sad_self`: `td4` downhearted and sad, 2001, time-of-past-4-weeks scale; `td12` days in past 7 felt sad, 2005 (day-count rule). K6 `tg14` asks "depressed", not sad.
- `wb_happiness_singleitem_self`: `td5` (2001, how much of the time a happy person) and `td9` (2005, days felt happy); both are items of a broader scale.
- `mh_lonely_*`: `td10` days in past 7 felt lonely (2005) is a single item, so it cannot fill the loneliness scale row.
- `wb_socsupport_family_self`: only `th7_p` "how much you feel parent/guardian care about you" (2001), a single item.
- `acc_schoolbelonging_self`: only `te65` "feel safe at your school" (teen 2009), which is not belonging.
- `beh_agg_*`: `fight` and `tg3_p` physical fights past 12 months, teen 2001.
- A label search found no items for `fam_screenrules`, `fam_bedroomscreen`, `fam_parentstress`, `st_messaging*`, `st_creating`, `beh_truancy`, `beh_stealing`, `beh_vandalism`, `beh_cannabis_self`, `beh_problematicuse_self`, `mh_worry_self` (only the proxy `cg29`), `mh_nobodycares_self`, `mh_lifenotworth_self`, `mh_selfharm_self`, psychosomatic or somatic complaints, domain satisfaction, `hlth_sleep*` or creativity; `cg41`, `cg42`, `te64` are neighbourhood trust and safety items.

### Missing-value codes and label quirks

- CHIS reserves negative codes across every file: **-9 NOT ASCERTAINED, -8 DON'T KNOW, -7 REFUSED, -5 ADULT/HOUSEHOLD INFO NOT COLLECTED, -2 PROXY SKIPPED, -1 INAPPLICABLE (labelled SKIPPED in 2001)**. They are left in place; `na_if()` or a `< 0` filter belongs in the mapping.
- 91 is "no formal education" in the education recodes, a real category, not missing.
- The 2001 files write the curly apostrophe in "DON'T KNOW" while the later cycles use a straight one.
- The 38 columns listed above carry **no value labels** in the tidied table by design.
- `acmdnum` has only 12 distinct values and is not an identifier despite looking like one.
- A variable label worded differently across parts takes the latest cycle's wording (`bp2387_restore_labels()`), so `rakedw0` reads "CHIS2009 RAKED WEIGHT - FULL SAMPLE" on every row; only `.wave`, `.wave_label` and 59 imputation-flag `_x` columns that carry no label in any source file are unlabelled.

## Open questions / spec problems

- **2001 PUF row count.** 12,802 child and 5,858 teen rows against 13,276 and 6,058 completed interviews in the 2001 dictionary's Table 1-2. The 2001 dictionary says material was excluded from the PUF for confidentiality but gives no record count. No rows were dropped for this in the tidier; worth confirming against the CHIS 2001 methodology reports on healthpolicy.ucla.edu.
- **Whether the imputation-flag columns are wanted.** Dropping the six `*F.dta` resources would remove 306 columns.
- No spec problems: every declared glob matched exactly one file, no undeclared files were found, and no reader change is needed.

## Convention review (2026-09-25)

- Tidier now drops cycle x component parts with no answered single-device screen item, which removes the 2007 child part (9,913 rows, 43 columns found only there); rows 59,922 -> 50,009.
- No retained value changed; `cb16_a`-`cb16_d` and `cc6` regain value labels, since only the 2007 child file coded them differently (43 -> 38 unlabelled columns).
- Rules on attendance, second reports and carry-forward do not apply (repeated cross-section, one completed interview per row, no carry, weight on every row).
- Tidier header and helper comments cut to the one-line style.
- Notes: counts, drops and decisions updated; screen pointers rewritten to the Screen time conventions (bundled TV/video-game item, no summing, codes 93/94, top bands, 2009 weekend-only).
- Notes: pointers added for survey design, birthplace, immigrant background, general health, alcohol, tobacco and the mood single items; strata and cluster are not released.

## Verification (2026-09-25)

- `bind_rows()` dropped the variable label of 272 columns whose label wording differed between parts (among them `puf_id`, `rakedw0`-`rakedw80`, `srage_p` and the 38 columns without value labels), so the tidier now restores each from the latest part that has one; no value, value label or row changed.
- Notes: child age gate corrected (the computer items were asked from age 4, not 2, from 2005), `cg10_p` top band filled in, and the unlabelled-column statement replaced.
