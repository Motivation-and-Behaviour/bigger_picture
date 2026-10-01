# BPIPD-1025 (1970 British Cohort Study) - tidier notes

## Study summary

BCS70 is a multi-disciplinary longitudinal birth cohort study that follows people born in England, Scotland and Wales in a single week of April 1970.
The longitudinal target sample was 17,287 babies; 17,198 were achieved at birth (`UKDA-3535-spss/mrdoc/pdf/a3535uab.pdf`, "Cases (obtained)", and the populations/samples technical report `ncds_and_bcs70_populationsandsamplesovertime.pdf` Table 4.2).
Births in Northern Ireland were included at birth but were not retained in the longitudinal sample, so the cohort is a Great Britain sample (same technical report, para. 2.6).
The study is run by the UCL Centre for Longitudinal Studies and distributed through the UK Data Service, one UKDA study number per data release.

Sweeps have been run at ages 0, 5, 10, 16, 26, 29/30, 34, 38, 42, 46-48 and 51 (`bcs70_age_16_leisure_and_television_diaries_user_guide.pdf`, "About the 1970 British Cohort Study").
The nine UKDA releases in this study folder cover six of them, and only three contain a participant who is a child or adolescent:

- **1980, age 10** (UKDA 3723).
  Data collected by a parental interview with the mother (health visitor administered), a maternal self-completion form, an educational (teacher's) questionnaire, a pupil self-completion form filled in by the child at school, a medical examination by the school doctor, and school-administered educational tests (`a3723uab.pdf`, "Survey Instruments"; the same document gives the achieved sample as 14,875 cohort members with at least one instrument).
- **1986, age 16** (UKDA 3535, known as "Youthscan"), with three later supplementary releases of the same sweep: the 16-year arithmetic test (6095), the 1986 reading and matrices tests (8288), and the leisure and television diaries (8949).
  Instruments are lettered Documents A-S; the cohort member self-completed Documents C, E, F, G, H, J, Q and S, the mother completed Document P, a health visitor completed the Parental Interview Form (Document O, with Document T its alternative version), the teacher completed Document L, the head teacher Document M, and a community medical officer Document R (`a3535uab.pdf`, "Survey Instruments" and the per-document descriptions).
  Variable names are the document letter plus the question number, so `f13` is Document F question 13 and `c5j1a` is Document C question 5J item 1a (`a3535uab.pdf`, "Variable names").
  The achieved sample was 11,622 cohort members with at least one instrument (same document, "Cases (obtained)").
  Fieldwork ran through 1986 and into 1987 (diaries user guide, section 1.2).
- **2004-2005, age 34** (UKDA 5585) - the **Parent & Child survey**.
  This element gathered information from a one-in-two sample of cohort members about all of their co-resident natural or adopted children aged under 17, through a parent interview, four age-banded parent paper self-completions, child assessments for ages 3-16, and a child paper self-completion for ages 10-16 (`bcs70_age_34_user_guide_2020_update.pdf`, section II.17).
  In this release the **participant is the cohort member's child**, not the cohort member.

The remaining releases are the age 26 (3833, 1996), age 42 (7473, 2012) and age 51 (9347, 2021) sweeps, in which the cohort member is an adult.

The cohort member identifier `BCSID` links the releases longitudinally (diaries user guide, section 3.3; age 34 user guide, section II.11).
A small number of identifiers were reassigned in a 2020s re-deposit to realign sweeps - 86 cases at 1980 and 48 at 1986 (`realignment_of_bcs70_identifiers_documentation.pdf`).
The age 34 user guide warns that although the longitudinal link has been made, it has had only limited longitudinal validation, and advises users to check it themselves (section II.11, "A NOTE OF CAUTION").

## Tidied table

**Grain**: one row per participant per sweep.
**Row key**: `participant_id` + `wave` (checked with `anyDuplicated()` in the tidier and confirmed by `check_tidied.R`).

16,821 rows x 8,874 columns, 1.1 Gb in memory, 39 MB serialised in the targets store.
**Design**: two waves kept: the cohort member at 16 and, as a second generation, the cohort members' children in 2004-05.
**Participants**: 16,821 distinct `participant_id` - 11,614 cohort members (age 16) and 5,207 cohort members' children (one row each); every participant has one row.

| wave | rows | participant | source of the row |
|---|---|---|---|
| `age16` | 11,614 | cohort member, aged ~16 in 1986 | `bcs7016x.sav` |
| `child2004` | 5,207 | a cohort member's child aged 0-16 in 2004-05 | `bcs_2004_parent_and_child.sav` |

The 5,207 `child2004` rows are the children of 2,846 distinct cohort members; child ages run 0 to 16 with 443 at age 0 falling to 73 at age 16.
2,303 of the 2,846 cohort-member parents in the 2004 survey also have an `age16` row of their own.
The age-10 sweep (1980) is dropped (decision 2, confirmed 2026-10-01); 10,253 of the 10,870 cohort members seen at both 10 and 16 carry at least one 1980 item on their age-16 row (see Values carried).

**Row counts against the documentation**: the age-16 spine has 11,614 rows against the 11,622 the 1986 user guide reports (and the dropped 1980 file 14,868 against 14,875).
The missing rows are absent from the current deposit; the documentation in the folder does not say why, and the identifier-realignment note does not account for it.

**How the table was assembled**

1980 items - from `sn3723.sav`, only the 1980 parents' qualifications grid (`c1.1`-`c1.22`), the child's ethnic group (`a12.1`, `a12.4`, `a12.7`) and country of birth (`a2.1`) are kept, left-joined one-to-one on `bcsid` to the age-16 row (`bp1025_add_age10_items()`).
`bcs3derived.sav` is still declared in the spec but no longer used.

`age16` - `bcs7016x.sav` (the whole 1986 sweep in one file) left-joined one-to-one on `bcsid` to, in order, `bcs4derived.sav`, `bcs70_age16_school_type.sav`, `jiig-cal_occupational_interests.sav`, `bcs70_16-year_arithmetic_data.sav`, `bcs1986_reading_matrices.sav` and `bcs4_leisure_diary_aggregate.sav`.
All 3,651 reading-and-matrices rows and all 7,242 diary-aggregate rows matched the spine; 4 of 3,475 JIIG-CAL rows and 1 of 3,677 arithmetic rows did not and were dropped by the left join.
The school-type file holds the whole 18,035-strong target sample, of which 9,957 matched.

`child2004` - `bcs_2004_parent_and_child.sav` (the parent interview, one row per child) is the spine, keyed on `bcsid` + `b7chdid`.
All of the child assessment (5,207), child self-completion (942), and the four age-banded parent self-completion files (414 / 825 / 1,259 / 2,285) matched it one-to-one, with no unmatched rows in any of them.
The cohort member's own 2004 interview (`bcs_2004_followup.sav` joined to `bcs7derived.sav`) is then joined many-to-one on `bcsid` as the parent's characteristics; all 5,207 child rows found their parent.

The three sweeps are then `bind_rows()`ed.

**Screen rule (waves with no disaggregated screen item)**: the `age10` wave is dropped.
`age16` (`f13`-`f15` durations, the TV diary) and `child2004` (`yp_q1`, `yp_q2` durations) have disaggregated duration items.
`age10` has only `m88`, a mother-reported TV frequency with no duration; the project lead confirmed on 2026-10-01 that it goes (14,868 rows; 3,998 cohort members seen only at 10 leave the table).

**Attendance**: every spine row is an attender.
BCS70 releases hold no roster rows: every row of `bcs7016x.sav` (min 18 non-missing items) and `bcs_2004_parent_and_child.sav` (min 47) carries data, and the user guides define the achieved samples as cohort members with at least one instrument (`a3723uab.pdf`, `a3535uab.pdf`, "Cases (obtained)").
At 16 the study's own participation record agrees: the document-completion flags `flag_c`-`flag_t` ("Document X completed") show at least one completed document on every one of the 11,614 rows, so the flag would drop none.
The low-count rows are thin, not empty: their 18 non-missing values are mostly administrative (`sex86`, `dob86`, `land86`, the flags), plus one or two items from the completed document.

**Values carried from the dropped 1980 sweep** (joined onto the age-16 row; each label ends "[1980 sweep, carried to the age-16 row]"):

- Parent education, always: `c1.1`-`c1.22` (1980 father's and mother's qualifications grid); the mapping uses it only where the 1986 grid (`t6.*`) is blank.
- The child's ethnic group (`a12.1`, `a12.4`, `a12.7`) and country of birth (`a2.1`).
  The conventions carry these in the mapping with `carry_within()`, but once the 1980 rows are dropped there is no row to carry from, so the tidier attaches them; the mapping uses them only where the 1986 items (`oa3.1`, `oa2.1`) are missing.
- Household SES and family structure: not carried, because the only two linked sweeps are about six years apart (1980 and 1986-87; `a3723uab.pdf`, `a3535uab.pdf`, diaries user guide section 1.2), well over the 12-month limit.
- Age: not filled; the mapping derives it from the birth date and document dates at 16 and from `b7age` in 2004.
- Survey design: none exists for these sweeps (a whole-population birth cohort with no design weight, stratum or cluster at 10, 16 or in the 2004 Parent & Child release; the only weights in the folder are age-51 non-response weights).

**Constructed and renamed columns**

- `participant_id` - `bcsid` for a cohort-member row; `bcsid` plus `_` plus the child number for a `child2004` row (for example `B10001N_3`).
- `wave` - `"age16"`, `"child2004"`.
  The spec's waves are UKDA study numbers, which are data releases rather than sweeps, so the tidier builds its own `wave` and drops the reader's `.wave` and `.wave_label`.
- `b7chdid` - the child number.
  `ChildID` in the assessment file and `ch` in the five self-completion files are renamed to it, and its value labels are stripped because each file labels only the child numbers it happens to contain, which made the join refuse the key.
  It runs 2 to 10: the cohort member is person 1 in the family grid.
- `yp_`, `pc0to11_`, `pc1to2_`, `pc3to5_`, `pc6to16_` prefixes on the five 2004 paper self-completions.
  Each form numbers its questions from one, so `q1` means five different things; without the prefix they would collide.
  The child assessment file keeps its own names (`basnv01`, `baswrA`, ...) because they do not collide.
- The `_age10` / `_age16` suffixes are gone: with the 1980 rows dropped, the eleven names the two questionnaires share (`c6.2`, `c6.5`, `c6.7` to `c6.14`, `status`) hold only the 1986 questions, and none of the joined 1980 items reuses a 1986 name.
- `mother_highest_qual`, `father_highest_qual`, `parents_highest_qual` (`bp1025_parent_qualifications()`) - each parent's highest ticked qualification, labelled codes 0-12: at 16 from the 1986 grid (`t6.*`, father, mother or both) else the 1980 grid (`c1.*`, with 'other' ticks classified in `c1.8` / `c1.19`); in 2004 the cohort member's own (`BD7HACHQ` degree-level, else `BD7HNVQ`) as mother or father by `bd7sex`, the partner's qualifications not being asked; `parents_highest_qual` is the higher of the two at 16 and NA in 2004.
- `sdq_p01`-`sdq_p25` (`bp1025_sdq_parent_items()`) - the parent SDQ items in SDQ order, pooled from the 6-16 form (`pc6to16_q1a`-`q1y`) and the 3-5 form (`pc3to5_q1a`-`q1v`, with items 18, 21 and 22 from `pc3to5_q3a`-`q3c` under age 4 and `pc3to5_q4a`-`q4c` at 4-5); codes 1-3.
- `partner_natural_parent` (`bp1025_partner_natural_parent()`) - for `child2004`, `b7wpar` at the child's household person number (`b7chdid`): whether the cohort member's partner is this child's natural parent.
- `BCSID` is lower-cased to `bcsid` in every file that spells it in upper case, so that one identifier name is used throughout.

Every other column keeps its source name and its variable and value labels.

## Files

**Used (17 data files)**

| release | file | role in the table |
|---|---|---|
| 3723 | `sn3723.sav` | 1980 qualifications grid, ethnic group and birthplace joined to `age16` |
| 3535 | `bcs7016x.sav` | `age16` spine |
| 3535 | `bcs4derived.sav` | joined to `age16` |
| 3535 | `bcs70_age16_school_type.sav` | joined to `age16` |
| 3535 | `jiig-cal_occupational_interests.sav` | joined to `age16` |
| 6095 | `bcs70_16-year_arithmetic_data.sav` | joined to `age16` |
| 8288 | `bcs1986_reading_matrices.sav` | joined to `age16` |
| 8949 | `bcs4_leisure_diary_aggregate.sav` | joined to `age16` |
| 5585 | `bcs_2004_parent_and_child.sav` | `child2004` spine |
| 5585 | `bcs_2004_child_assessment_bas.sav` | joined to `child2004` |
| 5585 | `bcs_2004_pc_yp_10to16.sav` | joined to `child2004` (`yp_`) |
| 5585 | `bcs_2004_pc_0to11mths.sav` | joined to `child2004` (`pc0to11_`) |
| 5585 | `bcs_2004_pc_1to2yr11mths.sav` | joined to `child2004` (`pc1to2_`) |
| 5585 | `bcs_2004_pc_3to5yr11mths.sav` | joined to `child2004` (`pc3to5_`) |
| 5585 | `bcs_2004_pc_6to16yr.sav` | joined to `child2004` (`pc6to16_`) |
| 5585 | `bcs_2004_followup.sav` + `bcs7derived.sav` | the cohort member's own 2004 interview, joined many-to-one to `child2004` as the parent's characteristics |

**Ignored (37 data files)**

- The whole of UKDA 3833 (age 26, 1996), UKDA 7473 (age 42, 2012) and UKDA 9347 (age 51, 2021) - 2 + 12 + 16 files.
  At each of these the cohort member is an adult (26, 42, 51), far above the dataschema's `age_years` maximum of 19, and none has a child participant.
  They are not excluded by the screen rule: 1996 has only computer-skill items (`b960228`, `b960230`), but 2012 has adult TV duration items for a typical weekday and weekend day (`B9SCQ10A`, `B9SCQ10B`) and 2021 has social-media time in a typical day (`b11smfreqh`) and frequency (`b11smfreq`) plus a device grid (`b11devices01`-`11`).
  Their exclusion is a study-population decision; see Decisions to confirm.
- `bcs70_age16_school_type.sav` as shipped with UKDA 7473 - byte-identical twin of the 3535 copy (both 18,035 x 4); the 3535 copy is used.
- `bcs70_1980_audiometry.sav` (3723) - 14,901 rows with only 14,897 distinct `BCSID`; the four repeats are an all-missing row plus a populated row for the same cohort member, so the file cannot be joined one-to-one as deposited.
  It holds seven audiogram measures (`RTOTLOSS`, `LTOTLOSS` and counts) that no dataschema variable uses.
- `bcs4_leisure_diary_episode.sav` (483,937 rows), `bcs4_leisure_diary_calendar.sav` (28,427 rows) and `bcs4_television_diary_event.sav` (59,783 rows) from UKDA 8949 - these are at episode, diary-day and television-event grain, not participant grain.
  The participant-level `bcs4_leisure_diary_aggregate.sav` derived from them is used instead.
- `bcs_2004_adult_assessment_basic_skills.sav` and `bcs_age34_dast.sav` (5585) - the cohort member's own adult literacy, numeracy and dyslexia assessments; not joined to the child rows.
  They could be joined as further parent characteristics if a reviewer wants them.

**Undeclared files found in the study folder**: none remain.
`describe_raw.R` reported nine (`UKDA-<n>-spss/mrdoc/UKDA/UKDA_Study_<n>_Information.htm`), which is the spec edit below.

**Spec edits**: one.
The nine `UKDA_Study_<n>_Information.htm` docs resources were globbed at `.../mrdoc/pdf/UKDA/...`; the files actually sit at `.../mrdoc/UKDA/...`, so the globs matched nothing and the files showed up as undeclared.
The extra `pdf/` was removed from all nine globs and from the sentence in `notes:` that repeats the path.
No data resource was added, removed or changed.

## Decisions to confirm

1. **Resolved 2026-10-01: dropped.** Dropping the three adult sweeps.
   The 1996, 2012 and 2021 releases are not in the table, because the cohort member is 26, 42 and 51 and so outside the children-and-adolescents population.
   The conventions say tidiers never filter on age and drop rows only to define the study population, and the 2012 and 2021 sweeps do carry adult duration items (TV weekday/weekend at 42, social media per day at 51), so the screen rule would not drop them.
   The project lead should confirm that a whole adult sweep counts as outside the study population; if not, 2012 and 2021 would be added as two more waves (1996 would still go under the screen rule).
2. **Resolved 2026-10-01: `age10` dropped.** Keeping `age10` although its only screen item is a frequency.
   `m88` (mother report, Never or hardly ever / Sometimes / Often) gives no duration, so under the screen rule the wave is kept for confirmation rather than dropped; it contributes outcomes and covariates but no `st_*` value.
3. **Resolved: only the 1980 grid carried forward now.** Parent education carried backwards as well as forwards.
   `carry_within()` takes the earliest observed sweep, so 1986 values (`t6.*`, `t7.*`) fill 1980 rows that have no 1986-style item, and 1980 values (`c1.*`) fill 1986 rows.
   The two grids use different response schemes and stay in separate columns; the mapping chooses between them.
4. **Resolved 2026-10-01: kept.** Including the 2004 Parent & Child survey as its own population.
   `child2004` rows are a *second generation*: the cohort members' children, not the cohort members.
   They carry the study's only screen-time item measured in hours (`yp_q1`, `yp_q2`), so they were kept, but they mix two populations in one table and a reviewer may prefer them split out or excluded.
   `wave == "child2004"` selects them.
5. **The 2004 cohort-member interview attached to child rows.**
   All 2,640 columns of `bcs_2004_followup.sav` plus the 16 of `bcs7derived.sav` ride along on each child row as the parent's characteristics.
   404 of them are NA for every child row because the Parent & Child half-sample was never routed to those modules.
6. **Every column is kept.**
   No column map is applied.
   The table is 11,795 columns wide and 2.8 Gb in memory, but it serialises to 67 MB in the targets store (against 552 MB for `tidied_BPIPD_107`) and the tidier runs in 12 seconds, so size did not force a cut.
   BCS70 variable names are opaque (`m88`, `c5j1a`, `j136`, `pc6to16_q1a`), and the instruments this project wants - the Rutter behaviour scale, the Malaise Inventory, GHQ-12, the SDQ, the British Ability Scales - are scattered through unrelated question blocks, so a hand-built map would very likely have dropped some of them.
   If the store size does become a problem, the blocks to cut first are the 803 `i*` item-level educational test columns and the 490 `mea*`/`meb*` medical-examination columns at age 10 (their derived scores survive in `bcs3derived.sav`), and the 116-column `q35.*` drug-use grid at age 16.
7. **Resolved: suffixes removed with the `age10` rows.** The eleven `_age10` / `_age16` suffixes are a hard-coded list in `bp1025_split_reused_names()`.
   It was derived by intersecting the two sweeps' column names and comparing the labels; if the deposit changes, that list must be rechecked.
8. **Non-substantive codes are carried with parent education.**
   `carry_within()` treats every non-missing code as observed, so `t6.*` code 4 "No response" (4,265 of the 7,335 answered `t6.1` values), `t6.10` "not known", and the 1980 "not known" and "no male head" ticks (`c1.10`, `c1.11`, `c1.21`, `c1.22`) are carried like any qualification.
   The mapping should treat them as it would on the wave that recorded them.
9. **The diary average is not a usual-day value.**
   The diary covers a consecutive Friday, Saturday, Sunday and Monday (diaries user guide, section 1.2), so `total_activity_31 / total_days` weights weekend days at about half rather than the 2/7 of (5 x wd + 2 x we) / 7.
   Mapping it to the unsuffixed `st_watch` should say so in the notes, or the tidier should aggregate the calendar file by weekday and weekend day (Open question 4).

## Rows dropped

The whole `age10` wave (14,868 rows from `sn3723.sav`) is dropped under the screen rule, as confirmed on 2026-10-01; 3,998 cohort members seen only at 10 leave the table.
Otherwise each wave's spine is used in full and every join is a `left_join`; no participant-wave is dropped by attendance and no age filter is applied.

Rows that exist in a *joined* file but have no spine row, and so contribute nothing:

- 4 of 3,475 rows of `jiig-cal_occupational_interests.sav` have no matching `age16` row;
- 1 of 3,677 rows of `bcs70_16-year_arithmetic_data.sav` has no matching `age16` row;
- 6,421 of 18,035 rows of `bcs70_age16_school_type.sav` have no matching `age16` row (that file covers the whole birth target sample).

The 37 ignored files above are excluded as whole files, not as row filters; see **Files**.

## Done in the tidier that is arguably harmonisation

Almost nothing.
Three items are worth naming:

- Parent education: the 1980 grid (`c1.1`-`c1.22`) is joined to the age-16 row, and `mother_highest_qual`, `father_highest_qual` and `parents_highest_qual` rank each parent's ticked qualifications; the qualification ranking is a study-structure choice the mapping notes describe.
- The parent SDQ items are pooled across the 3-5 and 6-16 forms (`sdq_p01`-`sdq_p25`); scoring stays in the mapping.
- The child number (`ChildID` / `ch` / `b7chdid`) is cast to a plain integer and its value labels are dropped.
  This was forced: each 2004 file labels only the child numbers it contains, so the labelled key would not join.
  The variable label is replaced with "Child number within the cohort member's family".
- No sentinel-code zapping, no scoring, no recoding, no unit conversion, no coalescing.
  The SPSS reader has already turned almost every declared user-missing code into `NA` (see **Pointers for harmonisation**), so no blanket zap was needed.

## Pointers for harmonisation

Since 2026-10-01 the `age10` rows are gone; the age-10 pointers below are kept for reference, and only the carried items (`c1.*`, `a12.1`, `a12.4`, `a12.7`, `a2.1`) remain in the table.

### Identifiers, wave, year

- `participant_id` (character), `wave` (character, `age10` / `age16` / `child2004`), `bcsid` (character, the cohort member throughout - for `child2004` it identifies the **parent**, not the participant), `b7chdid` (integer, `child2004` only).
- `wave` maps from the tidier's `wave` column (the reader's `.wave` was dropped).
- `data_year`: per-row `b7intyr` for `child2004` (compatible); `age10` is 1980 and `age16` 1986 (fieldwork ran into 1987), assigned from the user guides (inferred), or per row from the document completion years (`fdoc_yr` etc.) at 16 and `j009c` (teacher form year) at 10.
  `child2004` also carries `b7intyr` (2004 or 2005) and `b7intmon` per row; the diary aggregate carries `year` (1986/1987, with an undeclared `9999` for undecipherable), `month` and `day`.

### Age

- `age10`: `bd3age`, derived age in years when the assessments were sat, range 9.81-11.61, non-NA for 9,254 of 14,868.
  Date of birth is in `doba10` / `dobb10` / `dobc10`.
- `age16`: `BD4AGE`, derived age when the assessments were sat, range 15.78-17.71, non-NA for only 3,967 of 11,614 (it is computed from the Document F completion date).
  `dob86` holds the date of birth.
- `child2004`: `b7age` (whole years, 0-16, complete for all 5,207) plus `b7nmonth` (month part, 0-11); the assessment file repeats them as `Age` and `NMonth`.
  `b7age + b7nmonth / 12` is a decimal age; `b7age` alone would take the +0.5 rule.
- Where `bd3age` / `BD4AGE` are missing, the Age and time rules allow a decimal age from the date of birth (`doba10`-`dobc10`, `dob86`) and a per-row completion date (`j009a`-`c`, `odoc_mt`/`odoc_yr`, `fdoc_mt`/`fdoc_yr`), or survey year minus birth year adjusted by the fieldwork midpoint; the tidier fills no age because every kept wave has an age item.
- Ages above the `age_years` maximum (19) never occur here; the oldest `age16` value is 17.71.

### Sex

- `age10`: `sex10` (1=Male, 2=Female, 3=Not Known; complete).
- `age16`: `sex86` (complete, 11,614; **codes 1 and 2 are unlabelled** - only the negative missing codes carry labels).
  `SEX` from the JIIG-CAL file is present for 3,471 rows and `sex86` from the diary aggregate was dropped as a duplicate of the spine's.
  The reading-and-matrices file (8288) also has a `SEX`, dropped because the JIIG-CAL `SEX` is joined first, so its values on 889 rows without a JIIG-CAL record are not in the table; `sex86` is complete, so no sex information is lost.
- `child2004`: `b7csex` (1=Male, 2=Female; complete).
- Sex is carried in the mapping, not the tidier: modal observed value within `participant_id` (`modal_within()`), ties Unknown; demographic rows emit "Unknown", never NA.

### Country and region

- The cohort is Great Britain only (England, Wales, Scotland).
- `age10`: `bd3cntry` (1=England, 2=Wales, 3=Scotland, 4=Northern Ireland declared but unused; 14,787 non-NA) and `bd3regn`.
- `age16`: `BD4CNTRY` (same coding, 11,417 non-NA), `BD4REGN`, `BD4GOR`, plus `land86` ("Which country in Great Britain", complete but with **unlabelled codes 1-3**).
- `child2004`: the parent's `BD7CNTRY`, `BD7REGN`, `BD7GOR` (complete for all child rows).
- Urbanicity at 16: `c6.13_age16` ("Do you live in city-town-village-country": big city / town / village / the country).

### SES and parental education

- `age10`: `bd3psoc` (social class from the father's occupation, or the mother's if missing; 1=V unskilled to 6=I professional; 13,228 non-NA), `bd3inc` (gross weekly family income), `bd3ben` (state benefit in the last 12 months).
  Parental qualifications are a 22-item tick grid, `c1.1`-`c1.11` for the father and `c1.12`-`c1.22` for the mother (trade apprentice / O level / A level / SRN / Cert Ed / degree / other / none / not known).
  Parental employment is `c2.1`-`c2.16`; accommodation is `d1.*`.
- `age16`: `BD4PSOC` (derived from `t11.2` + `t11.9`; -3=parent dead, 0=student, 1=unskilled to 6=professional; 6,714 non-NA).
  Raw social class is `t11.2` (father) and `t11.9` (mother), coded I / II / III non-manual / III manual / IV / V / student / dead.
  Parental education: `t7.1` and `t7.2`, the **age the father and the mother finished full-time education** (range 10-50, ~6,800 observed at 16 each, carried to `age10` rows by the tidier) - the cleanest parental-education variable in the study, though an age rather than a level.
  Parental qualifications as a tick grid: `t6.1`-`t6.7`, `t6.9`, `t6.10` (1=Father, 2=Mother, 3=Both, 4=No response; there is no `t6.8`), also carried to `age10` rows.
  The 1980 grid `c1.*` (ticks are 1, unticked is NA) is carried to `age16` rows.
  Parent education uses one vocabulary with ISCED anchors and all three parent-education rows call the same lookup; a parent's own characteristic used for the child is proximate.
  Household income: `oe2` (combined parental income per week/month) with sources `oe1.1`-`oe1.19`.
  Housing: `c6.8_age16` (tenure), `c6.9_age16` (dwelling type), `c6.11_age16` (rooms), `c6.12_age16` (people in the household).
- `child2004`: all of the parent's 2004 variables are on the row - `bd7hq5` and `bd7hq13` (highest academic qualification, reduced and detailed), `BD7HACHQ` / `BD7HNVQ` / `BD7HANVQ` / `BD7HVNVQ` (highest academic and NVQ-level qualifications up to 2004), `bd7ns8` (NS-SEC 8 analytic version, 3,913 non-NA), `b7nssec`, `b7sc` (old-scheme social class), `b7seg`, `b7ten2` (tenure), `b7accom`, `b7numrms`.
  Note these describe the **parent**, which is what `parental_education` and `ses_household` want for a child participant.
- Household SES follows the SES cuts (40/20/40 within wave; a banded source by ridit); social class (`bd3psoc`, `BD4PSOC`, `b7sc`) is a fixed class scheme with its standard three-way collapse (proximate).
  SES and family structure are not carried between 1980 and 1986 (gap about six years), so waves without the item are "Unknown".
- Survey design (`survey_weight`, `survey_strata`, `survey_cluster`): none in the kept sweeps; 'Not a survey sample.'

### Family structure and school

- `age16`: `c6.7_age16` ("Who does teen live with as parent?": real mother & father / mother & new father / father & new mother / mother alone / father alone / a relative / someone else).
- `BDSTYPE` (`age16`, 9,957 non-NA) - school type at 16, derived at age 42 from `STYPE`, `B9SC16TP` and the 1986 School Census; see `bcs70_age_16_school_type_variable_user_guide.pdf`.
- `child2004`: `pc6to16_q9a`-`q12b` (school absence, suspension, exclusion), `pc6to16_q18`-`q20` (homework), `yp_q14` ("How much does child enjoy school"), `yp_q16` (truancy in the last year).

### Ethnicity

- `age16`: `c6.14_age16` (self-reported: European / West Indian / Asian / Chinese / mixture / other race) and `oa3.1`-`oa3.3` (ethnic group of teenager, mother and father, from the parental interview).
- `child2004`: the parent's `BD7ETHNIC`, and `b7pcethe` / `b7pcethw` / `b7pceths` on the child row.

### Screen-time items

Conventions that decide these mappings: no item anywhere asks about all screen use, so `st_total*` is incompatible or unavailable and is never a sum (DR-0004); never sum across watching, gaming and computer (DR-0002), though TV plus video films are sources within watching and may be summed there; frequency, time-of-day and ownership items give no hours and are incompatible, item named.
The primary series is the self-report questionnaire (`age16`, `child2004`); the 1986 diary is a second instrument on the same occasion and maps as a `diary`-tagged block (DR-0003), never as a wave.

**`age10` (mother report, 1980)** - one item only:

- `m88` "B1 E WATCHES TELEVISION", 1=Never or hardly ever, 2=Sometimes, 3=Often; 13,580 non-NA.
  It sits in a grid of spare-time activities (`m84` sports, `m85` records, `m86` books, `m87` bicycle, `m89` club, ... `m95` library), so it is a frequency of the activity, not a duration.
  Responder: the mother, on the Maternal Self-completion Form.
  Frequency only, so `st_watch*` rows are incompatible at 10 (the wave is kept; see Decisions to confirm).

**`age16` (cohort member self-report, 1986)** - several, in three response formats:

Duration "yesterday after school" (Document F; `-` codes are already `NA`; 0=Not at all, 1=Less than 1 hr, 2=More than 1 hr, 3=More than 2 hrs, 4=More than 3 hrs, 5=More than 4 hrs, 6=More than 5 hrs):

- `f13` "How long watch tv after school yesterday?" (4,402 non-NA)
- `f14` "Time watch video film after schl yester?" (4,418)
- `f15` "Time play comp. games after schl yester?" (4,408)

These are the only duration-band screen items for the cohort member, and they refer to a single day after school, not to an average weekday or weekend day.
They are school-day items, so they belong on `_wd` rows (`f13` + `f14` on `st_watch_wd` as sources within watching, `f15` on `st_game_wd`), with the after-school-only and yesterday-only scope stated in notes.
Bands: 0 = 0, 'Less than 1 hr' = 0.5; each 'More than N hrs' below the top is followed by 'More than N+1 hrs', so it reads as the band N to N+1 (midpoint N + 0.5), and the open top band 'More than 5 hrs' takes 5; write the top-band value in notes.

Frequency in the past month (Document G; 1=Most days, 2=2-3 times a week, 3=1-2 times a week, 4=Less than once/wk, 5=Never):

- `gf1.1` "How often watched TV in past month?" (6,236), `gf1.4` video films, `gf1.5` videoing TV programmes, `gf1.2`/`gf1.3` video nasties and pornographic videos.

Frequency of leisure activities (Document C; 1=Rarely/never, 2=Less than once week, 3=Once a week, 4=More than once week):

- `c5j1a` "Stay at home and watch TV" (5,575), `c5j2a` "watch videos" (5,518), `c5j8a` "Use home computer" (5,489), `c5j14a` "Play electronic games" (5,485).

Time-of-day TV (Document J; 1=Most days to 5=Rarely or never): `ja2a` early morning, `ja2b` morning, `ja2c` lunchtime, `ja2d` afternoon, `ja2e` evening 6-9pm, `ja2f` evening 9-11pm, `ja2g` late night.
Motives for watching: `ja1a`-`ja1k`. Content watched: `c5n8`-`c5n22` and `ja5`.
Ownership/access, not time: `c5q6` (own TV), `c5q7` (video recorder), `c5q8` (electronic TV games), `c5q22` (home computer), each 1=Own one / 2=Would like one / 3=Wouldn't want one; and household-level `pg1.3` television, `pg1.4` video recorder, `pg1.8` home computer.

**`age16` time-use diary (8949)** - a **second screen-time measure** for the same participant-wave, and the natural candidate for a `measure` tag in `variables.csv`:

- `total_activity_31` "Total time spent: TV, video", in **minutes summed over the whole diary**, range 0-2510, median 359, present for 7,242 of the 11,614 age-16 rows.
- `total_days` is the number of diary days the total covers (1=118, 2=123, 3=254, 4=6,747 cohort members), so minutes per day is `total_activity_31 / total_days`.
- `total_duration` (1,440 to 11,520) is the total recorded minutes and should equal 1,440 x `total_days`.
- `riskdiary` flags a diary with 7 or fewer episodes per day or missing time (0=More than 7 episodes, 1=7 or fewer).
- Diarists were asked to keep a Friday-Saturday-Sunday-Monday diary, so a weekday/weekend split is **not** available from this aggregate file; it would need the episode or calendar file, which this tidier does not read.
  The diaries user guide notes 75% were collected between June and September.
- The other forty `total_activity_*` columns are the remaining time-use categories (16=sleep, 4=school/classes, 19=active sport, and so on) and are useful as denominators.
- In the `diary` block, minutes / 60 per diary day go to the unsuffixed `st_watch` only (no weekday/weekend split, so `_wd` and `_we` are unavailable); the block maps `st_measure_type`, `st_measure_name` ('Time use diary') and `st_responder`.

**`child2004` (2004-05)** - the only hours-banded items in the study, self-reported by children aged 10-16 (n=942, so ~97% of all rows are NA):

- `yp_q1` "Number of hours child spends watching TV (mon-fri)": 1=None - I don't watch TV (7), 2=Less than an hour (132), 3=1 to 3 hours (521), 4=4 to 6 hours (255), 5=7 hours or more (22).
  The printed questionnaire wording is "On a normal school day, how many hours do you spend watching TV, including videos and DVDs?" (`bcs70_2004_yp_self_completion.pdf`, Q1), so this is a **weekday** item covering TV, video and DVD.
- `yp_q2` "Number of hours child spends playing computer games on computer (mon-fri)": 1=Less than an hour (373), 2=1 to 3 hours (309), 3=4 to 6 hours (57), 4=7 hours or more (11), 5=None, I don't play computer games at home (150), 6=I don't have a computer that I can use at home (39).
  Note that the "none" categories are coded **above** the hour bands, not below them, and that code 1 is the lowest band rather than "none" - the two items are not coded the same way.
  Questionnaire wording: "On a normal school day, how many hours do you spend playing computer games on your computer at home?" (Q2).
- Mapping: `yp_q1` to `st_watch_wd`, `yp_q2` to `st_game_wd` (a computer-only gaming item leaves out phones and tablets, so partial); school-day items only, so the unsuffixed rows are incompatible ('Weekday items only; no usual-day value.') and `_we` unavailable.
  Bands: none = 0, 'less than an hour' = 0.5, '1 to 3' = 2, '4 to 6' = 5, open top '7 hours or more' = 7.
  Non-users rule: `yp_q2` codes 5 (does not play) and 6 (no computer at home) are 0.
- Access, not time: `pc6to16_q21` "Is there a computer at home that child can use for homework" (parent report, 1=Yes 1,905, 2=No computer at home 251, 3=Computer but not for my child 53).
- The parent's own 2004 items are on the row too and are **not** the child's screen use: `b7pchome`, `b7hpcuse`, `b7pcwork`, `b7wpcuse`, `bd7hpc04`, `bd7video`.

### Outcome instruments

**Behaviour**

- `age10`: the **Rutter A2 parent scale** as 39 items, `m43`-`m82` (A1-A19 and B1-B19), mother report; `m83` is the Rutter pattern code and `bd3mrutt` / `bd3mrutg` the derived total and grouping.
  Teacher report: the Child Health and Education Study behaviour rating scale, `j122`-`j166`+, each a 1-47 visual-analogue scale (for example `j136` "clumsy at games", `j150` "restless or over-active", `j155` "pays attention in class").
- `age16`: `BD4RUTT` / `BD4MRUTG`, the mother-reported Rutter total (0-38, 7,804 non-NA) and its grouping, from the Document P items `pa5.*`.
- `child2004`: the **SDQ, parent report, all 25 items**, `pc6to16_q1a` to `pc6to16_q1y` (1=Not true, 2=Somewhat true, 3=Certainly true; ~2,280 non-NA), plus the impact supplement `pc6to16_q2`-`q6`.
  The 3-5 form carries 22 of the 25 items as `pc3to5_q1a`-`pc3to5_q1v`: "lies or cheats", "thinks things out before acting" and "steals" are not on it, so conduct has 3 of 5 items and hyperactivity 4 of 5 there (label check of `bcs_2004_pc_3to5yr11mths.sav`).
  The `pc0to11_q1*` and `pc1to2_q1*` grids are developmental-milestone items ("Has child ever crawled ..."), not the SDQ.
  Per the SDQ rule, pool the parent forms (recording the form version and the items administered), rebase 1-3 to 0-2, reverse items 7, 11, 14, 21 and 25, and score subscales as mean of answered items x 5 with at least 3 of 5 answered.
  Bullying at 2004 also has single items: `yp_q36` (been bullied) and `yp_q37` (bullied others).

**Mental health**

- `age10`: mother's own **Malaise Inventory**, `m254`-`m279` (24 items), with `bd3mmal` / `bd3mmalb` the derived total and grouping - this is the *parent's* mental health, not the child's.
  Child items in the pupil form: `k011` "feel lonely at school", `k017` "feel sad because no-one to play with", `k029` "worry a lot".
- `age16`: the cohort member's **own Malaise Inventory (22 items)**, `c5o1`-`c5o22`, with `BD4MAL` the total (0-44, 5,448 non-NA) and `BD4MALG` its grouping.
  Also the **GHQ-12**, `c5i1`-`c5i12` (1=More than usual to 4=Much less tn usual), which includes "Reasonably happy all things considered" (`c5i4`) and "Been feeling unhappy and depressed" (`c5i10`).
  The mother's Malaise total is `BD4MMAL` / `BD4MMALA` / `BD4MMALB`.
- `child2004`: the SDQ emotional-symptoms items are within `pc6to16_q1a`-`q1y`.

**Wellbeing**

- `age10`: the pupil self-completion (Document/Form K) carries the **LAWSEQ self-esteem** and **CARALOC locus of control** items, `k010`-`k025` (for example `k019` "things about self would change", `k020` "feel foolish in front of peers"), and a personality grid `k026`-`k033`.
- `age16`: `f22score` "Total score for self-esteem question", plus the GHQ-12 above.
- `child2004`: `yp_q19`-`yp_q30`, a 12-item self-perception and self-esteem block ("Extent to which child is unhappy with themselves", "... is happy with themselves", "... wishes they were like someone else", "... feels as clever as their peers", "... does very well with their school work").
  Relationship quality with the cohort member: `pc6to16_q7a`-`q7m`.

**Cognition and academic achievement**

- `age10`: `bd3read` (standardised Edinburgh Reading Test score, -3.23 to 1.95, 9,584 non-NA), `bd3rread` (raw), `bd3rdage` (estimated reading age), `bd3maths` (Friendly Maths Test score, 1-72, 11,631 non-NA).
  Item-level test responses are the 803 `i*` columns.
  Teacher and self ratings: `k036` ability in maths, `k037` ability in reading, `k038` ability in spelling.
- `age16`: `BD4READ` (standardised Vocabulary Test score, -3.4 to 2.55, 4,720 non-NA), `BD4RREAD` (raw, items `cv01`-`cv075`), `BD4RDAGE`.
  `mathscore` "BCS70 16-year Arithmetic scores (out of 60)" and `mathincorrect` from the 6095 release.
  The 8288 release adds the 1986 reading and matrices tests as per-question scores `SCR_A1`-`SCR_A10` (skimming), `SCR_B*` (vocabulary) and the rest of its 185 columns; see `bcs70_1986_reading_and_matrices_tests_data_note.pdf`.
  Occupational interests from JIIG-CAL (194 columns) are on the row too; see `jiig-cal_occupational_interests_dataset_user_guide.pdf`.
- `child2004`: **British Ability Scales II**, administered by the interviewer.
  `basnvR` / `basnvA` Naming Vocabulary raw and ability score (ages 3:0-5:11), `basencR` / `basencA` Early Number Concepts (3:0-5:11), `baswrR` / `baswrA` Word Reading (6:0-16:11), `basnsR` / `basnsA` Number Skills (6:0-16:11), and `basspR` / `BASsp10` / `BASsp20` / `bassp1hr` revised BAS Spelling.
  Item-level responses are the `basnv01`-style columns.
  See `bcs70_2004_guide_to_child_assessments.pdf`.

### Newly added dataschema variables

Candidates only; the mapper judges the status by the conventions.

- `born_in_country`: `a2.1` "Child's country of birth" (1980, mother, 13,640) and `oa2.1` "Teenager's country of birth" (1986, parental interview, 9,439); carried in the mapping, not the tidier; UK as a whole counts.
  `immigrant_background`: no direct item found; parents' countries of birth were not searched in full.
- `fam_screenrules`, `fam_bedroomscreen`, `st_messaging*`, `st_creating`: no item found.
- `fam_parentstress`: none; the mother's Malaise Inventory (`m254`-`m279`, `BD4MMAL`) is parental mental health, not stress.
- `beh_truancy`: `q22.8` "Stay away from school more than 1 week" (16, self, past-year counts), `yp_q16` "skipped/bunked off school in the last year" (2004, self; 1=Never to 4=5 times or more, 5=Don't know, 6=I don't go to school); `m20` (10, mother) and `j172`/`j115a` (10, teacher) are other informants.
- `beh_stealing`: `q22.7`, `q22.21` (shoplifting over / under 5 pounds), `q22.9`, `q22.17`, `q22.23` (16, self); `yp_q35` "ever stolen from a store" (2004, self, lifetime yes/no, so partial); SDQ `pc6to16_q1v` is truth-rated and never maps to a binary row.
- `beh_vandalism`: `q22.1` "Broken windows/smash property not yours" (16, self), `hc3.4` (16, self).
- `beh_alcohol_self`: `hd1` "How often in past year drank alcohol?" (16, self), `yp_q32` (2004, self, never / once or twice / used to / sometimes / weekly); `pg8.1` is the mother's report at 16.
- `beh_tobacco_self`: `c6.18` cigarettes per week and `f43a`/`f43b` (16, self), `yp_q31` (2004, self); `og2.1` is the parent's report.
- `beh_cannabis_self`: `q31.7` "Have you ever tried taking cannabis?" (16, self, lifetime), `yp_q33` (2004, self).
- `mh_lonely_*`: `c5h23` "I am lonely" (16, applies very much / somewhat / not; not a frequency scale), `f22e` "Do you often feel lonely at school?" (16), `k011` (10, "feel lonely at school").
- `mh_sad_self`, `mh_worry_self`, `mh_somatic*`, `mh_psychsom*`: the 16-year Malaise items `c5o3` miserable or depressed, `c5o5` worry, `c5o1` backache, `c5o4` headaches, `c5o18` upset stomach, `c5o2` tired, `c5o6` difficulty sleeping (Most of the time / Some of the time / Rarely or never); the HBSC-SCL was not administered.
- `wb_happiness_singleitem_self` and `wb_domainsat_*`: the 16-year "In comparison" grid `c5g*` (`c5g7` "I am happy", `c5g1` amount of sleep; Much less / About the same / Much more than others) and 2004 `yp_q20`-`yp_q30` self-perception items; none is a satisfaction rating of life or of a domain, so check wording before mapping.
- `wb_generalhealth_parent`: no general-health rating of the child found in the 2004 parent interview (only conditions and hospital admissions).
- `hlth_sleep`: diary `total_activity_16` (sleep minutes over `total_days`, 16) as a usual-day value.
  `hlth_sleepprob`: `m36` (10, mother), `pa4.1` (16, mother), `c5o6` (16, self).
- `beh_problematicuse*`, `beh_prosocial_online*`, `acc_schoolbelonging*`, `acc_schoolpressure_self`, `mh_selfharm_self`, `mh_lifenotworth_self`, `wb_eudaimonic*`, `wb_posaffect*`, `wb_socsupport*`, `cog_creativethinking_stdtest`: no item found in label searches.

### Missing-value codes and label quirks

- **The SPSS reader has already converted almost every declared user-missing code to `NA`.**
  A full scan found negatives surviving in only 6 of 2,963 `sn3723` columns (`b6.2`, `b16.22`, `j300`, `i2539`, `i3503`, `i4140`), 0 of 4,693 `bcs7016x` columns, 0 of the 2004 child files, and 94 of 2,640 `bcs_2004_followup` columns (mostly `b7xqa*`).
  `bd3read` and `BD4READ` also hold genuine negatives - they are standardised scores, not sentinels.
  So no blanket `na_if()` is needed, but the `b7xqa*` block and those six age-10 columns should be checked individually.
- The value-label sets still *declare* the sentinel codes (`-9` Refusal, `-8` Don't know, `-4` Not asked, `-3` Not stated, `-2` Not stated, `-1` No questionnaire / Not applicable), so `haven::as_factor()` output will show levels with zero observations.
- Several columns carry **only the missing-code labels** and leave the substantive codes unlabelled: `sex86` and `land86` at 16, `c6.5_age16`, `c6.11_age16` and `c6.12_age16` (the count questions, where only the top category is labelled).
  Take their meaning from the variable label and the questionnaire, not from `as_factor()`.
- `bd3mrutt` (range 0-1854) and `bd3mmal` (range 1-2154) run far above the plausible range of a Rutter or Malaise total.
  The 1980 derived-variables guide (`bcs70_derived_variables_at_1980_sweep.pdf`, Appendices 2 and 3) should be read before either is used; the age-16 equivalents `BD4RUTT` (0-38) and `BD4MAL` (0-44) look correct.
- `year` in the diary aggregate has a maximum of 9999, which is an undecipherable marker not covered by the declared labels.
- 517 columns are `NA` in every row.
  This is not an assembly fault: 113 are all-`NA` in their own source file (15 in `sn3723`, 10 in `bcs7016x`, 88 in `bcs_2004_followup`), and the other 404 are `bcs_2004_followup` columns that the Parent & Child half-sample was never routed to.
- Value labels for the same code can differ between the age-10 and age-16 questionnaires; only the carried 1980 items (whose names the 1986 questionnaire does not use) remain, so no column carries both.

## Open questions / spec problems

1. **The spec treats a UKDA study number as a wave, and it is not one.**
   Age 16 alone is four study numbers (3535, 6095, 8288, 8949), and 5585 holds two different populations (the cohort member at 34 and their children).
   The tidier therefore builds its own `wave` and drops the reader's `.wave`.
   The spec's nine waves are still the right unit for *reading* the files, so nothing needs to change, but `wave` in the tidied table will not match `spec$waves` and a reviewer should know that.
2. **Sample sizes are 7 and 8 rows short** of the 14,875 and 11,622 the 1980 and 1986 user guides report.
   Neither the user guides, the identifier-realignment note, nor the file-information sheets in the folder explain it, and I did not find an explanation on the CLS site.
3. **Whether the `child2004` generation belongs in this project at all** is a scientific call; see Decisions to confirm.
4. **The diary can support a weekday/weekend split, but not from the file this tidier reads.**
   `bcs4_leisure_diary_calendar.sav` has one row per diarist per day with a `diaryday` weekday marker, and `bcs4_leisure_diary_episode.sav` the underlying episodes.
   If `st_watch_wd` and `st_watch_we` are wanted from the 1986 diary, the tidier must be extended to aggregate one of those files per participant and weekday/weekend, which is a bigger change than a mapping expression.
5. **Age at the age-16 sweep is known for only 3,967 of 11,614 rows** because `BD4AGE` depends on the Document F completion date.
   Age could be derived instead from `dob86` and a per-document completion date, but choosing which date is a harmonisation decision and was left alone.
6. The `bcs_2004_adult_assessment_basic_skills.sav` and `bcs_age34_dast.sav` files are the parent's literacy, numeracy and dyslexia measures and are not currently joined to the child rows; say if they should be.

## Convention review (2026-09-25)

- Tidier: parent education (`c1.1`-`c1.22`, `t6.*`, `t7.1`, `t7.2`) is now carried within participant between `age10` and `age16` with `carry_within()`, 103,282 values filled, 0 observed values changed; rows and columns unchanged (31,689 x 11,795).
- Tidier: header cut to two summary lines with the grain in Output; helper description blocks cut to one-line titles and one-line comments.
- Checked and not needed: no wave dropped by the screen rule (`age10` kept with a frequency-only item, flagged), no non-attender rows, no SES, family, age or design fills (sweeps six years apart; every wave has an age item; no design variables), no sex or ethnicity carry.
- Notes: corrected the claim that the adult sweeps have no screen item (2012 and 2021 have adult duration items) and reframed their exclusion as a population decision to confirm.
- Notes: added participants (20,819), screen-rule, attendance and fill accounting, and convention-based pointers (DR-0002, DR-0003 `diary` block, DR-0004, band values, Non-users, SDQ rule, age, SES, survey design).
- Notes: added pointers for the newly added dataschema variables the study can fill (birthplace, truancy, stealing, vandalism, alcohol, tobacco, cannabis, loneliness, Malaise symptoms, sleep).
- Verification: counts, fill counts (103,282; 0 observed values changed), key uniqueness and the parent-education column set re-checked against the raw releases; notes corrected on the 3-5 SDQ form (22 items; the infant grids are milestones, not SDQ), the age-16 participation flags and the dropped 8288 `SEX`; decisions 8 and 9 added.

## Revision (2026-10-01)

Decisions from the project lead and the changes they caused:

- Decision 1 (adult sweeps): confirmed dropped; no change.
- Decision 2 (`age10`): the wave is dropped; the 1980 items on fixed characteristics ride along on the age-16 row (see Values carried).
- Decision 4 (`child2004`): kept as a second-generation population.
- Mapping expressions were simplified by moving three constructions into the tidier: each parent's highest qualification, the pooled parent SDQ items and `partner_natural_parent`.
- The SDQ 3-5 form is complete: items 18, 21 and 22 are in `pc3to5_q3*` / `pc3to5_q4*`, so earlier notes saying conduct and hyperactivity had 3 and 4 of 5 items at 3-5 were wrong.
- New dataschema rows used by this dataset: `mh_distress_malaise_self`, `mh_distress_ghq12_self`, `cog_earlynumber_bas_enc`, `acc_literacy_bas_wordreading`, `acc_numeracy_bas_numberskills`.
