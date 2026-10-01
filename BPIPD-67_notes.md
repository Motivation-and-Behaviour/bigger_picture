# BPIPD-67 (PATH) - tidier notes

## Revision 2026-10-01: full ICPSR 36498 release

**This section supersedes the rest of these notes wherever they differ; the later sections describe the 2026-09-25 build.**

The study folder was re-uploaded as the complete ICPSR 36498 v25 release (2026-07-09): 78 `DS*` folders, every `.rda` checked against the MD5 sums in `36498-manifest.txt` (all 77 match; ICPSR releases `DS5503`, an ever/never tobacco reference file, without an `.rda`).
The half-wave release ICPSR 37786 (waves 4.5, 5.5 and 7.5) was restored the same day under `Supplementary Cohort/` (the three youth/parent files only; its half-wave weight files are not in the folder).
New in the folder: the Wave 8 youth file (`DS8002`, 8,002 youth, fieldwork 22 Jan-31 Dec 2024) and the per-wave weight files for waves 3-8.

**Spec.** Resources renamed `youth_w1` … `youth_w8` (`DS1002` … `DS8002`), `youth_w4_5`, `youth_w5_5`, `youth_w7_5` (37786 `DS1002`, `DS2002`, `DS4002`) and `weights_w3` … `weights_w8`, with a codebook for each, plus the user guide, the youth crosswalk, the youth questionnaires of both releases and the nonresponse reports as `role: "docs"`.
The weight file read for each wave is the one PATH designates for cross-sectional estimates (User Guide Table 6.1): `DS3202` (Wave 3 single-wave), `DS4322` (Wave 4 Cohort cross-sectional), `DS5222` and `DS6222` (Wave 4 Cohort single-wave), `DS7332` (Wave 7 Cohort cross-sectional) and `DS8232` (Wave 7 Cohort single-wave); waves 1-2 release their weights in the youth files.
Not read: adult files and adult weights (18 and over), the youth all-waves, older-cohort single-wave and special-collection weights (longitudinal analyses), and the ever/never reference files.

**Tidied table.** 118,230 rows (one per youth per wave interviewed), 3,782 columns, keyed on `PERSONID` + `wave` (checked unique).
Rows per wave: 1 = 13,651; 2 = 12,172; 3 = 11,814; 4 = 14,798; 4.5 = 13,131; 5 = 12,098; 5.5 = 7,129; 6 = 5,652; 7 = 10,834; 7.5 = 8,949; 8 = 8,002 - each equals the youth interview count in User Guide Table 1.2.
Every wave 1-7 value present before the re-upload is unchanged apart from the design columns.

**Tidier changes.**

- `wave` now comes from each file's variable-name prefix (`R01` = wave 1, `X04` = wave 4.5; User Guide section 7.4).
- Each wave's weight file is joined one-to-one on `PERSONID`; its full-sample and replicate weights are renamed to the `Y_PWGT` stem the wave 1-2 youth files use, so `Y_PWGT` and `Y_PWGT1` … `Y_PWGT100` are the wave's cross-sectional weight at every wave (label lists the source variable per wave).
  The weight is **no longer carried**; the half-wave rows are `NA` (no weight files), as are 475 rows outside the wave's weighted cohort (4: 5, 5: 122, 6: 67, 7: 184, 8: 97). Weighted totals are about 25 million youth per wave (16 million at wave 6, ages 14-17).
- `VARSTRAT` and `VARPSU` come from the youth files (waves 1-2) and the weight files (3-8); they never change within a youth (checked across all eight waves, 36,405 youth), so they are carried to rows without a weight record, half-waves included; 670 rows stay `NA`.
- `parent_relationship` (new): the parent respondent's relationship to the youth.
  PATH asks it (`PT0001`) only when the parent did not answer at the PATH wave immediately before, half-waves included (questionnaire box `PXR03` at every wave), so the tidier fills a skipped value from the youth's previous row only where that row is the immediately preceding PATH wave and both waves had a parent interview.
  Filled: 2 = 7,676; 3 = 2,074; 4 = 7,153; 4.5 = 8,736; 5 = 7,963; 5.5 = 5,786; 6 = 1,765; 7 = 2,430; 7.5 = 5,716; 8 = 5,378. Family structure is Unknown on under 6% of rows at every wave.
  A re-asked relationship often differs from the last one recorded (76-80% of re-asks at waves 2 and 4, where the previous wave is in the data), which is why the fill never crosses a missing wave.
- `other_parent_relationship` (new): the relationship of the other resident parental figure (spouse or partner, `PT0002`; else other parental figure, `PM0059`, `PM0062`; all recodes), the one nearest to a biological parent where there are several (3-4% of rows have both).
  Whenever another parental figure lives in the household, this is known in 99.7% of rows at every wave.
- `mother_educ`, `father_educ` (new): the biological mother's or father's education on the five-level scale of `parent_educ_highest`, from the respondent's own education (`R_Y_PM0001`) where the respondent is that parent, else the spouse or guardian's (`R_Y_PM0118`, waves 2-4; asked about the resident spouse, else a biological parent figure), carried within participant to every wave.
  Asked separately only at waves 1-4; the Wave 7 Cohort has none.
- `parent_educ_highest` now also takes `R_P_PARSP_EDUC_V2` at wave 8.

**Open.**

- The half-wave weight files of ICPSR 37786 are not in the folder, so `survey_weight` is `NA` at waves 4.5, 5.5 and 7.5.
- `YX0106` (wave 8, hours of sleep in 24 hours) maps to `hlth_sleep`; `YX0246_NN`/`_UN` (wave 8, time sitting or reclining on a typical day) and `YX0402`-`YX0406` (wave 8, five everyday-discrimination items) have no dataschema row.


## Study summary

PATH is the Population Assessment of Tobacco and Health Study, a nationally representative longitudinal cohort study of tobacco use in the United States, funded by NIH/NIDA and the FDA Center for Tobacco Products (codebook title pages, e.g. `Original Cohort/DS1002/36498-1002-Codebook.pdf` p. 1).
The study folder holds two ICPSR releases: ICPSR 36498 "Public-Use Files" (folder `Original Cohort`, waves 1 to 7) and ICPSR 37786 "Special Collection Public-Use Files" (folder `Supplementary Cohort`, the half-waves 4.5, 5.5 and 7.5).

**The folder names are misleading: `Supplementary Cohort` is not a separate cohort.**
The 37786 files are half-wave follow-up interviews put to the same youth between main waves, which the codebook title pages name "ICPSR Codebook for Wave 4.5 / Wave 5.5 / Wave 7.5: Youth/Parent Questionnaire Data".
Two independent checks confirm it: the derived sex variable in `Supplementary Cohort/DS1002` is labelled "Wave 4.5 Youth Gender", and 16,941 `PERSONID` values occur in both folders.

The Wave 1 sample was replenished at Wave 4 (the "Wave 4 Cohort") and again at Wave 7 (the "Wave 7 Cohort"), and shadow youth sampled as children enter the youth interview when they reach 12 (Wave 7 Nonresponse report, sections 2.1, 2.3 and 2.9).
Follow-up waves are scheduled on each household's "anniversary month" from its previous interview, "approximately 1 year after" it at Wave 2 (`Original Cohort/DS2002/36498-2002-Documentation-Nonresponse_Report.pdf`, age-classification section and footnote 2).
The later reports describe the design only loosely: "annual or biennial waves" (Wave 4 report, sample-design chapter), Wave 4.5 "continuing the annual collection of data among youth" (Wave 5, 6 and 7 reports, Wave 4.5 section), and Wave 5.5 "scheduled for interviews approximately 1 year after their Wave 5 interview" (Wave 6 and 7 reports); none states a maximum gap.

Every file used here is a **Youth / Parent Questionnaire** file (the ICPSR `DS*002` series).
One row is one sampled youth; the youth answered the youth sections and one parent or guardian answered the parent sections about that youth.
The codebook marks each item with its respondent, e.g. "ASK: Parent/guardian of sampled youth" (`36498-1002-Codebook.pdf` p. 5).
Parent-reported stems begin with `P` (`PT…`, `PM…`, `PX…`, `PY…`); youth-reported stems begin with `Y` (`YX…`, `YM…`, `YC…`, …).

Youth are aged 12 to 17 at interview, except at Wave 5.5, which interviewed only Wave 4 Cohort youth aged 13 to 17, and at Wave 6, where all respondents were at least 14 (Wave 7 Nonresponse Bias Analysis Report, Table 2-1 footnote b).
The public-use files release age only as a two-level band (`R_Y_AGECAT2`); there is no exact age in years.

Data collection windows and youth interview counts, from Table 2-1 "PATH Study data and biospecimen collection summary by wave, cohort, and interview type" in `Original Cohort/DS7002/36498-7002-Documentation-Nonresponse_Report.pdf` (p. 17):

| Wave | Field period | Youth interviews reported | Rows in the tidied table |
|---|---|---|---|
| 1 | 12 Sep 2013 - 14 Dec 2014 | 13,651 | 13,651 |
| 2 | 23 Oct 2014 - 30 Oct 2015 | 12,172 | 12,172 |
| 3 | 19 Oct 2015 - 23 Oct 2016 | 11,814 | 11,814 |
| 4 | 1 Dec 2016 - 3 Jan 2018 | 11,059 (W1 cohort) + 14,793 (W4 cohort), overlapping | 14,798 |
| 4.5 | 1 Dec 2017 - 1 Dec 2018 | 8,761 (W1 cohort) + 12,918 (W4 cohort), overlapping | 13,131 |
| 5 | 1 Dec 2018 - 30 Nov 2019 | 6,760 (W1 cohort) + 11,976 (W4 cohort), overlapping | 12,098 |
| 5.5 | 3 Jul 2020 - 31 Dec 2020 | 7,129 | 7,129 |
| 6 | 1 Mar 2021 - 30 Nov 2021 | 2,377 (W1 cohort) + 5,585 (W4 cohort), overlapping | 5,652 |
| 7 | 6 Jan 2022 - 2 Apr 2023 | 899 + 3,452 + 10,650 across three overlapping cohorts | 10,834 |
| 7.5 | 2023 (ICPSR study description; not in Table 2-1) | not reported there | 8,949 |

The report's counts are given per analytic cohort ("Wave 1 Cohort", "Wave 4 Cohort", "Wave 7 Cohort") and footnote a says those cohorts overlap substantially, so only the non-overlapping waves can be compared one-to-one.
Waves 1, 2, 3 and 5.5 match the file row counts exactly; the others are consistent with the union of the overlapping cohorts.
Wave 5.5 began in person on 1 Dec 2019, was suspended on 17 Mar 2020 for COVID-19, and the released data were collected by telephone from 3 Jul 2020 (footnote e); the small Wave 6 sample reflects the age-14 restriction.

## Tidied table

**Grain: one row per youth per PATH wave interviewed.**
**Row key: `PERSONID` + `wave`** - checked, no duplicates (`check_tidied.R 67 --key PERSONID,wave`).

Design: longitudinal cohort with replenishment (Wave 4 and Wave 7 cohorts added), plus three half-wave special collections.
110,228 rows (participant-waves), 3,383 columns, 35,915 distinct `PERSONID` values, 10 waves kept.
Rows per wave: 1 = 13,651; 2 = 12,172; 3 = 11,814; 4 = 14,798; 4.5 = 13,131; 5 = 12,098; 5.5 = 7,129; 6 = 5,652; 7 = 10,834; 7.5 = 8,949.

Assembly: **no joins at all.**
The ten files are stacked with `dplyr::bind_rows()`.
Everything else the tidier does exists to make that stack line up, plus the carries below:

1. **The wave prefix is stripped from every column name** with `sub("^[RX][0-9]{2}_?", "", x)`.
   PATH names each variable `<wave token><PATH ID>`: `R01_YX0494`, `R07R_Y_SEX`, `X04_YX0494`.
   Stripping the token leaves the PATH ID, which is the study's own stable identifier for a question - codebook display note 2 says "A variable with the same PATH ID … can display differently over time", and the item that matters most here, the TV-time question `YX0494`, carries that ID in all ten files.
   The rule is applied blanket, produces no collisions, and leaves all 3,382 stems as syntactic R names (no backticks needed in `variables.csv`).
2. **`PERSONID` is trimmed.**
   It is stored padded to 20 characters up to wave 4 and to 10 characters from wave 5 on, so without `trimws()` the same youth reads as two people and 11,643 spurious extra ids appear.
3. **Factor levels and variable labels are re-encoded from latin1 to UTF-8.**
   The `.rda` files declare `codepage = 28591`; 101 columns have level strings with latin1 bytes that make `grepl()`/`sub()` error in a UTF-8 session.
4. **146 stems whose class differs between waves are coerced to `character`** before the bind.
   ICPSR writes a variable with no valid case in a wave as a numeric all-`NA` column, which will not bind against the factor the other waves carry; 238 of the 246 numeric copies involved are entirely missing, the other 8 are tobacco-quantity items in waves 1-2.
5. **Variable labels are re-attached after the bind**, because the ICPSR files keep them in a `variable.labels` attribute on the data frame that `bind_rows()` drops (and `describe_tidied.R` only reads a per-column `label` attribute).
6. **Parent education and survey design values are carried within participant** (next subsection).

Values are ICPSR factors whose level strings carry the code and the label, e.g. `(3) 3 = About 1 hour`.
That is why the wave-to-wave changes in response sets described below are visible rather than silent: a code that changes meaning changes its level string too, so `bind_rows()` unions them instead of merging them.

**Waves dropped by the screen rule: none.**
The TV/video duration item `YX0494` (a disaggregated screen item) was answered in every wave: 1 = 13,600; 2 = 12,120; 3 = 11,780; 4 = 14,740; 4.5 = 13,070; 5 = 12,056; 5.5 = 7,113; 6 = 5,635; 7 = 10,764; 7.5 = 8,873.

### Values carried or filled across waves

All fills use `carry_within()` from `R/helpers.R` on `PERSONID`, in numeric wave order (1, 2, 3, 4, 4.5, 5, 5.5, 6, 7, 7.5): a missing wave takes the participant's earliest observed value, which also fills waves before the first observation.
Observed values are never overwritten (checked: every observed value in these seven columns is unchanged, and every other column is identical to the pre-carry table).
Each carried column's label ends "; missing waves take the participant's earliest observed value".

| Column | Construct | Rule | Values filled | By wave |
|---|---|---|---|---|
| `R_Y_PM0001` | parent's education (waves 1-4) | parent education, always | 30,241 | 1: 60, 2: 836, 3: 230, 4: 91, 4.5: 11,345, 5: 8,812, 5.5: 4,538, 6: 3,092, 7: 1,180, 7.5: 57 |
| `R_Y_PM0118` | spouse/guardian's education (waves 2-4) | parent education, always | 31,655 | 1: 6,951, 2: 1,875, 3: 1,224, 4: 591, 4.5: 8,184, 5: 6,341, 5.5: 3,318, 6: 2,250, 7: 881, 7.5: 40 |
| `R_P_PARSP_EDUC` | highest of parent/spouse/guardian (waves 4.5-6) | parent education, always | 32,944 | 1: 3,587, 2: 5,258, 3: 7,017, 4: 11,444, 4.5: 100, 5: 60, 5.5: 77, 6: 37, 7: 3,512, 7.5: 1,852 |
| `R_P_PARSP_EDUC_V2` | highest of parent/spouse/guardian (waves 7-7.5) | parent education, always | 13,575 | 1: 3, 2: 2, 3: 84, 4: 1,184, 4.5: 2,326, 5: 3,443, 5.5: 3,107, 6: 3,297, 7: 37, 7.5: 92 |
| `VARSTRAT` | variance stratum | design value fixed at sampling, to every wave | 26,885 | 3: 9,769, 4: 7,473, 4.5: 5,328, 5: 3,448, 5.5: 780, 6: 84, 7: 3 |
| `VARPSU` | variance PSU | design value fixed at sampling, to every wave | 26,885 | as `VARSTRAT` |
| `Y_PWGT` | person weight | design weight, to every wave | 26,885 | as `VARSTRAT` |

`VARSTRAT`, `VARPSU` and `Y_PWGT` are released only in the Wave 1 and Wave 2 youth files; for the 10,081 youth in both, stratum and PSU are identical at the two waves, which confirms they are fixed at the Wave 1 sample.
`Y_PWGT` is two different weights under one PATH ID: "Wave 1 Final Person-level Weight" at wave 1 and "Wave 2 Youth Longitudinal Weight" at wave 2 (variable labels of `R01_Y_PWGT` and `R02_Y_PWGT`), so the carried value is the Wave 1 weight, or the Wave 2 longitudinal weight for the 2,091 youth first interviewed at wave 2.
Youth first interviewed at wave 3 or later (9,901 rows from youth first seen at wave 3, plus the Wave 4 and Wave 7 replenishment cohorts and youth first seen at later waves) have no design values in these files, so 57,520 rows stay `NA` on all three.
The 100 replicate weights (`Y_PWGT1` … `Y_PWGT100`) are not carried; they are not harmonised.

**Not carried:** household income and family structure.
The public-use files release no interview date, and the only documented interval is the Wave 2 "anniversary month", "approximately 1 year after the Wave 1 interview" with a target window from the month before to two months after it (`Original Cohort/DS2002/36498-2002-Documentation-Nonresponse_Report.pdf`, section on age classification and footnote 2), i.e. up to about 14 months.
The Wave 3 to 7 reports define the anniversary month from prior interview dates without stating its length; the later reports call the design "annual or biennial" and schedule Wave 4.5 and Wave 5.5 about a year after the previous wave (see Study summary), but none bounds the gap at 12 months, and the field periods put several adjacent waves more than 12 months apart (e.g. Wave 3 Oct 2015 - Oct 2016 against Wave 4 Dec 2016 - Jan 2018).
So no gap of 12 months or less is documented and nothing was filled (see Decisions to confirm).
Age needs no fill: `R_Y_AGECAT2` is present at every wave and missing on only 3 rows (wave 1).

## Files

**Used:** all ten declared `role: data` resources, i.e. every `DS*002` `.rda` file in both folders.

**Ignored:** none.

**Undeclared files in the study folder (27):** the per-wave Questionnaire PDFs, Informed Consent PDFs, Nonresponse Bias Analysis Report PDFs and telephone showcard PDFs.
They were read here for documentation only.
Declaring the questionnaires and nonresponse reports as `role: "docs"` would be a reasonable follow-up, but nothing in the tidier needs them and the spec was not changed.

**Spec edits:** none.
The spec read all ten files correctly as written.

## Decisions to confirm

- **Stripping the wave prefix and treating one PATH ID as one column.**
  This is the whole shape of the table.
  It is right for the screen-time and demographic items (verified item by item below), but 952 of the 2,524 stems present in more than one file have at least two different label texts across waves.
  Most are rewordings of the same question; a minority of multi-select indicator suffixes genuinely point at different response options in different waves (for example `YX0307E_10` is "switching … to bidis" at one wave and "… to other smokeless tobacco" at another, and `YG1011FC_04` is a different cigar flavour at different waves).
  All of those are tobacco items this project does not use, but a mapper who reaches for a `_NN`-suffixed multi-select stem should check the wave-specific codebook first.
- **Each column's label is taken from the earliest wave in which the stem appears.**
  So `YX0494` is labelled "Hours spent watching TV on a typical day" (its Wave 1 wording) even though seven of the ten waves ask "Amount of time spent watching TV and streaming videos that include commercials on an average weekday".
  **Do not map a screen-time item from its label alone** - use the per-wave table in "Pointers for harmonisation".
- **146 stems were coerced to `character`.**
  They lose their factor ordering; the level text is preserved.
- **No Wave 6.5 file is missing.**
  The spec note flags that `Supplementary Cohort` has DS1002, DS2002 and DS4002 but no DS3002.
  The third Special Collection is PATH-ATS, the Adult Telephone Survey conducted 10 Sep - 20 Dec 2020, which interviewed adults only (Wave 7 Nonresponse report Table 2-1, "PATH-ATS … Adults 8,874"), so there is no youth file for it.
  Nothing is absent from the folder.
- **`CASEID` is file-local.** It runs 1..N within each wave's file, is not a person id, and must not be used for linkage.
  `participant_id` should be built from `PERSONID`.
- **Household income and family structure are not carried across waves.**
  No interview dates are released and no wave interval of 12 months or less is documented (see "Values carried or filled across waves"), so the tidier leaves them missing and the mapping emits Unknown.
  The main effect is wave 1, which has no income item: if the project accepts the Wave 2 "approximately 1 year" anniversary design as a 12-month interval, carrying `R_Y_PM0130` from wave 2 would fill 8,933 wave-1 rows.
  The later reports also call the design "annual" and put Wave 4.5 and Wave 5.5 about a year after the previous wave, so the same question applies to family structure and to income gaps at later waves.
- **The survey design carry puts the Wave 1 weight (or the Wave 2 longitudinal weight) on later waves.**
  The public-use youth files release no weight, stratum or PSU after wave 2, and youth first interviewed at wave 3 or later (including the Wave 4 and Wave 7 cohorts) have none at all, so `survey_weight` would be a carried, partial value for 26,885 rows and missing for 57,520.
  Confirm this is wanted rather than leaving the design rows unavailable after wave 2; the per-wave weights are separate ICPSR weight files that are not in the study folder.
- **Parent education is carried in four separate columns**, each now filled at every wave where the participant ever had a value (including waves before the first observation).
  They use different codings (5 and 6 levels) and different respondents (one parent at wave 1; parent and spouse at waves 2-4; highest across parent, spouse and guardian from wave 4.5), so the tidier did not merge them.
  The mapping needs one precedence among them; see "Pointers for harmonisation".
  Wave 1 asked about one parent only, but the carry now puts a later wave's spouse/guardian education (`R_Y_PM0118`) on 6,951 wave-1 rows; confirm whether those rows take the two-parent maximum or the dataschema's "only one parent was asked about: Unknown" rule.
- **`YX0494` at waves 1-2 ("watching TV") and from wave 4 ("TV and streaming videos that include commercials") is narrower than `st_watch`**, so those waves look partial; confirm when mapping.

## Rows dropped

None.
No filter of any kind is applied; every row of every declared file reaches the tidied table.

- Screen rule: no wave dropped, because `YX0494` (TV/video hours) was answered in all ten waves.
- Attendance: the PATH youth files hold completed youth interviews only, and there is no participation flag or roster of non-participants; the wave 1, 2, 3 and 5.5 row counts equal the youth interview counts in Table 2-1 of the Wave 7 Nonresponse report, and no row is missing on every non-derived item (0 rows), so every row is a wave the youth attended.
- No age filter: the ages in `R_Y_AGECAT2` are all 12-17 bands, and nothing was dropped on age.

## Done in the tidier that is arguably harmonisation

- `trimws()` on `PERSONID` (structural: without it the longitudinal id does not link across waves).
- `iconv()` from latin1 to UTF-8 on factor levels and variable labels.
- `as.character()` on the 146 class-clashing stems listed above.
- Construction of the `wave` column ("1", "2", "3", "4", "4.5", "5", "5.5", "6", "7", "7.5") from the resource name, per the codebook title pages.
- Carrying parent education (`R_Y_PM0001`, `R_Y_PM0118`, `R_P_PARSP_EDUC`, `R_P_PARSP_EDUC_V2`) and survey design values (`VARSTRAT`, `VARPSU`, `Y_PWGT`) within participant, as the harmonisation conventions assign to the tidier; counts in "Values carried or filled across waves".
- The derived column `parent_educ_highest` (added 2026-10-01 so the `parental_education` expression is a plain lookup): the highest parent education on one five-level scale shared by the four codings, taking PATH's own highest-parent variable at waves 4.5-7.5, the higher of `R_Y_PM0001` and `R_Y_PM0118` at waves 2-4, and at wave 1 the parent's own value where no spouse or partner lives in the household (`PT0045`), otherwise the highest including carried values, else NA (3,064 wave-1 rows).
- The derived column `adhd_told_before` (added 2026-10-01 for `beh_adhd_diagnosed`): TRUE if a parent reported an ADHD/ADD diagnosis (`R_Y_PY0052`, `PT0052_NB` or `PT0052_12M`) at any earlier wave, FALSE if the youth's first interview recorded never told and no later wave reported it, NA at the first interview.
  Waves 2-7.5 ask every parent only about the past 12 months and ask ever only of new respondents, so this history is what separates previously from never diagnosed.

Nothing else beyond `parent_educ_highest` and `adhd_told_before`. **No scale was scored, no category recoded, no sentinel code zapped.**
Sentinel codes need no zapping here: ICPSR's R conversion already turns the PATH missing codes (-1, -5, -6, -7, -8, -9 and the derived -999xx family, listed in `36498-1002-Codebook.pdf` pp. 4-5) into `NA`.
Only three columns still carry negative values, and in all three the negative code is meaningful rather than missing: `R_Y_YX0074` and `R_Y_YX0074_NB` use `-2 = I have never had an alcoholic drink…`, and `YV9040` uses `-2` in the same way.

## Pointers for harmonisation

`wave` values are `"1" "2" "3" "4" "4.5" "5" "5.5" "6" "7" "7.5"`; map `wave` from the tidied `wave` column.
`data_year` has no per-row date in the public-use files, so it is a per-wave constant from the field-period table above (inferred); most waves straddle two calendar years, so state the rule used (e.g. the year holding most of the field period).
`country` is a constant, United States.

### Identity and design

| Target | Source column | Notes |
|---|---|---|
| `participant_id` | `PERSONID` | character, already trimmed, unique within a wave, stable across waves |
| `wave` | `wave` | built by the tidier |
| - | `CASEID` | file-local sequence, do **not** use |
| `survey_strata` | `VARSTRAT` | "Stratum Indicator for Variance Estimation"; released at waves 1-2, carried by the tidier to later waves of the same youth; `NA` for youth first interviewed at wave 3 or later, including the Wave 4 and Wave 7 cohorts |
| `survey_cluster` | `VARPSU` | "PSU Indicator for Variance Estimation"; same coverage as `VARSTRAT` |
| `survey_weight` | `Y_PWGT` | wave 1 "Wave 1 Final Person-level Weight" (cross-sectional), wave 2 "Wave 2 Youth Longitudinal Weight"; carried to later waves, so later waves are a carried design weight (partial) and the wave 2 values are a longitudinal weight (say which in notes) |
| - | `Y_PWGT1` … `Y_PWGT100` | replicate weights, waves 1-2 only; not harmonised |

### Demographics and SES

Demographic and SES rows emit "Unknown", never `NA`, wherever nothing is observed or carried.
Sex, ethnicity, born-in-country and immigrant background are carried in the mapping with `carry_within()`/`modal_within()`, not in the tidier.

| Target | Source column | Waves | Response options |
|---|---|---|---|
| `age_years` | `R_Y_AGECAT2` | all 10 | `1 = 12 to 14 years old`, `2 = 15 to 17 years old`; completed-year bands take (a + b + 1) / 2, i.e. 13.5 and 16.5, and are partial (multi-year bands) - **band only, no exact age in the public-use files** |
| | `R_Y_AGECAT2_IMP` | 1 | imputed version, with `R_Y_AGECAT2_IMPFLAG` |
| `sex` | `R_Y_SEX` | all 10 | `1 = Male`, `2 = Female`; label reads "Gender from the interview"; modal value within participant |
| | `R_Y_SEX_IMP` | 1, 4, 7 | imputed version |
| `ethnicity` | `R_Y_RACECAT3` | all 10 | `1 = White alone`, `2 = Black alone`, `3 = Other` |
| `ethnicity` / `minority` | `R_Y_HISP` | all 10 | `1 = Hispanic`, `2 = Not Hispanic`; `R_Y_HISP_IMP` at waves 1, 4, 7; the Hispanic-origin item sets Hispanic in this US sample |
| `born_in_country` | `R_Y_YM0065_V2` | 4 onwards | `1 = Lived in the U.S. entire life`, `2 = Not lived in the U.S. entire life` (a residence item standing in for birthplace) |
| | `R_Y_YM0065` | 3 | years youth lived in the US, 3 bands (682 answers) |
| | `R_Y_YM0069` | 3, 4 | youth is a US citizen - citizenship, not birthplace |
| `immigrant_background` | `R_Y_PM0065_V2` + youth item above | 4 onwards | parent respondent `1 = Lived in the U.S. entire life`, `2 = Not …`; only one parent is recorded, so "one parent's birthplace unknown: the other decides"; `R_Y_PM0065` (wave 3) and `R_Y_PM0069` (citizenship, 3 onwards) also exist |
| `ses_household` | `R_Y_PM0130` | 2 onwards | 5 bands, `Less than $10,000` … `$100,000 or more` (parent-reported household income, past 12 months); banded source, so classify by ridit within wave (Household SES cuts); wave 1 has no income item and is Unknown (not carried; see Decisions to confirm) |
| | `PM0031` | 2 onwards | `1 = Above $50,000`, `2 = Below $50,000`; asked only of parents who would not give a band, so it is 98% missing |
| `parental_education` | `R_Y_PM0001` | observed 1-4, carried to all | parent's education; **two codings share this column**: wave 1 uses 5 levels (`2 = High school graduate or equivalent`), waves 2-4 use 6 levels (`2 = GED`, `3 = High school graduate`), distinguishable by level string |
| | `R_Y_PM0118` | observed 2-4, carried to all | same 6-level coding, for the spouse/guardian |
| | `R_P_PARSP_EDUC` | observed 4.5, 5, 5.5, 6, carried to all | 6 levels, the study's own highest across parent/spouse/guardian |
| | `R_P_PARSP_EDUC_V2` | observed 7, 7.5, carried to all | 5 levels, GED folded into "High school graduate or GED" - PATH's own `_V2` rename, kept as a separate column |
| `family_structure` | `R_Y_PT0001` (1-4), `R_Y_PT0001_V2` (4.5 onwards), `R_Y_PT0002` (1-4), `R_Y_PT0002_V2` (4.5 onwards), `PT0045` (1), `R_P_OTHPAR_INHH` (2 onwards), `PM0060` (7, 7.5), `R_Y_PT0047` (all), `PT0003` (all) | various | respondent's and spouse/partner's relationship to the youth (`_V2`: `Biological mother`, `Biological father`, `Other`), spouse or partner in household, other parental figure in household, marital status, other parent living elsewhere; not carried across waves |
| (no direct target) | `X_CB_REGION` | 1, 4, 7 only | Census region: `1 = Northeast`, `2 = Midwest`, `3 = South`, `4 = West`. Not urbanicity and not a US state; the public-use files release neither |
| `school_grade` | `R_Y_YM0018` | 1-4 | 7 levels, `1 = 6th grade or below` … `7 = Other` |
| | `R_Y_YM0018_V2` | 4.5 onwards | **two codings share this column**: `1 = 7th grade or below` at waves 4.5-6 and `1 = 8th grade or below` at waves 7-7.5, because the cohort ages. Distinguishable by level string |
| (covariate) | `R_Y_BMI` | all 10 | numeric |

For `parental_education`, all three parent-education rows call the shared education lookup, and the dataschema asks for the study's own highest-parent variable where one exists (`R_P_PARSP_EDUC`, `R_P_PARSP_EDUC_V2`), otherwise the maximum of `R_Y_PM0001` and `R_Y_PM0118`.
Because the tidier has filled all four columns at every wave, a fixed coalesce order will put a carried value from one column ahead of a value observed in another at the same wave; pick the precedence deliberately and record it.

### Screen-time items (all youth self-report, questionnaire, categorical bands)

PATH has **no gaming item and no computer/tablet-device item**; the screen-time content is television/streaming and social media only.
All screen items are youth self-report questionnaire items, so they are all the primary measure: there is no second responder or instrument and no tagged measure block.
TV and social media are different `st_*` rows of the same primary measure, never separate measures.

**`YX0494` - television and video, weekday or typical day. Present in all ten waves, but the reference period and the bands change:**

| Waves | Question wording | Response options |
|---|---|---|
| 1, 2 | Hours spent watching TV on a typical day | None / Less than 1 hour / 1 to 2 hours / 3 to 4 hours / More than 4 hours |
| 3 | Hours spent watching TV or videos on a television, computer, tablet or smartphone on a typical day | None / Less than 1 hour / 1 to 3 hours / More than 3 hours |
| 4, 4.5, 5, 5.5, 6, 7, 7.5 | Amount of time spent watching TV **and streaming videos that include commercials** on an average **weekday** | None / Half hour or less / About 1 hour / About 2 hours / About 3 hours / About 4 hours / About 5 hours / About 6 hours / 7 hours or more |

Waves 1-3 are a usual day (the unsuffixed `st_watch` only; `_wd`/`_we` unavailable for those waves), waves 4 onwards are an average weekday (`st_watch_wd`), so this one column feeds two rows conditional on `wave`.
Band values under the screen-time rules: None = 0, Less than 1 hour = 0.5, Half hour or less = 0.25, closed bands at their midpoint (1 to 2 = 1.5, 3 to 4 = 3.5, 1 to 3 = 2), "About N hours" = N, and open top bands at their stated lower bound (More than 4 hours = 4, More than 3 hours = 3, 7 hours or more = 7); write the top-band values into the notes.
Waves 1-2 ask about "TV" only (an item limited to the TV set is partial on `st_watch`); from wave 4 the "that include commercials" qualifier narrows the construct, so those rows are partial too.
Non-missing counts by wave: 1 = 13,600; 2 = 12,120; 3 = 11,780; 4 = 14,740; 4.5 = 13,070; 5 = 12,056; 5.5 = 7,113; 6 = 5,635; 7 = 10,764; 7.5 = 8,873.

**`YX0474` - television and streaming video, average weekend day.** Waves 4, 4.5, 5, 5.5, 6, 7, 7.5 (not waves 1-3).
Options: None / Less than 1 hour / 1 to 2 hours / 3 to 4 hours / 5 to 6 hours / 7 to 8 hours / 9 to 10 hours / 11 hours or more.
Candidate for `st_watch_we` (0, 0.5, 1.5, 3.5, 5.5, 7.5, 9.5, 11); the weekend bands are wider than the weekday bands, so each item uses its own bands.
From wave 4, `st_watch` = (5 × `st_watch_wd` + 2 × `st_watch_we`) / 7 where both are mapped for the wave.

**`YX0370` - social media, average weekday.** Waves 6, 7, 7.5 only.
Same 9 bands as weekday `YX0494`; candidate for `st_socmedia_wd`.

**`YX0371` - social media, average weekend day.** Waves 6, 7, 7.5 only.
Same 8 bands as `YX0474`; candidate for `st_socmedia_we`; `st_socmedia` = (5 × wd + 2 × we) / 7 at those waves.

**`YX0320` - "How much time spent on social media sites on a typical day". Wave 2 only.**
Options: Up to 30 minutes / More than 30 minutes, up to 3 hours / More than 3 hours, up to 6 hours / More than 6 hours (0.25, 1.75, 4.5 and 6 under the band rules).
Candidate for `st_socmedia`; there is **no social-media duration item at waves 1, 3, 4, 4.5, 5 or 5.5**.
`YX0062` "Has a social media account" (all waves, yes/no) can set non-users to 0 under the **Non-users** rule only where the hours item was routed past or left unanswered because the youth has no account; check the questionnaire routing first, and a reported duration always wins.

**No `st_total` item.**
There is no item on all screen use, so `st_total*` rows are unavailable; never build a total by adding TV and social media (DR-0004, DR-0002).

**Frequency or ownership items, which give no hours (incompatible on `st_*` rows, item named):**
`YX0486` "How often do you use the internet" (wave 1, 7 options ending `Don't have regular internet access`);
`YX0495` "Use a smart phone regularly" (wave 1, yes/no);
`YX0315` "Ever go online to access the Internet" (waves 2-3, yes/no);
`YX0062` "Has a social media account" (all ten waves, yes/no);
`YX0317` "How often you visit your social media accounts" (waves 2 to 5.5) - **the codes shift at wave 5.5**, which inserts `1 = Almost constantly` and pushes every other option down one; the level strings differ, so the data distinguishes them.
`YX0060` / `YX0061` are the radio equivalents of `YX0494` / `YX0474` and are **not** screen time.

`st_measure_type` is `Categorical` (banded self-report questionnaire items), `st_measure_name` "Custom", and `st_responder` the youth.

### Outcome instruments

**The "PSYCHOSOCIAL" problem screener, youth self-report, all ten waves.**
Every item asks "When was the last time that you had significant problems with …" with options `1 = Past month`, `2 = 2 to 12 months ago`, `3 = Over a year ago`, `4 = Never`.
The questionnaire introduces the block under the heading PSYCHOSOCIAL with: "The following questions are about common psychological, behavioral, and personal problems. These problems are considered significant when you have them for two or more weeks, when they keep coming back, when they keep you from meeting your responsibilities, or when they make you feel like you can't go on." (`36498-1002-Questionnaire.pdf` p. 214; the questionnaire numbers that text `R01_YX0488`, which is not released in the data file).

**Instrument: the GAIN-SS** (Global Appraisal of Individual Needs - Short Screener; Dennis et al. 2006, doi:10.1080/10550490601006055).
Neither the codebook nor the questionnaire names it, but Riehm et al. 2019 (JAMA Psychiatry, doi:10.1001/jamapsychiatry.2019.2325) identify these PATH items as the GAIN-SS and score them as past-year symptom counts (past month or 2-12 months ago).
The externalising set also includes `YX0250` (felt restless or the need to run around) and `YX0251` (gave answers before the question was finished), asked at every wave, so it has 7 items; the internalising set has 4 of the 5 items of the 20-item GAIN-SS (no suicidal-thoughts item).
The substance-problem items (`YX0170`-`YX0174`, `YX0193`, `YX0194`) are the GAIN-SS substance screener and are not mapped.

| Stem | Item | Subscale |
|---|---|---|
| `YX0161` | Feeling very trapped, lonely, sad, blue, depressed or hopeless about the future | internalising |
| `YX0162` | Sleep trouble - bad dreams, sleeping restlessly, falling asleep during the day | internalising |
| `YX0163` | Feeling very anxious, nervous, tense, scared, panicked | internalising |
| `YX0164` | Becoming very distressed and upset when something reminded you of the past | internalising |
| `YX0165` | Lied or conned to get things you wanted or to avoid something | externalising |
| `YX0166` | Had a hard time paying attention at school, work or home | externalising |
| `YX0167` | Had a hard time listening to instructions at school, work or home | externalising |
| `YX0168` | Were a bully or threatened other people | externalising |
| `YX0169` | Started physical fights with other people | externalising |
| `YX0172` | Kept using alcohol or drugs despite social problems or fights | substance |

These are recency ratings ("last time"), not frequency or truth ratings.
`YX0168` (bullying others) and `YX0169` (starting fights) can be binary behaviour rows (TRUE for any occurrence in the recall window chosen, e.g. past month or past 12 months; record it); there is no *being bullied* item.
`YX0161` bundles sadness, loneliness and hopelessness and `YX0163` is "feeling anxious", so under the frequency-item rules `mh_sad_self`, `mh_worry_self` and `mh_lonely_scale_self` are incompatible (items named); `YX0162` is a single sleep item, so `hlth_sleepprob` is incompatible (single items are); `YX0166`/`YX0167` are two items, not an attention scale, for `beh_atten_self`/`beh_atten_measure`.

**Other outcome-relevant columns:**

- `YX0091` - "Self perception of mental health, including stress, depression, and problems with emotions", waves 2 onwards, `Excellent / Very good / Good / Fair / Poor`; `YX0091_12M` is the compared-to-12-months-ago version (waves 3 onwards).
- `YX0088` - "Self perception of overall health", waves 2 onwards, `Excellent` … `Poor` (self-rated health).
- `wb_generalhealth_parent`: `PT0035` "Youth's overall health status" (parent, waves 2 onwards, `Excellent / Very good / Good / Fair / Poor`) and `R_Y_PY0035` (wave 1, "as reported by parent or emancipated youth").
- Perceived stress: `YX0325`-`YX0328` (waves 6, 7, 7.5), four past-30-day items whose wording matches the PSS-4 (instrument name not checked in the codebook), `1 = Never` … `5 = Very often`; `YX0326` and `YX0327` are positively worded.
- Social support: `YX0329`, `YX0331`, `YX0333`, `YX0334` (wave 6 only), "How often support is available to you if you need it", `None of the time` … `All of the time`; the items name no source (family or friends), so the source-specific `wb_socsupport_*_self` rows are incompatible (items named).
- `beh_alcohol_self`: `YX0673` "In past 30 days, used alcohol" (waves 2 onwards, yes/no; 22,410 answers against 85,239 for `YX0084_12M`, so it looks routed on past-12-month use - check the questionnaire before using `YX0084_12M = No` as non-use), `YX0084_12M` past 12 months, `YX0075` days drank in past 30 days (all waves, routed); wave 1 has only `YX0084` ever (lifetime) and `YX0075`.
  Keep one recall window across waves: yes/no prevalence rows never splice in a different recall window, so the lifetime `YX0084` cannot fill wave 1 of a past-30-day or past-12-month series.
- `beh_cannabis_self`: `YX0675` past 30 days (waves 2 onwards, routed like `YX0673`), `YX0085_12M` past 12 months; wave 1 has only `YX0085` ever (lifetime), which cannot be spliced into a shorter-window series, so wave 1 stays missing unless the whole row uses lifetime use (partial).
- `beh_tobacco_self`: `YC1112` "Last time smoked a cigarette, even one or two puffs" (all waves; past 30 days = codes 1-3), and PATH's derived per-wave `R_Y_CUR_CIGS` "Current Cigarette User" (all waves; check the codebook definition of "current" before using it); cigars are in `R_Y_CUR_CIGAR`, `R_Y_CUR_GTRAD`, `R_Y_CUR_GRILLO`.
- `PT0019` - parent-reported "In past 12 months, youth's grade performance in school", waves 2 onwards, ten levels `Mostly A's` … `Mostly F's` plus `Your child's school is ungraded`; `R_Y_PY0019` is the wave 1 equivalent ("as reported by parent or emancipated youth").
- `R_Y_PY0030` / `PT0030` - school missed through illness, so not truancy (`beh_truancy` has no item).
- `PX0756*`, `PX0761*` - parent-reported lifetime / past-12-month professional diagnosis of schizophrenia or a psychotic illness, waves 4 onwards. Low prevalence; not a general mental-health scale.
- COVID-period items at waves 5.5-7.5 (`YM1934` "Rating of your experience of stress related to the coronavirus pandemic", `PM1934` the parent's rating of the child's) are pandemic-specific.
- Not found in these files: SDQ, CBCL, KIDSCREEN, WEMWBS, cognitive or standardised achievement tests, life satisfaction or happiness ratings, domain satisfaction, positive affect or eudaimonic scales, loneliness scales, self-harm, psychosomatic or somatic checklists, sleep duration or sleep-problem scales, parenting stress, screen rules or screens in the bedroom (`PT0009`/`PT0011` are curfews), messaging or content-creation time, problematic use, online prosocial behaviour, school pressure or belonging, stealing, vandalism or truancy.

### Missing values, label quirks and unclear columns

- The PATH missing codes are already `NA` (see "arguably harmonisation" above); do not write `na_if()` rules for `-9`/`-8`/`-7`/`-1`.
- Values are factors whose level string is `(<code>) <code> = <label>`, e.g. `(3) 3 = About 1 hour`.
  A recode must match the whole level string, or pull the code out with something like `readr::parse_number(as.character(x))` - but note that where a code changes meaning across waves (`R_Y_PM0001`, `R_Y_YM0018_V2`, `YX0317`) the code alone is ambiguous and the level string is what disambiguates.
- 16 columns are `NA` in every row (`YD1097`, `YD1098`, `YD1032`, `YD1033`, `YB1098KK`, `YR0125_NB`, `YR0130_NB`, `YR0230_NB`, `YN0348H`, `YQ1002_30D` and six more).
  Each is also empty in its source file - these are variables ICPSR released with no valid case, not a stranded rename.
- 858 of the 3,382 stems appear in only one wave, which is normal for a study that revised its questionnaire at every wave.
- Stems ending `_V2` (`R_P_PARSP_EDUC_V2`, `R_Y_YM0018_V2`, `R_Y_PM0065_V2`, `R_Y_YM0065_V2`, and others) are PATH's own re-derivations with a different level set.
  They are deliberately kept as columns separate from the un-suffixed originals; coalescing them is a harmonisation decision.
- Stems ending `_NB` ("new base") and `_12M` are PATH's re-asked variants of an item; likewise kept separate.
- `PARENT_PERSONID`, `R_P_SPOUSE_PERSONID`, `PM0058_PERSONID`, `PM0061_PERSONID` are ids of the adults linked to the youth, not of the youth. They repeat across rows (several youth in one household) and are not a row key.

## Open questions / spec problems

1. **Nothing blocked the build** - the spec read all ten files as written and needed no edit.
2. **Should the questionnaires and nonresponse reports be declared as `role: "docs"`?**
   27 PDFs sit undeclared in the study folder, including the Nonresponse Bias Analysis reports that are the authoritative source for field periods and sample sizes.
3. **Is the half-wave data wanted at all?**
   Waves 4.5, 5.5 and 7.5 add 29,209 rows (26% of the table) to the same participants at intermediate time points.
   If the analysis wants one observation per participant per year, or only the main waves, that is a filter the mapping or the analysis stage should apply - the tidier keeps everything.
4. **`YX0494` spans two reference periods** (typical day at waves 1-3, weekday at waves 4 onwards) under one column, because it is one PATH ID.
   Confirm that the mapping splits it by `wave` into `st_watch` and `st_watch_wd` rather than treating it as one construct.
5. **The psychosocial screener's instrument is not named in the study documentation.**
   Resolved 2026-10-01: Riehm et al. 2019 (doi:10.1001/jamapsychiatry.2019.2325) identify it as the GAIN-SS; see "Outcome instruments".
6. **Age is only a two-level band.** If exact age is needed, it exists only in the PATH restricted-use files (ICPSR 36231 / 37519), which are not in this folder.
7. The tidied table is 3,383 columns and 1.6 Gb in memory (33 Mb on disk under the store's `qs` format).
   Every source column was kept, per the tidier convention; if that turns out to be a burden the column set can be cut later without changing the table's shape.
8. **Weights, strata and PSUs after wave 2 are not in the folder.**
   The public-use youth files for waves 3 onwards carry no design variables; ICPSR releases PATH weights as separate files, and adding them would be a spec change (new resources) and a join in the tidier.

## Convention review (2026-09-25)

- Tidier: parent education (`R_Y_PM0001`, `R_Y_PM0118`, `R_P_PARSP_EDUC`, `R_P_PARSP_EDUC_V2`) carried with `carry_within()`, 108,415 values filled; labels note the rule.
- Tidier: `VARSTRAT`, `VARPSU` and `Y_PWGT` (waves 1-2 only) carried to every later wave of the same youth, 26,885 values each; rows, columns and every other column unchanged.
- Tidier: header cut to two summary lines and helper docs to one-line titles; no wave dropped (TV hours asked at every wave) and no row dropped (files hold completed interviews only).
- Not carried: household income and family structure, because no interval of 12 months or less is documented and no interview dates are released (decision listed).
- Notes: removed the advice to tag TV and social media as separate measures, added band values, the DR-0002/DR-0004 total rule, the age-band rule and Unknown on demographic rows.
- Notes: added pointers for survey design, born-in-country, immigrant background, parent-rated health, substance use, perceived stress and social support, and listed the new dataschema constructs PATH does not measure.

## Verification (2026-09-25)

- Recomputed from a fresh build and the pre-revision tidier: 110,228 rows, 35,915 youth, rows per wave, every fill count and the 57,520 rows left without design values all match; no observed value was overwritten and every other column is identical.
- Corrected: `R_Y_PM0001` uses 5 levels at wave 1 only and 6 levels at waves 2-4 (the notes said waves 1 and 4 used 5).
- Corrected: the later nonresponse reports do describe the interval loosely ("annual or biennial", Wave 4.5 "annual", Wave 5.5 "approximately 1 year after their Wave 5 interview"); none bounds it at 12 months, so the decision not to carry income and family structure stands as a decision to confirm.
- Corrected: the rows with no design values are those of youth first interviewed at wave 3 or later, not only the Wave 4 and Wave 7 cohorts.
- Corrected: Wave 5.5 interviewed only Wave 4 Cohort youth aged 13-17; `st_measure_type` is `Categorical`; the wave-1 lifetime alcohol and cannabis items cannot be spliced into a shorter-window series.
- Added a decision on wave-1 parent education, where the carry fills spouse/guardian education for 6,951 rows.
