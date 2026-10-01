# BPIPD-2207 (Korean Panel Survey) - tidier notes

## Study summary

The dataset is the **Korea Media Panel Survey** (한국미디어패널조사, KMPS), run annually by KISDI (정보통신정책연구원 / Korea Information Society Development Institute).
Everything in this section comes from the one document in the study folder, `2025 한국미디어패널조사_유저가이드.pdf` (the study's own user guide, dated 2025.12), unless another source is named.

Design: an annual household panel with a household questionnaire, an individual questionnaire for every household member aged 6 and over, and a three-day media diary recorded in 15-minute slots as part of the individual questionnaire (guide p.6 §1-2, p.16 §미디어 다이어리).
Country: South Korea.
Official statistic no. 405001, approved 15 September 2010; the statistic is produced every year ("통계작성 주기: 1년", guide p.6 §1).

Sample and panel history (guide p.6 §1):

- 2010: 3,085 households in the Seoul capital region plus the six metropolitan cities (Incheon, Daejeon, Daegu, Gwangju, Ulsan, Busan), and their members aged 6+.
- 2011: rebuilt nationwide across all 16 metropolitan cities and provinces including Jeju, 5,109 households.
- 2012 onwards: follow-up of the panel built in 2011 (`KMPS11`).
- 2019: a refresher panel (`KMPS19`) of 1,027 households was added because the original panel had aged; from 2019 the combined panel is 4,537 households (3,510 surviving `KMPS11` households plus the 1,027 new ones).

Sampling (guide pp.20-21, §2 표본 설계): the frame is the 2005 census enumeration districts; the design is stratified two-stage sampling, with strata formed by the 16 metropolitan cities/provinces crossed with urban dong versus rural eup/myeon, enumeration districts (조사구) as primary sampling units drawn with probability proportional to size, and about 10 households per district.
`KMPS19` was designed the same way with 100 districts.

Weights (guide pp.22-27, Table 2-3): household and person weights are recomputed every year (cross-sectional `wt`, longitudinal `wt2`, and from 2019 the original-panel versions `wt_org`/`wt_org2`); no longitudinal weight exists for 2011 or for the integrated panel's first year 2019, and the person and diary weights are identical.

Fieldwork timing: the guide gives no fieldwork dates, but the diary records the month and day of each diary day (`d_mm1st_dk`, `d_dd1st_dk`); diary day 1 falls in October-November in 2010 and in May-October (mostly June-August) from 2011 to 2024.

Respondents: each household member aged 6+ answers for themselves, including the diary; the household questionnaire is answered once per household.
Age range in the data: 6 to 108 years (a handful of out-of-scope rows are noted under "Decisions to confirm").

**Waves are calendar years, not wave ordinals.**
The spec's waves `10` ... `24` are the two-digit survey *years* 2010 ... 2024.
Guide Table 3-1 (p.29) lists the released files as `h10v32_KMP`/`p10v32_KMP`/`d10v32_KMP` for 조사년도 2010 through `..24..` for 2024, and the household row counts in the data match the guide's stated household samples exactly (3,085 households in wave `10`, 5,109 in wave `11`).
The guide also documents a 2025 release; it is not in the study folder, and the folder's files are release `v31` where the guide describes `v32`.

## Tidied table

Design: longitudinal (household panel with a refresher panel in 2019).
Grain: **one row per respondent per survey year.**
Row key: `pid` + `.wave` (asserted in the tidier; `check_tidied.R --key pid,.wave` confirms it is unique).
`pid` is the study's own longitudinal person identifier and is stable across years: 5,654 `pid`s appear in both the 2011 and the 2024 person files and the birth year agrees for all of them.

Size: **147,915 observations (respondent-years) from 19,840 distinct participants (`pid`), 4,432 columns, 15 waves kept.**

Rows per wave, which equal the person-file row counts exactly (no row was gained or lost):

| wave | year | rows | aged 6-17 |
|---|---|---|---|
| 10 | 2010 | 6,737 | 1,282 |
| 11 | 2011 | 12,000 | 2,071 |
| 12 | 2012 | 10,319 | 1,605 |
| 13 | 2013 | 10,464 | 1,550 |
| 14 | 2014 | 10,172 | 1,367 |
| 15 | 2015 | 9,873 | 1,301 |
| 16 | 2016 | 9,788 | 1,228 |
| 17 | 2017 | 9,425 | 1,075 |
| 18 | 2018 | 9,426 | 988 |
| 19 | 2019 | 10,864 | 1,141 |
| 20 | 2020 | 10,302 | 996 |
| 21 | 2021 | 10,154 | 908 |
| 22 | 2022 | 9,941 | 750 |
| 23 | 2023 | 9,757 | 656 |
| 24 | 2024 | 8,693 | 354 |

17,272 rows are aged 6-17 and 20,960 are aged 6-19; all ages are kept, since eligibility is applied at analysis.
The child count falls steadily because the panel ages and is only partly refreshed, which matches the guide's own explanation for building `KMPS19` (p.6 §1).

How it was assembled, per year:

1. The **person file** is the spine, one row per respondent.
2. The **household file** is joined on `hid`, `relationship = "many-to-one"`, so household columns broadcast to every respondent in the household.
3. The **diary file** (three rows per respondent, one per diary day) is reshaped to one row per respondent and joined on `pid`, `relationship = "one-to-one"`.
4. The fifteen years are `bind_rows()`ed.
5. Household income and family type are carried across gaps of 12 months or less (below).

**Screen rule: no wave dropped.**
The media diary, which records minutes per medium (device-level codes such as the smartphone, code 19), was fielded in every year 2010-2024, so every wave has a disaggregated screen item.

**Attendance: no rows dropped.**
Each year's person file holds only that year's respondents (`p<yy>ans == 1` on every row of year `<yy>`), and every respondent has a complete three-day diary.

**Values carried across waves** (in the tidier, never overwriting an observed value; the rule is appended to each column's label):

| column | rule | values filled |
|---|---|---|
| `h_h_income1` | household SES: nearest year for the same `pid` interviewed 12 months or less away | 83 |
| `h_h_income2` | household SES: same rule | 83 |
| `h_fly_typ` | family structure: same rule | 83 |

The only missing values in these columns are the 727 respondent-years whose `hid` has no row in that year's household file; 83 of them had a household record for the same `pid` within 12 months and were filled, and 644 stay missing.
The interview month is year (2000 + `.wave`) times 12 plus the month of diary day 1 (`d_mm1st_d1`); the 12 rows in 2010 with `d_mm1st_d1 == 888` have no date and are never filled.
Gaps are counted in whole months, so a 12-month gap is filled (inclusive); sentinel codes (`9999`) in these columns are observed values and are not replaced.

Not carried, and why:

- Parent education: no parent-education column exists (see Pointers).
- Age: `p_age1` is recorded for every row in every year.
- Survey design: the weights are recomputed each year rather than fixed at sampling, and no stratum or cluster column is released.
- Sex and the other fixed characteristics are carried in the mapping, not here.

**Column renaming.** Every source column name carries the two-digit survey year (`p24gender`, `h24h_income1`, `d24M_time19`), so nothing would line up across years without renaming.
Each file's own columns are renamed to a `p_`/`h_`/`d_` prefix: `p24gender` becomes `p_gender` in every year.
This is safe because the guide states the naming is stable: "데이터의 변수명은 연차별 조사항목에서 동일하게 유지됨" - variable names are kept identical across years for the same survey item (p.28).
Every one of the 45 files' columns matched either the `<prefix><year>` rule or the linkage block below, with no leftovers.

**The linkage block is left alone.** `pid`, `hid`, `housenum`, `p10pid`..`p24pid`, `h10hid`..`h24hid`, `p10ans`..`p24ans`, `h10ans`..`h24ans`, `d10ans`..`d24ans` and `KMPS10`/`KMPS11`/`KMPS19` already carry the same names in every file, so stripping the year would have collapsed fifteen different columns onto one name.
`p<yy>pid` and `h<yy>hid` are that year's person/household id, `<x><yy>ans` is a per-year participation flag, and `KMPS10`/`KMPS11`/`KMPS19` flag which recruitment panel the case belongs to.

**The diary pivot.** The diary file holds exactly three rows per respondent, keyed by `d<yy>m1` (values 1, 2, 3), which was verified unique with `pid` in all fifteen years.
The columns that differ between the three days are pivoted into `_d1`/`_d2`/`_d3` suffixed columns: `mm1st`, `dd1st` (the month and day-of-month of *that* diary day, despite the "1st" in the name), `day1` (day of week), `weekend`, `spday1`, `diary1`-`diary3` (2010 only), the 96 sleep slots `s1`-`s96` (2011-2024), and all 330 per-day summary columns.
The remaining diary columns (`d_wt`, `d_gender`, `d_age`, `d_age1`, `d_school`, `d_p_income`, ...) were verified constant across a respondent's three days and are carried once.

## Files

**Used - all 45 declared data files**, i.e. `h{10..24}v31_KMP_csv.csv`, `p{10..24}v31_KMP_csv.csv`, `d{10..24}v31_KMP_csv.csv`, plus the user guide PDF as documentation.

**Columns dropped from the diary files.** For each of the 96 fifteen-minute slots the diary files carry a sleep flag (`d__s#`, 2011-2024), a place code (`d__p#`), and media/activity/connection codes (`d__ma#`..`d__mi#`, `d__aa#`..`d__ai#`, `d__ca#`..`d__ci#`; nine parallel streams in 2010-2012, two from 2013 when the diary was redesigned to record one main and one simultaneous medium - guide p.16, pp.31-32).
The place and media/activity/connection slot columns are dropped; KISDI's own per-day summaries of them (`d__M_fre#`/`M_time#`/`M_user#` for 42 media codes, `A_*` for 47 activity codes, `C_*` for 21 connection codes - guide footnote 11, p.33) are all kept, pivoted across the three days.
The sleep slots are kept (288 columns, `d_s1_d1`..`d_s96_d3`, `NA` in 2010, when the diary had no sleep slot) because they are the only source for the sleep-duration rows.
Keeping the dropped slot columns for three days would have added several thousand further columns.
**This is reversible and a reviewer may want it reversed**: screen time *at school* (`st_*_school`) and any time-of-day analysis can only be reconstructed by crossing the media slots with the place slots, which the tidied table no longer carries.

**No undeclared files.** `describe_raw.R` found nothing in the study folder that the spec does not declare.

**No spec edits were made.** The spec read all 45 files without error.

## Decisions to confirm

- **Keeping every person and household column.** The tidied table is 4,432 columns wide because all person and household columns are kept.
  The source CSVs carry no variable labels and KISDI's integrated codebooks (`P_codebook`, `H_codebook`, `D_codebook`, guide Table 3-1) are **not in the study folder**, so there was no basis on which to choose a subset.
- **The diary is pivoted, not averaged.** Each respondent-year carries three diary days side by side; averaging the weekday days and the weekend days is left to the mapping.
- **Interview month for the household carry.** No interview date is released, so the month of diary day 1 stands in for it; the diary is part of the individual questionnaire (guide p.6 §2).
  With that date, 83 respondent-years with no household record were filled from a year 12 months or less away, and 644 were left missing.
- **Only the household-file columns are carried.** `h_h_income1`, `h_h_income2` and `h_fly_typ` are carried; the person-file copies `p_fly_typ`, `p_hhldsiz` and `p_houstyp` (absent in 2010) and household size (`h_hhld_siz`, `h_hhld`, `h_hhld_6`) are not.
- **`hid` misses in the household file.** 727 person-rows across all years (32 in 2010 falling to 0 in 2024) have an `hid` for which the household file has no row, so the `h_*` columns are `NA` for them apart from the carried values; they are kept.
- **Out-of-scope ages kept.** 19 rows report an age of 3, 4 or 5 (waves 10-14) although the guide says eligibility starts at 6, and 2 rows in wave 10 report `p_age1 == 9999`; all are kept, as tidiers never filter on age.
- **Which age column is authoritative.** `p_age1` is age in years and `p_age` is a banded age; the diary file carries its own copies (`d_age1`, `d_age`), and `p_byear` (birth year) is also present.
- **Six person columns forced to character.** `p_school9`, `p_school13`, `p_school14`, `p_school17`, `p_a01030` and `p_a01031` are free text in some years and numeric codes in others, which would have failed `bind_rows()`; they are cast to character in every year so no value is lost.

## Rows dropped

**None.**
No wave is dropped by the screen rule, because the diary was fielded every year.
No attendance filter is needed, because each year's person file contains only that year's respondents (`p<yy>ans == 1` for every row) and every respondent has a complete three-day diary.
No age filter is applied.

## Done in the tidier that is arguably harmonisation

- The carry of `h_h_income1`, `h_h_income2` and `h_fly_typ` (83 values each), which the harmonisation conventions assign to the tidier; the mapping should mark `ses_household` and `family_structure` accordingly and cite the count.
- The 990 pivoted diary summary columns and 288 sleep-slot columns get `label` attributes (e.g. `d_M_time19_d1` -> "Diary day 1: minutes, media code 19"), because the source files carry no labels; the labels restate the guide's naming rule and assert nothing about what a code number means.
- The six free-text/numeric columns listed above are cast to character.

No scale is scored, no sentinel code is zapped, no category is recoded and no unit is converted.

## Pointers for harmonisation

### Identity, wave, demographics, design

| dataschema target | source columns | notes |
|---|---|---|
| `participant_id` | `pid` | numeric up to ~1.2e8; use `sprintf("%.0f", pid)` |
| `wave` | `.wave` | character "10".."24" |
| `data_year` | `.wave` | `2000 + as.integer(.wave)`: the wave id is the survey's own two-digit year label |
| `age_years` | `p_age1` (completed years), `d_age1` | apply the +0.5 age rule; `p_age` / `d_age` are banded; `p_byear` is birth year; clear `9999` first |
| `sex` | `p_gender`, `d_gender` | values 1 and 2 only; which is male needs the codebook; carry with `modal_within()` in the expression; Unknown, never NA |
| `country` | constant | South Korea |
| `urbanicity` | `p_area_siz`, `h_area_siz`, `h_area_siz2` | `p_area_siz` is absent in 2010; `p_area`/`h_area` are the 17 metropolitan cities/provinces (values 1-17); the design strata are dong versus eup/myeon (guide p.20) |
| `ses_household` | `h_h_income1`, `h_h_income2` | monthly household income (월평균 가구 소득, asked every year, guide Table 1-2); banded, so cut by ridit within wave; carried in the tidier (83 values); `9999` occurs (322 rows in 2010, 231 in 2016, 121 in 2017); Unknown where missing; `p_income`, `p_income1`, `d_p_income` are personal income |
| `ses_household_measure` | constant | 'Income: monthly household income (banded)' |
| `parental_education` | none direct | the household roster gives each member's relation, sex and birth year (`h_head_*`, `h_m1_rel`..`h_m10_rel`, `h_m*_gen`, `h_m*_dob`, `h_m*_res`, `h_m*_mob`) but **no member education column**; `p_school*` is each respondent's own education, so a parent's own row could be linked to the child through `hid` and the relation codes, which need the codebook |
| `family_structure` | `h_fly_typ` (5 codes, all years), `p_fly_typ` (2011-2024) | `h_fly_typ` carried in the tidier (83 values); codes need the codebook; `p_rel` is the respondent's relation to the household head |
| `school_grade` | `p_school`, `p_school1`, `p_school2`, `p_school4`, ... | education level and its sub-items |
| `survey_weight` | `p_wt` | the core cross-sectional person weight, recomputed every year (integrated-panel weight from 2019); `d_wt` is identical (guide p.26); `p_wt2` longitudinal, `p_wt_org`/`p_wt_org2` original-panel versions from 2019; not carried, since it is observed every year |
| `survey_strata`, `survey_cluster` | none | stratified two-stage design (strata = city/province x dong/eup-myeon, PSU = enumeration district, guide p.20) but neither is released: 'Not in the supplied extract.' |
| `born_in_country`, `immigrant_background` | none found | no birthplace item in the guide's section list |

### Screen time - the media diary

This study's screen-time measure is a **three-day time-use diary in 15-minute slots**, and it is the study's only screen measure, so it is the **primary** (untagged) block: `st_measure_type` is the diary format, `st_measure_name` is 'Time use diary', and `st_responder` is Self (every household member aged 6+ keeps their own diary).
No questionnaire "hours per day" item was identified.

The usable columns are, for diary day *k* in 1..3:

- `d_M_time<code>_dk` - **minutes** on media code `<code>` (1..42)
- `d_M_fre<code>_dk` - number of episodes; `d_M_user<code>_dk` - used that day (0/1)
- `d_A_time<code>_dk` / `_fre` / `_user` - the same for 47 **activity** codes ("what they were doing")
- `d_C_time<code>_dk` / `_fre` / `_user` - the same for 21 **connection** codes ("over what route")

Units are minutes (divide by 60) and every value is a multiple of 15.
There are no missing values in these columns; a diary that records no minutes on a medium is a real zero.

**Rules that will bite if missed:**

1. **The media, activity and connection families describe the same minutes three ways.** Summed over all codes, `M_time`, `A_time` and `C_time` give an identical total for every row (correlation 1.000). Pick one family per target variable; never add across families.
2. **Simultaneous media use is double counted.** From 2013 the diary records a main and a simultaneous medium per slot (nine parallel streams before that), and each is credited to its own code, so day totals exceed 24 hours for some respondents (238 rows exceed 1,440 minutes on day 1). Keep such values; they are screened at analysis.
3. **No `st_total`.** The diary has no all-screens item, so `st_total*` is incompatible (DR-0004: a sum of separately recorded media is never a total), and codes belonging to different rows (watching, gaming, computer, phone, internet) are never summed (DR-0002). Summing is allowed only for codes that are sub-categories of one row, noted 'Sum of A, B (DR-0002)'.
4. **Weekday/weekend is derived across the three days in the mapping.** Each day has its own `d_weekend_dk` flag (1 = weekday, 2 = weekend; `888` and `9999` also occur) and `d_day1_dk` (day of week, 1-7). `st_*_wd` is the mean of the days flagged weekday, e.g.

   ```
   rowMeans(cbind(
     ifelse(d_weekend_d1 == 1, d_M_time19_d1, NA),
     ifelse(d_weekend_d2 == 1, d_M_time19_d2, NA),
     ifelse(d_weekend_d3 == 1, d_M_time19_d3, NA)
   ), na.rm = TRUE) / 60
   ```

   and `st_*_we` the same with `== 2`; the unsuffixed row is (5 x wd + 2 x we) / 7, only where both exist. Across all rows and days the split is roughly 74% weekday / 26% weekend, so most respondents have both.

**The media/activity/connection code numbers cannot be resolved from anything in the study folder.**
The guide prints a questionnaire lookup card (Table 1-4, pp.16-19) but warns that those are the *questionnaire* codes and that the raw data use master codes given only in the codebook: "표기된 다이어리 매체/행위/연결코드는 설문지상의 코드로, 실제 원시자료를 분석할 경우에는 코드북 또는 원시자료 컬럼별 설명에 나와있는 마스트코드를 참조" (p.19).
The card lists 35 media codes where the data have 42, and the guide's own worked example gives `d11M_fre19 -> 19(스마트폰)`, i.e. **media code 19 is the smartphone**, whereas the card numbers the smartphone 14.
**The `D_codebook` must be obtained from KISDI STAT (https://stat.kisdi.re.kr, "코드북 / 유저가이드") before the device-level screen-time variables can be mapped.**

As evidence for whoever does that mapping, here is the observed mean minutes on diary day 1 among respondents aged 6-17, by media code:

| year | top media codes (mean minutes/day) |
|---|---|
| 2010 | 3=106, 1=63, 7=54, 17=40, 26=6, 8=3 |
| 2017 | 1=187, 3=104, 19=90, 7=26, 8=5, 10=3 |
| 2024 | 1=245, 19=146, 3=46, 7=32, 10=30, 8=12, 26=9 |

Share of diary days with any use, all ages, moves as expected for a device list: code 19 goes from 5% in 2010 to 94% in 2024 (corroborating "19 = smartphone"), code 17 falls from 68% to 4%, code 3 holds at 86%/85%, code 7 falls from 41% to 24%.
Code 1 is large and rising among children and is **not** an aggregate of the others (correlation -0.17 with the sum of codes 2-42), but what it is cannot be settled without the codebook; do not assume it matches the card's "newspaper/book/magazine".

### Sleep - the diary sleep slots

`d_s1_dk`..`d_s96_dk` (2011-2024) are the diary's per-slot sleep flag (수면 여부; slot 1 = 00:00-00:15, slot 96 = 23:45-24:00, guide pp.31-32).
Values are 0 and 1 only, and the mean of 32 slots flagged 1 per day (about 8 hours) indicates 1 = asleep; confirm with the `D_codebook`.
Hours asleep on day *k* are `rowSums(tbl[grep("^d_s[0-9]+_dk$", names(tbl))] == 1) * 0.25`, averaged over weekday-flagged days for `hlth_sleep_wd` and weekend-flagged days for `hlth_sleep_we`; the diary day runs midnight to midnight, so it counts sleep within the calendar day rather than a night.
2010 has no sleep slot, so the sleep rows are 2011-2024 only.

### Outcome instruments

The person file's item columns are named `p_<letter><serial>`, and the guide's Table 3-3 (pp.30-31) maps the letter to the questionnaire section.
Cross-referencing that with the survey-topic tables (Table 1-1, p.7; the special-topic row of the person table, p.15) and with the years each family actually appears in the data:

| family | section (guide Table 3-3) | years present |
|---|---|---|
| `p_f*` | 삶의 만족도 및 정신건강 - **life satisfaction and mental health** (domain satisfaction, emotions experienced, mental health, perceived class) | 2013, 2017, 2021 |
| `p_h*` | 자아존중감 및 인지욕구 - **self-esteem and need for cognition** | 2015, 2020, 2021 |
| `p_g*` | 건강행태 - **health behaviour** (physical activity, drinking, smoking, height, weight) | 2014 |
| `p_e*` | 가치관과 라이프스타일 - values and lifestyle | 2012, 2016, 2022 |
| `p_n*` | 비판적 미디어 이해능력 - critical media literacy | 2020-2024 |
| `p_k*` | 4th industrial revolution (2018) / digital transformation (2023) | 2018, 2023 |
| `p_m*` | 소비자 혁신성 - consumer innovativeness | 2019, 2024 |
| `p_a*` | 휴대폰 - mobile phone | all years |
| `p_l*` | 태블릿 PC - tablet | 2019-2024 |
| `p_j*` | 웨어러블 기기 - wearables | 2017-2024 |
| `p_b*` | 보유 기기 간 연결상태 - device interconnection | 2010, 2011, 2016-2018 |
| `p_c*` | 방송통신 서비스 가입 및 지출 - telecom subscription and spend | all years |
| `p_d*` | 미디어 이용현황 / 미디어 활용 현황 - media ownership and use | all years |
| `p_i*` | 전자상거래 및 온라인 거래 - e-commerce | 2016-2024 |

Household families (Table 3-3): `h_a*` media device ownership and use, `h_b*` device connectivity (2010, 2011 only), `h_c*` telecom subscription and spend, `h_d*` **미디어 이용 제한 - media use restriction**, every year.

Candidates among the newly added dataschema rows, all needing `P_codebook`/`H_codebook` for item wording and codes:

- `fam_screenrules`: `h_d*`, the household questionnaire's restrictions on TV, internet, games and smart devices, with daily-average limit items added in later years (guide Table 1-2, p.10); the years each item ran and who answers need the codebook, and a household-level answer should record its respondent.
- `hlth_sleep_wd`, `hlth_sleep_we`, `hlth_sleep`: the diary sleep slots above.
- `beh_alcohol_self`, `beh_tobacco_self`: `p_g*` drinking and smoking items, 2014 only.
- `wb_domainsat_*_self`, `wb_posaffect_self`, `mh_sad_self`, `mh_worry_self`, `mh_lonely_scale_self` and the other single-item mental-health rows: `p_f*` (2013, 2017, 2021), if the items match the rows' wording and response format (frequency-item rules).
- Self-esteem (`p_h*`) has no dataschema row of its own.

**The mental-health, wellbeing and self-esteem modules run only in scattered years**, so those outcomes exist for three waves at most.
There is **no academic-achievement, cognition, behaviour or bullying instrument** anywhere in the person file's section list.

### Missing-value codes and quirks

- **`9999` and `888` are missing/refusal codes**, not values. Across the tidied table `9999` occurs 27,164 times in 350 columns and `888` occurs 20,927 times in 19 columns, including `p_age1` (2 rows), `d_school`, `h_h_income1`, `d_weekend_dk`, `d_day1_dk`, `d_mm1st_dk` and `d_spday1_dk`. Nothing was zapped in the tidier, because the codebook is not available to confirm the codes' meaning in every column; use `na_if()` per mapped column.
- **No value labels or variable labels exist** in the source, because the release is plain CSV. The only labels in the tidied table are those the tidier attaches to the diary summary, sleep-slot and carried columns.
- **Korean free text is mojibake.** The CSVs are encoded in CP949/EUC-KR and the pipeline's `csv` reader reads them as UTF-8, which garbles the ~121 person and ~68 household free-text columns (e.g. `p_school9` in 2010, `p_a01030` mobile tariff names). Numeric and coded columns are unaffected. Fixing it would need an encoding option in `R/registry/read_tabular_file.R`.
- **257 columns are `NA` in every row.** Each appears in only one or two years' files and is empty there too, so it is an empty column in the release, not a missed rename. Most are `p_b*` (device interconnection) and `h_a*`/`h_c*` items.
- **Columns are populated in only some years** throughout, because the survey adds and drops modules annually; 485 of the person stems appear in a single year.
- **The item serial numbering changed in 2014**: sub-item numbers went from two digits to three (guide footnote 8, p.30), so a 2013 stem and a 2014 stem are not always the same question. Check the codebook before pooling an item across that boundary.

## Open questions / spec problems

1. **The integrated codebooks are missing from the study folder.** `P_codebook`, `H_codebook` and `D_codebook` (guide Table 3-1) give every column its question wording and every code its meaning; without them the diary media/activity/connection codes, the sleep-flag coding and all `p_*`/`h_*` item serials are opaque. They should be requested from KISDI STAT, added to the study folder and declared in the spec with `role: "codebook"`.
2. **The spec calls the `d*` files "device-level data".** They are the **media diary** files; the resource names (`kmp_wave<n>_device_data`) and the `notes:` block in `dataset.yaml` are misleading. Not changed, because the rename is not needed for the files to be read.
3. **The spec's wave ids are years.** `wave: "10"` .. `wave: "24"` are 2010..2024; consider setting `label:` to the calendar year.
4. **A 2025 wave exists** (guide Table 3-1) and is not in the study folder, which also holds release `v31` where the guide documents `v32`.
5. **The wide ("가로 통합") diary version is not in the folder**, only the long one, so there were no duplicate-format files to choose between.
6. **What media code 1 is**: it accounts for the largest share of children's diary minutes in recent years and its identity is unresolved.
7. **Parental education has no direct source column.** Linking a child to a parent's own `p_school` through `hid` and the relation codes (`h_m*_rel`, `p_rel`) is possible but needs the codebook.

## Convention review (2026-09-25)

- Checked the tidier against the 2026-09-25 conventions: no wave fails the screen rule (diary every year), no attendance filter is needed, the diary stays columns on one row, and nothing fixed (sex etc.) is carried.
- Added a within-`pid` carry of `h_h_income1`, `h_h_income2` and `h_fly_typ` across gaps of 12 months or less, dated by the diary month: 83 values each, no observed value changed.
- Kept the diary sleep slots (`d_s#`, 288 columns, 2011-2024) as the source for `hlth_sleep*`; the table went from 4,144 to 4,432 columns with the same 147,915 rows.
- Shortened the header and comments to the new style.
- Notes: the total-screen-time advice now follows DR-0004/DR-0002, the diary is the primary block, and the notes add the survey design, weights, fills and pointers for the new dataschema rows.

## Codebook update (2026-09-30)

KISDI's integrated codebooks are now in the study folder and declared in the spec as dataset-wide `role: "codebook"` resources: `P_codebook_v32.xlsx`, `H_codebook_v32.xlsx`, `D_codebook_v32.xlsx` and `D_codebook_wide_v32.xlsx` (the last documents the wide diary release, which is not in the folder).
They are release v32 (2010-2025); the data files are v31, and the variable names used here match.
The spec's `notes:` now describe the `d*` files as the media diary; resource names are unchanged.

Settled by the codebooks, superseding the statements above:

- Media code 1 is 신문/책/잡지 (newspaper, book, magazine), print rather than a screen; code 19 is the smartphone (incl. kids' phones and paired smartwatches), 3 home TV, 7 desktop, 8 laptop, 10 tablet, 31 handheld console, 32 home console (D_codebook, 매체 sheet).
- Activity codes give content: TV programmes 1-5, 37, 38; film/video 8 (to 2019) and 41 (from 2020); creator video 9, 10; SNS 21; messaging 17, 19; games 23; learning video 42 (from 2020); online class 46 (from 2022); video call 44 (from 2021; pooled with voice in code 16 before).
- Sleep slots: 0 = awake, 1 = asleep; weekend flag: 1 weekday, 2 weekend; `888` = no diary written that day; `9999` = don't know / no answer throughout the release.
- Sex: 1 male, 2 female.
- Each diary stream records one medium with its activity and connection, so device x activity crossings are exact.
- Place code 4 is 교육시설 (education facility, which includes private academies).
- `p_school13` (college type) is asked only of current university students; no item splits a parent's junior-college from four-year degree.
- `h_fly_typ` code 3 (couple + children) includes about 600 child-rows with no resident spouse in the roster, so it does not identify single parents.

Tidier changes:

- Every column the codebooks describe carries its (Korean) codebook label; diary summaries also name the code, e.g. `d_M_time19_wd` = "Mean over weekday diary days: minutes, media code 19 (스마트폰 이용)".
- The diary is no longer pivoted into per-day `_d1`/`_d2`/`_d3` columns: every day-level column is averaged in the tidier over the respondent's weekday (`_wd`) and weekend (`_we`) diary days, by each day's weekend flag, so days not written (888) drop out and the mapping rows are plain sums.
  `d_days_wd`/`d_days_we` count the days of each type (47.7% of respondent-years have no weekend day), and `d_mm1st`/`d_dd1st` keep the first diary day's date for the interview month.
- New columns built from the slot columns before they are dropped, counted as KISDI counts `*_time` (15 minutes per slot and stream): `d_M/A/C_edutime<code>_wd` and `d_edu_time_wd` (minutes at an education facility, averaged over weekday days with any time there; `d_edu_days_wd` counts them), `d_M_gametime<code>_wd/_we` (minutes per medium spent gaming; summed over media they equal `d_A_time23` exactly) and `d_sleep_wd/_we` (minutes asleep, 2011-2024).
- `d_M_user41_*` (VR-device use flag) is NA almost everywhere because KISDI leaves it blank rather than 0 for non-users.
- Parent linkage: `mother_school`/`mother_school2` and `father_school`/`father_school2` are the parent figure's own `p_school`/`p_school2` (head and spouse for a child of the head; head's child and child-in-law for a grandchild; one member of each sex only), carried within `pid` from the earliest year with a value: 1,857 mother values and 3,427 father values filled (407 and 1,107 on rows aged 6-17).
- `parent_figures` (0-2 resident parent figures in the roster) for family structure, carried across gaps of 12 months or less like `h_fly_typ` (168 values filled).
- The carry helper now groups donors by participant (the tidier runs in about 35 s).
- Result: 147,915 rows x 3,997 columns, same rows and key as before.

Still open: Korean free text is mojibake (CSV reader encoding); `h_area_siz2` (codes 1-3) has no codebook entry.
