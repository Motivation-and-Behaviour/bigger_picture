# BPIPD-1127 (TEPS) - tidier notes

## Study summary

- Study: Taiwan Education Panel Survey (TEPS), first phase, run by Academia Sinica with the Ministry of Education and the National Science Council (TEPS English overview, `TEPS-EnglishOverview081231b.pdf`, p. 1).
  The spec's `notes` call it the "Transition from Education to Employment Panel Survey", which is wrong.
- Design: school-based longitudinal panel of three samples, with students, parents and teachers surveyed at each wave (overview pp. 3-9).
  - J: 2001 grade-7 junior-high sample (1988/89 birth cohort), surveyed in 2001 (G7) and 2003 (G9); a subset, the core panel (CP, `cp == 1`), was followed again in 2005 (G11) and 2007 (G12) (overview pp. 3, 9).
  - S: 2001 grade-11 senior-high, comprehensive, vocational and junior-college sample (1984/85 birth cohort), surveyed in 2001 (G11) and 2003 (G12) (overview p. 3).
  - NP: a new grade-11 panel drawn in 2005, which with the CP forms a representative 2005 grade-11 sample, surveyed in 2005 and 2007 (overview p. 9).
  NP students are the CP's grade-11 classmates (about 16,000 alongside about 4,200 CP; abstract of the TEPS/TEPS-B introduction, Airiti Library 05296528-201703-201703060009), so they belong to the same 1988/89 birth cohort as J.
- Country: Taiwan (overview p. 1).
- Sampling: multi-stage stratified cluster sample; strata by urban/rural area, public/private and senior-high/vocational school; schools, then classes, then 15 students per class (whole classes in aboriginal, earthquake-affected, small and junior-college classes); standardised weights are released (overview pp. 8-9).
- Fieldwork (user guides, `User Guide.docx` in each folder): W1 2001-09-01 to 2002-03-01; W2 2003-03-03 to 2003-05-16; W3 2005-10-01 to 2006-01-31; W4 2007-03-01 to 2007-07-31.
- Respondents: the student (questionnaire plus the Comprehensive Analytical Ability test), a parent (questionnaire taken home), homeroom and subject teachers (a rating of each student plus the teacher's own questionnaire), and the school (W1 junior only) (overview pp. 4-7).
- Ages: about 12-13 (J at W1) to about 18-19 (CP and NP at W4); birth years in the data are ROC 77-78 (1988-89) for J (`w1s501`) and ROC 73-74 (1984-85) for S (`w1s501`).
  NP's `w3s479` carries the labels 1-5 = ROC 71-75, which would put most NP students at ROC 73-74 and age 20-21 in grade 11; its distribution (codes 3 and 4 hold 96%) matches J's ROC 77-78 shifted by four codes, so the labels look copied from the 2001 senior form; the tidier relabels codes 1-5 as ROC 75-79 (project decision, 2026-10-01).
- W1 collection report: 20,004 junior and 19,332 senior/vocational/junior-college students (overview p. 9); the released W1 files hold 20,055 junior and 19,051 senior rows (by `w1pgrm`: general senior high 8,422 vs 8,719, comprehensive 2,200 vs 2,062, vocational 3,944 vs 4,066, junior college 4,485 vs 4,485; net 281 fewer, unexplained).
- The CP is "about 4,200" students (TEPS/TEPS-B introduction, Airiti Library abstract and ResearchGate publication 314260007; the overview gives no count); the W3 file holds 4,261.

## Tidied table

- Grain: one row per student per wave they took part in; key `participant_id` + `.wave` (unique; the tidier stops otherwise).
- `participant_id` = `<cohort>-<stud_id>`; `stud_id` repeats between the 2001 junior and senior samples (9,873 shared ids, different people: J rows are born ROC 77-78, S rows ROC 73-74).
  CP students keep their J id (all 4,261 W3 CP ids are W1 junior ids).
- `cohort`: `J`, `S` or `NP`; `cp` (W3/W4 only) marks the CP.
- Observations: 115,893 rows; participants: 54,894 (J 20,020; S 19,050; NP 15,824).
- Rows per wave: W1 39,070 (J 20,020, S 19,050); W2 37,289 (J 18,925, S 18,364); W3 20,079 (CP 4,255, NP 15,824); W4 19,455 (CP 4,162, NP 15,293).
- Columns: 2,825 (2.4 GB in memory), all source columns of the student, parent and teacher-rating files kept under their source names; item names carry the wave prefix (`w1s427`, `w2s107`), so each item is populated only in its wave and the mapping coalesces across waves.
- Assembly per wave: the student file is the spine; the parent file and the teacher-rating ("student performance evaluation") file join one-to-one on `cohort` + `stud_id`.
  W1 and W2 stack the junior and senior files; W3 and W4 use the combined CP+NP files (`_cpnp`) and add the CP-only columns of the `_cp` files.
  Parent- and rating-file columns whose names clash with the student file's (the QA counts `wNsumerr`, `wNsummis`, `wNsumtrp`, `wNsumlog`, `wNlogK`) get a `_parent` or `_rating` suffix.
- Screen rule: no wave or sample dropped; every wave x sample answered a disaggregated duration item (W1 `w1s427` computer/internet 38,746 valid; W2 `w2s106`/`w2s107` about 37,000; W3 `w3s208`, `w3s209`, `w3s212`, `w3s213` about 20,000; W4 `w4s117`/`w4s118` about 19,250).
- Values carried or filled across waves (no observed value changed; codes 97 and 99 are never carried):
  - `w1faedu`, `w1moedu` (W1 father's/mother's education): carried from W1 to later rows; 42,662 and 43,326 cells.
  - `faedu_w34`, `moedu_w34` (W3/W4 parent education, a different scale): earliest W3/W4 value carried to rows with no valid W1 value; 728 and 768 cells (190 and 192 of them on W1/W2 rows of CP students with no valid W1 value).
  - `w1s501` (birth year, W1 coding): carried from W1 to later rows; 45,632 cells (age at follow-up, which has no age item).
  - `w3s479` (birth year, W3 coding, NP only): carried to W4 rows with no valid W1 birth year; 15,235 cells.
  - `sampling_stwt1`, `sampling_urban3`, `sampling_priv`, `sampling_pgrm`: design values at the sample's first wave (W1 for J and S, W3 for NP), on every row; 60,999 rows carry them from another wave.
  - Not carried: household income and family structure, because every gap between waves exceeds 12 months (fieldwork dates above); sex, ethnicity and other fixed characteristics (mapping).

## Files

- Used: student, parent and teacher-rating `.dta` files of every wave (`wave1_junior_student`, `wave1_senior_student`, `wave1_junior_parent`, `wave1_senior_parent`, `wave1_junior_student_performance`, `wave1_senior_performance`, the matching `wave2_*`, `wave3_senior_student_cpnp`/`_cp`, `wave3_senior_parent_cpnp`/`_cp`, `wave4_senior_student_cpnp`/`_cp`, `wave4_senior_parent_cpnp`/`_cp`, `wave4_senior_performance_cpnp`).
- Ignored:
  - Teacher questionnaires `*_ct`, `*_dt`, `*_et`, `*_mt` and class ratings `*_ctc`, `*_dtc`, `*_etc`, `*_mtc` (W1-W4, including `wave3_teacher_files__*` and `wave4_teacher_files__*`): the teacher's own background, practice and class-level views, attached to each student; no child-level construct, and four subject teachers share item names.
  - `wave1_junior_school_questionnaire`: school level, and the student files carry no school id to link it.
  - `wave3_senior_performance_cp`/`_cpnp`: byte-identical copies of the W3 student files (same MD5); the W3 teacher-rating files (`w3_sf_testing_*` in the user guide) are not in the folder.
  - `wave4_senior_performance_cp`: its 3,921 rows repeat the combined file's rows exactly.
  - The CP files' items shared with the `_cpnp` files: identical, except that 8 (W3) and 30 (W4) CP students are blank in the `_cp` file where the `_cpnp` file has 99; the `_cpnp` copies are used.
- Undeclared files in the study folder: the `.sav`, `.dat`, `.sas` and `.csv` twins (not needed); `Wave 1/.../w1w4_jsf_sch...202109.pdf` and a `.doc` (school-questionnaire documentation with mis-encoded names); `Wave 2/C00124_Gc.pdf`, `Wave 3/C00137_Gc.pdf`, `Wave 4/C00137_Gc.pdf` (copies of the W1/W2 parent crosswalks).
- Spec edits (the only change to `dataset.yaml`): 31 resource globs pointed at folder names the study folder has in truncated form (for example `First Wave (2001) Junior High School Student Questionna`), and the W1 junior student file is `w1_j_s_lv6_0.dta`, not `w1_j_s_lv6.0.dta`.
  Before the fix the W1 student and teacher files, the W2 senior rating file and every W3/W4 student, rating and teacher file were unread.
  Each glob now ends the truncated folder name with `*` (for example `...Junior College Stu*/w1_sf_s_lv6.0.dta`), so it matches the truncated and the full name; nothing else in the spec changed.

## Decisions to confirm

- The spec glob fix (31 globs, above): minor in kind but many lines; it was needed for the student files to be read at all.
- Attendance: a student counts as taking part in a wave if they answered the student questionnaire (`wNrefuse == 0`), sat the ability test (`wNall3p` present), or have a parent or teacher report with a valid answer.
  This keeps 506 rows with no student questionnaire (W1 78, W2 226, W3 29, W4 173), which carry test scores or parent/teacher reports but no screen data.
  The alternative is to keep only questionnaire respondents.
- Survey design: `sampling_*` hold the first-wave design values, so CP rows at W3/W4 carry the 2001 J design and weight, although the 2005 weight `w3stwt1` treats CP and NP as one sample.
  Each wave's own released weight stays in `w1stwt1`, `w2stwt1`, `w3stwt1`, `w4stwt1` (and `w2stwt2`, `w4stwt2`).
  Which to map to `survey_weight` (the wave's own weight, compatible, or the carried first-wave weight, partial) is a project call.
- Parent education at W3/W4 was relabelled with W4's six-level scale (`w4p104`: 1 junior high or less, 2 senior high, 3 vocational high, 4 junior college/institute of technology, 5 university, 6 graduate school), because the released labels are W1's five-level ones; the evidence is the CP fathers' W1 x W3 cross-tab (W1 3 junior college -> W3 4, W1 4 university -> W3 5, W1 5 graduate -> W3 6, W1 2 senior/vocational high -> W3 2 or 3).
  Further support: where the responding parent is the father, `w3faedu` equals his own `w3p104` in every case and `w4faedu` equals `w4p104` in 99.7% (the mother's `w4moedu` 99.8%), and `w4p104` carries the six-level labels.
  The W3/W4 parent questionnaire would settle it; it is not in the folder.
- W1's only disaggregated screen item is computer/internet time (`w1s427`); `w1s423` bundles sports, music and TV/video, so W1 has no watching item.
- Age at follow-up: no wave has an age item, so the tidier carries birth year (`w1s501`, `w3s479`) to later waves rather than an age plus interval; the mapping computes age from it and the fieldwork dates.
  `w3s479` is relabelled ROC 75-79 (decided 2026-10-01; see Ages above).

## Rows dropped

- Rows with no student questionnaire, no ability test and no valid parent or teacher report (not observed at that wave): 294 in total.
  W1 J 35, W1 S 1, W2 J 163, W2 S 19, W3 CP 6, W3 NP 51, W4 CP 1, W4 NP 18.
- No wave, sample or age filter.

## Done in the tidier that is arguably harmonisation

- `faedu_w34`/`moedu_w34` carry corrected value labels (above); the codes are unchanged.
- The senior files' value labels were replaced by the junior files' where they differ only in case or spacing, and W2 senior `w1w2chg` labels (mis-encoded Big5) were replaced by the junior file's.
- `w3s479` relabelled: codes 1-5 = ROC 75-79 (released as 71-75); the codes are unchanged.
- Derived columns, so `variables.csv` needs no `local()` (source columns kept):
  - one column per construct asked in several waves under wave-prefixed names (`bp1127_cross_wave_columns()`, a labelled map that stops if two waves' sources are filled on one row): `father_ethnic_group`, `mother_ethnic_group`, `household_income`, `lives_with_father`, `lives_with_mother`, `lives_with_step_parent`, and the six symptom-checklist items asked at every wave (`feel_withdrawn`, `feel_low`, `urge_scream_hit`, `feel_lonely`, `sleep_problems`, `tight_head_numbness`); codes unchanged;
  - `birth_year` (Gregorian, from the ROC-year labels of `w1s501` and `w3s479`);
  - `father_education`, `mother_education` (W1's scale: the W3/W4 value where valid, with W3/W4 senior and vocational high school merged into W1's code 2, else W1's) and `parents_highest_education` (the higher of the two, 'other' ignored).
- Birth year carried for age at follow-up (above); no age was computed.

## Pointers for harmonisation

- `participant_id`: `participant_id` (compatible).
- `wave`: `.wave` (`1`-`4`; `.wave_label` 2001, 2003, 2005, 2007).
- `data_year`: no per-row interview date; W1 fieldwork spans 2001-09 to 2002-03, W2 2003, W3 2005-10 to 2006-01, W4 2007 (inferred from the user guides).
- `age_years`: `w1s501` (J, S, CP; codes 1-9 = ROC 71-79, 10 other; 97/99 missing) and `w3s479` (NP; codes 1-5 = ROC 75-79 as relabelled in the tidier, 6 other); Gregorian year = ROC + 1911; age = fieldwork year minus birth year, adjusted by the fieldwork midpoint.
  Both are carried to waves without the item (partial there).
- `sex`: `w1s502` (W1, and repeated in the W2 and W4 files), `w2s445` (W2), `w3s480` (W3); 1 male, 2 female, 99 missing; carry and take the mode in the mapping.
- `country`: Taiwan (constant).
- `school_grade`: from `cohort` and `.wave`: J W1 G7, W2 G9, W3 G11, W4 G12; S W1 G11 (junior college year 2), W2 G12; NP W3 G11, W4 G12 (overview p. 9); programme in `wNpgrm`/`wNclspgm`.
- `ethnicity`, `indigenous`, `minority`: `w1faethn`/`w1moethn` (W1), `w3faethn`/`w3moethn` (W3, NP only): the ethnic group of each parent's father (1 Fukienese/Hokkien, 2 Hakka, 3 Mainlander, 4 aborigine, 5 other; W3 also has an unlabelled 6); `w1p105`, `w1p122`, `w3p113`, `w3p702` are the respondent's and spouse's fathers' groups.
  Languages spoken (`w1s5051`-`w1s5056`, `w3s3311`-`w3s3316`) are proxies only.
- `immigrant_background`, `born_in_country`: no child or parent birthplace; `w3p105`/`w3p703` ask where the parent lived longest before 18.
- Parent education: `w1faedu`, `w1moedu` (W1 scale: 1 junior high or less, 2 senior/vocational high, 3 junior college, 4 university, 5 graduate, 6 other) and `faedu_w34`, `moedu_w34` (W3/W4 six-level scale); use `faedu_w34` when present, else `w1faedu`; the respondent's own items are `w1p104`/`w1p121`, `w3p104`/`w3p701`, `w4p104`/`w4p401`; W2 has none (carried).
- `ses_household`: monthly household income `w1p515` (W1), `w2p508` (W2), `w3p602` (W3); six bands from under NT$20,000 to NT$200,000 or more at W1/W2; `w3p602` keeps only its code-1 label (NT$20,000 or less) and its distribution differs from W1/W2, so check its bands before use; W4 has no income item (Unknown; no carry across the 2-year gap).
  Parent occupation: `wNfaocc`/`wNmoocc`, employment `wNfawork`/`wNmowork`.
- `family_structure`: respondent's marital status `w1p103`, `w2p103`, `w3p103`, `w4p103` (1 married, 2 widowed, 3 divorced/separated, 4 cohabiting, 5 other); lives with father/mother/step-parent/grandparents/siblings/other `w1s2021`-`w1s2026`, `w3s3021`-`w3s3026`; W2 `w2s4561`-`w2s4566` is before junior high only.
- `urbanicity`: school urbanisation `wNurban3` (1 village/country, 2 town, 3 city), a school-location stand-in.
- Survey design: `survey_weight` see Decisions; `survey_strata` from `sampling_urban3` x `sampling_priv` x `sampling_pgrm`; `survey_cluster` unavailable (no school or class id is released in the student files).
- Screen time (all self-report, bands; open top bands take their stated lower bound; weekly items / 7; DR-0002 forbids summing across categories, and DR-0004 leaves `st_total` incompatible: there is no all-screens item):
  - W1 `w1s427` daily time using the computer or the internet (1 almost none, 2 under 1 h, 3 1-2 h, 4 2-3 h, 5 3 h or more): usual day, no weekday/weekend split.
  - W1 `w1s423` daily time on sports, music or TV/video tapes: bundled with non-screen activities; not watching.
  - W1 `w1s4261`-`w1s4265`: most common computer use (single choice; no duration); `w1s425` age started using a computer; `w1s1145` summer internet/video games over 2 h (yes/no).
  - W2 `w2s105` weekly internet for homework, `w2s106` weekly chat/BBS/email/MSN, `w2s107` weekly online games (1 almost none, 2 under 5 h ... 6 20 h or more; `w2s105` bands 2 h wide, top 8 h or more); `w2s1225` summer internet/video games over 2 h a day (yes/no); `w2s4601` goes to internet cafes or plays video games with friends (activity choice).
  - W3 `w3s208` daily TV or videos (1 under 1 h ... 5 4 h or more, 6 almost none), `w3s209` daily talking with friends on the phone or texting (1 under 30 min ... 5 3 h or more, 6 almost none), `w3s212` weekly chat/BBS/email (1 under 5 h ... 5 20 h or more, 6 almost none), `w3s213` weekly online games (same bands), `w3s128` weekly internet for homework.
  - W4 `w4s117` weekly online chat/BBS, `w4s118` weekly online games, `w4s116` weekly internet for homework (W3 coding, 6 = almost none).
  - Code order changes: W1/W2 code 1 is "almost none", W3/W4 code 6 is "almost none"; recode per wave.
  - `st_measure_name` Custom; `st_responder` Self; one measure per wave (no second responder).
  - Online games only is a gaming sub-type (partial on `st_game`); chat/BBS/email/MSN is messaging or social media per the schema notes.
- `fam_screenrules`: `w3s3431` (CP only, W3): retrospective, whether parents limited TV-watching or play time in grades 5-6; no current item.
- `beh_problematicuse_self`: `w2s130` lost track of time when online (never/seldom/sometimes/often; W2 only).
- Mental health (self-rated frequency this semester, 1 never, 2 seldom, 3 sometimes, 4 often; a TEPS checklist, not a published instrument):
  - W1 `w1s515`-`w1s528` (trouble concentrating, withdrawn, down and frustrated, worried/nervous, want to scream or fight, want to end life, shaking/tense, lonely, helpless/no one to rely on, dizziness/headache/stomach ache, muscle pain, sleep problems, head pressure/numbness, lump in throat).
  - W2 `w2s426`-`w2s438` (withdrawn, down, want to scream, shaking, lonely, sleep problems, never enough sleep, head pressure, feel loved and cared for, bad luck, irritated, guilty; `w2s433` litter is a behaviour).
  - W3 `w3s429`-`w3s445` and W4 `w4s443`-`w4s459` (withdrawn, down, want to scream, don't want to live, lonely, helpless, sleep problems, head pressure, never enough sleep, feel cared for and loved, bad luck, irritated, guilty, nervous/anxious, exhausted, too much to do, under a lot of pressure).
  - Single-item candidates: `mh_sad_self` (down: `w1s517`, `w2s427`, `w3s430`, `w4s444`); `mh_lonely_self` (`w1s522`, `w2s430`, `w3s433`, `w4s447`); `mh_irritable_self` (want to scream/smash/fight: `w1s519`, `w2s428`, `w3s431`, `w4s445`; irritated: `w2s437`, `w3s440`, `w4s454`); `mh_lifenotworth_self` (`w1s520`, `w3s432`, `w4s446`; also `w4s404` life is meaningless); `mh_nobodycares_self` (helpless, no one to rely on: `w1s523`, `w3s434`, `w4s448`); `mh_worry_self` (`w1s518`; `w3s442`, `w4s456` nervous/anxious); `mh_stress_self` (`w3s445`, `w4s459`); `hlth_sleepprob` (`w1s526`, `w2s431`, `w3s435`, `w4s449`).
- Wellbeing: `wb_happiness_singleitem_self` from `w1s555`, `w2s458`, `w3s478`, `w4s460` (1 very happy ... 4 not happy at all; reverse); `wb_domainsat_appearance_self` `w3s403` (satisfied with appearance, 4 points); social support: family `w1s253` (family is an important source of support), feel loved and cared for `w2s435`, `w3s438`, `w4s452`; friends `w1s435` (a close friend to share thoughts with, yes/no), `w3s462` (number of such friends).
  `w2s435` is answered by only about 18,900 W2 students.
- School: belonging-type items `w1s301`-`w1s304` (school is a happy place, make friends, bored, learn a lot), `w3s108`-`w3s111`; homework time `w1s110`, `w2s120`, `w3s106`, `w4s134`.
- Behaviour (self-report since the start of the semester/grade): truancy `w1s116` (times skipped last semester), `w1s509`, `w2s125`, `w3s447`/`w3s448`, `w4s121`/`w4s122`; fights `w2s127`; fights at school or conflicts with teachers bundled `w1s510`, `w3s449`, `w4s123`; smoking, drinking or betel nut bundled `w1s512`, `w3s451`, `w4s125`; stealing or vandalism bundled `w1s514`, `w3s453`, `w4s127`; ran away `w1s513`, `w3s452`, `w4s126`; ever got drunk `w4s4633` (0/1); victimisation (threatened, extorted/robbed) `w1s334`, `w1s335`, `w3s117`, `w3s118`.
- Sleep: W2 `w2s101` and W3 `w3s101` hours slept on a school day (bands under 5 h ... 8 h or more); W1 wake `w1s101` and bedtime `w1s109a` (J and senior high) / `w1s109b` (junior college, different bands); W4 bedtime `w4s102` and wake `w4s103` (6 = it depends).
- Ability test (CAAT, IRT 3PL ability comparable across waves and programmes): `wNall3p` (overall), `wNcf3p` (general analytical), `wNm3p` (mathematics), with number-correct `wNnright`, `wNcfree`, `wNmath`.
- Teacher ratings: W1 `w1td01`-`w1td17` (homeroom teacher: abilities and problem behaviours), `w1tcs1`-`w1tcs4`, `w1tes*`, `w1tms*` (Chinese, English, maths teachers: keeping pace, studying hard, late homework, participation); W2 adds `w2tcs5`/`w2tes5`/`w2tms5` (class rank band); W4 `w4td01`-`w4td08` (includes `w4td05` resistance to stress, `w4td06` depression).
- Parent report on the child: W1 `w1p213`-`w1p221` (physical discomfort, sleep problems, concentration, wants to scream/fight, wants to end life, homework, pornography, smoking/drinking/betel nut, stealing; 5 = not sure); W3 `w3p207`-`w3p216` (same list plus overall happiness `w3p216`; value labels truncated to code 1); parent-rated academic performance `w2p211`, `w4p201`.
- Missing-value codes: 97 illegal value, 99 missing throughout (unlabelled but present in many W3/W4 columns); some items use 5 = not sure or 6 = it depends.
- Label quirks: `w3clspgm`, `w3priv`, `w3urban3` have variable labels shifted by one (their value labels are right); `w4stwt1`/`w4stwt2` labels begin "w1:"; the W2 junior file's behaviour items (`w2s123`-`w2s130`) say "since the fall semester of grade 12" but for J students mean since grade 8 (crosswalk `C00137_Ac_1.pdf`); many W3 parent items keep only the code-1 value label.
- Free-text friend names (`w1p224`, `w1p227`, `w1p230`, `w3p503`, `w3p506`, `w3p509`) are in the table; never map or display them.

## Open questions / spec problems

- Spec `notes` name the study wrongly ("Transition from Education to Employment Panel Survey"); left as is.
- W3 teacher-rating data (`w3_sf_testing_*`, crosswalk `C00175_Cc.pdf`) are missing; the W3 performance folder holds copies of the student files (contributor question).
- The W3 and W4 parent-questionnaire crosswalks are not in the folder (only W1/W2 parent crosswalks, copied into the W3/W4 folders); they are needed for `w3p602` income bands, the other truncated W3 parent labels and to confirm the W3/W4 parent-education scale (contributor question).
- The released W1 senior file has 281 fewer students than the overview's 2001 count (19,051 vs 19,332).
- `w3s479` (NP birth year, "In which ROC year were you born?", new-sample item per `C00175_Ac_1.pdf`) is labelled 1-5 = ROC 71-75 in the `.dta` and `.sav` files, which is implausible for 2005 grade-11 classmates of the CP (born ROC 77-78); the tidier now reads codes 1-5 as ROC 75-79 (project decision, 2026-10-01); the W3 student questionnaire would confirm it (contributor question).
