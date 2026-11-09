# Fixed-width layouts for BPIPD-775 (ECLS-K and ECLS-K)

The study folder holds two NCES public-use child files as fixed-width ASCII, each with the Stata dictionary (`.dct`) and do-file (`.do`) NCES ships for reading it.
The two layouts here were generated from those dictionaries by `make_layouts.R` in this folder.

| Layout | Data file | Dictionary | Fields kept | Lines per record | Record width |
|--------|-----------|------------|-------------|------------------|--------------|
| `eclsk2011_k5_child.csv` | `childK5p.dat` (ECLS-K, K-5) | `ECLSK2011_K5PUF.dct` | 11,945 of 26,060 | 27 | 135,066 |
| `eclsk_k8_child.csv` | `childk8p.dat` (ECLS-K, K-8) | `ECLSK_Kto8_child_STATA.dct` | 8,388 of 18,949 | 15 | 74,328 |

## These files are multi-line records

Each child's record is spread over several physical lines: the dictionaries open with `_lines(27)` and `_lines(15)`, and every field sits under a `_line(k)` marker with a `_column()` position within that line.
The current `fwf` reader treats one physical line as one record, so it cannot read these files, and `dataset.yaml` keeps the dataset at `status: "blocked"` until it can.
Pointed at these layouts today, the reader warns that the layout runs past the end of the first line, parses every physical line against the whole layout (490,698 lines by 11,945 fields for the K-5 file) and then stops on thousands of parse failures; on a three-record sample of each file it failed exactly that way.

The layouts are written so that a reader which joins each record's lines end to end can use them unchanged:

- `start` and `end` are 1-based, inclusive positions in the record formed by concatenating the record's physical lines without their line terminators.
- `line`, `line_start` and `line_end` are the dictionary's own line number and within-line columns, for a reader that works line by line instead.

This works because every physical line is exactly as wide as the dictionary describes it.
Each line of the dictionary is described gaplessly from column 1, so a line's width is the end of its last field; the K-5 line widths run 5,009, 5,000, 5,000, 4,999, 5,009, 4,999, 5,002, 5,001, 4,999, then 5,004 for lines 10-26 and 4,980 for line 27, and the K-8 widths are 5,302, 5,301, 5,301, 5,300, 5,301, 5,305, 5,310, 5,306, 5,306, 5,303, 5,301, then 5,304 for lines 12-14 and 80 for line 15.
A scan of every line of both files (2026-11-08) found each line exactly that wide and ending in CRLF.

- `childK5p.dat`: 490,698 lines, 18,174 records of 27 lines; (135,066 + 27 x 2) x 18,174 = 2,455,670,880 bytes, the file size exactly.
- `childk8p.dat`: 62,502 lines, so 4,166 complete records of 15 lines plus the first 11 lines of record 4,167 and a 12th line cut off at 3,616 of 5,304 characters with no line end.
  NCES documents 21,409 children for this file, so the copy in the folder is truncated (see `BPIPD-775_notes.md`).
  A multi-line reader must refuse a file whose line count is not a multiple of the record length, or this truncation would pass silently.

## Columns

| Column | Meaning |
|--------|---------|
| `name` | Variable name from the dictionary. |
| `start`, `end` | Positions in the joined record (see above); ascending and non-overlapping, checked by the script. |
| `type` | `c` for `str` fields, `d` for every numeric field. |
| `line`, `line_start`, `line_end` | The dictionary's `_line()` and `_column()` positions. |
| `stata_type` | The dictionary's storage type (`str8`, `long`, `double`), kept for provenance. |
| `format` | The dictionary's display format (`%8s`, `%7f`, `%10.6f`), kept for provenance. |
| `decimals` | The number of decimals in `format`; provenance only, not an instruction to rescale (see below). |
| `group` | Which labelled selection in `make_layouts.R` kept the field. |
| `label` | The dictionary's variable label, re-encoded from Windows-1252 to UTF-8; labels in the K-8 dictionary are cut at about 31 characters. |

The reader ignores every column after `type`.

`long` fields are typed `d` rather than `i` because some of them hold a written decimal point: in the K-8 file `G7PBLK` (`long`, `%6f`, "percent of Blacks in class") reads `1.00`, which an integer column would refuse.

## Decimal formats

There are no implied decimals in either file, and nothing needs rescaling.

Every field whose format carries decimals (`%10.6f`, `%6.2f`) has the decimal point physically present in the data.
This was measured, not assumed: across the first 300 K-8 records, all 6,183 decimal-format fields in the full dictionary held a literal `.` in every non-missing value (`R1_KAGE` reads `69.07`); across the first 2,000 records of each file, none of the 423 (K-5) and 464 (K-8) decimal-format fields kept in the layouts had a non-missing value without a point (single-digit negative missing codes such as `-9` excepted).

## Missing values

A blank field is system-missing (typically a round the child was not in), and `readr` reads it as `NA`.
NCES reserved codes are negative: `-1` not applicable, `-7` refused, `-8` don't know, `-9` not ascertained, with value labels in the `.do` files (for example `label define _1789F` in `ECLSK_Kto8_child_STATA.do`).
Some items carry further codes, so check each item's value labels before mapping it.

## Which fields are kept

The files are very wide (26,060 and 18,949 fields), and most of the width is replicate weights and teacher-, school- and provider-level questionnaires that no dataschema row describes.
Each layout keeps the fields of the blocks listed in `k5_groups` and `k8_groups` in `make_layouts.R`; the `group` column names the block that kept each field.

ECLS-K (`eclsk2011_k5_child.csv`):

| Group | Fields | What it holds |
|-------|-------:|---------------|
| `ids` | 33 | `CHILDID`, `PARENTID`, `PSUID`, `CC_ID`, `TWIN_ID` and every school and teacher id (`*_ID`). |
| `design_weights` | 315 | The 105 full-sample weights (`W1C0`, `W12P0`, ...) and their Taylor-series stratum and PSU (`W1C0STR`, `W1C0PSU`, ...). |
| `composites` | 938 | Every `X` composite: sex, birth date, race and ethnicity, assessment ages, SES, parent education, income, poverty, household composition, home language, school type (`X*PUBPRI`) and locale (`X*LOCALE`), BMI, assessment scores (for example `X1RTHETK5`, reading theta) and their presence flags, and the teacher and parent Social Rating Scale scores. |
| `imputation_flags` | 138 | `IF*` flags for the imputed composites. |
| `field_management` | 144 | `F*` dispositions and interview dates. |
| `parent_interview` | 7,155 | Every parent-interview item (`P1`-`P9`), including all screen, media and rules items. |
| `child_assessment_and_questionnaire` | 1,424 | Every child item (`C1`-`C9`): measurements, assessment scores and the child questionnaire. |
| `teacher_child_level` | 809 | The child-level teacher questionnaire (`T1`-`T9`), including behaviour and academic ratings. |
| `subject_teacher_child_level` | 770 | The round 8-9 reading, mathematics and science teacher questionnaires (`G`, `M`, `N`), which carry child-level ratings. |
| `o7_items_level_unconfirmed` | 74 | The round-7 `O7` items (reading, mathematics and science skills such as "identify central message"); the dictionary does not say whether they rate the child or the class, so they are kept until that is known. |
| `media_items_other_blocks` | 145 | Any field in a dropped block whose label names a screen, device or media use (classroom TV and computer use, TV and computer time in child care). |

ECLS-K (`eclsk_k8_child.csv`):

| Group | Fields | What it holds |
|-------|-------:|---------------|
| `ids` | 33 | `CHILDID`, `PARENTID` and every school and teacher id (`*_ID`). |
| `design_weights` | 200 | The 71 full-sample weights (`C1CW0`, `BYCOMW0`, ...) and the 65 stratum and 64 PSU fields that the public-use file carries for them (`C1TCWSTR`, `C1TCWPSU`, ...). |
| `child_level_composites` | 73 | Fields named without a round prefix: `GENDER`, `RACE`, `DOBMM`/`DOBYY`, `CREGION`, `KURBAN_R`, the kindergarten `WK*` composites (parent education, SES, poverty, income, home language) and imputation flags. |
| `round_composites` | 152 | Round composites `W1`-`W8` (SES, income, race) and `R1`-`R7` (assessment ages, region, school changes). |
| `school_context_composites` | 72 | School-level composites in the school block that carry no questionnaire item number: public or private (`S*PUPRI`), school type, enrolment, percent minority and free-lunch eligibility. |
| `field_management` | 35 | `F*` dispositions and programme type. |
| `parent_interview` | 5,833 | Every parent-interview item (`P1`-`P7`), including all screen, media and rules items. |
| `child_assessment_and_questionnaire` | 647 | Every child item (`C1`-`C7`) except replicate weights: measurements, assessment scores, the Self-Description Questionnaire and the round-7 student questionnaire with its screen-time items. |
| `teacher_child_level` | 348 | The child-level teacher questionnaire (`T1`-`T5`), including the Social Rating Scale and Academic Rating Scale. |
| `subject_teacher_child_level` | 488 | The round 6-7 subject-teacher child-level questionnaires (`G`, `M`, `N`). |
| `student_records` | 331 | The student record abstracts (`U*`). |
| `media_items_other_blocks` | 176 | Any field in a dropped block whose label names a screen, device or media use. |

Left out, apart from the media items above:

- Replicate weights (7,760 in K-5, 40 or 80 per weight; 5,590 in K-8, 40 or 90 per weight): the dataschema carries one weight, stratum and cluster per row (`survey_weight`, `survey_strata`, `survey_cluster`) and no replicates, and each full-sample weight is kept with its Taylor-series stratum and PSU.
- Teacher-level classroom and teacher-background questionnaires (K-5 `A`, `B`; K-8 `A`, `B`, `J`): they describe the class or the teacher, not the child.
- School administrator questionnaires, school facility checklists and school fact sheets (K-5 `S`; K-8 `S` items with a question number, `K`, `L`): school resources and policies; the school composites a dataschema row could use are kept.
- Special-education teacher questionnaires (`D`, `E` in both files): services delivered to children with an IEP; disability and diagnosis come from the parent interview and the composites.
- Child-care provider and centre-director questionnaires (K-5 `R`, `V`, `Y`, `Z`, round 2 only): about the care setting; their TV and computer items are kept as media items.

A full read was not attempted.
The kept layouts read in a simulation of a multi-line reader (each record's lines joined, then `readr::read_fwf()` with the pipeline's own layout validator, column types and parse-problem check, in chunks of 2,000 records): all 18,174 K-5 records into 11,871 columns (the layout before the 74 `O7` fields were added) in 11.2 minutes at a peak resident memory of 3.0 GB, a 1.6 GB table, with no parse failures, and the 74 `O7` fields over all 18,174 records with none either; the K-8 file's 4,166 complete records into all 8,388 columns in 2.5 minutes at a peak of 0.8 GB, with no parse failures.
In both files `CHILDID` was present on every record and unique.
At 8 bytes a cell, the full K-5 file as numbers would be about 3.8 GB and the full K-8 file about 3.2 GB in memory before any parsing overhead, and every tidier run would reload them, which is why the layouts list a subset.

## How the layouts were produced

Run from the repository root:

```bash
Rscript harmonisation/datasets/BPIPD-775/layouts/make_layouts.R
```

The script reads the two `.dct` files from the study folder (read-only), parses each `_line()` and `_column()` statement, checks that every line is described gaplessly from column 1 and that the dictionary's line count matches `_lines()`, converts within-line columns to joined-record positions, applies the labelled selection, checks that the kept fields are uniquely named, ascending and non-overlapping, and writes the two CSVs here.
Re-run it if NCES replaces the dictionaries or the selection changes.
