# Fixed-width layouts for BPIPD-46 (PSID-CDS)

Every data file in the PSID-CDS study folder is a headerless fixed-width ASCII `.txt` file, shipped with an SPSS (`.sps`), Stata (`.do`) and SAS (`.sas`) setup file and a `<stem>_codebook.pdf` of the same stem.
There is no self-describing (`.sav`, `.dta`) copy of any file, so `dataset.yaml` reads every data resource with the `fwf` reader and points its `col_positions` at one of the CSVs here.
Each CSV is named after the resource that uses it, so `demog_1997.csv` is the layout of the `demog_1997` resource, which reads `1997/DEMOG1997.txt`.

## How the layouts were produced

`generate_layouts.R` writes all 68 layouts from the study's own setup files.
Run it from the repository root; it reads the study folder read-only and writes only into this folder:

```sh
Rscript harmonisation/datasets/BPIPD-46/layouts/generate_layouts.R
```

The list of files at the top of the script, `declared`, is the list of `fwf` resources in `dataset.yaml`; add a file to both when declaring a new one.
Re-run the script if the study folder is ever replaced by a new PSID release.

Positions come from the `DATA LIST ... FIXED` block of the SPSS setup file, one `NAME start - end` entry per field, with `(A)` marking a string field.
Parsing stops at the line holding only `.` that closes that block, because the `VARIABLE LABELS` block after it can contain text that looks like a field (it does in `PCGHH2019.sps`).
Decimal formats come from the SPSS `FORMATS` block (`Fw.d`), and labels from the `VARIABLE LABELS` block.

## Columns

| Column | Meaning |
|--------|---------|
| `name` | Variable name, as in the setup files and codebooks. |
| `start`, `end` | 1-based, inclusive character positions. |
| `type` | `d` for numeric fields, `c` for the nine string fields (the NCES school IDs in the 2014, 2019 and 2021 PCG-child files). |
| `decimals` | The number of decimals in the field's SPSS format (`F7.3` gives 3); 0 when the setup file gives none. Provenance only: see below. |
| `decimal_point` | For fields with `decimals > 0`, what the data hold: `present` when every non-blank value contains a literal `.`. Empty for other fields. |
| `label` | The variable label from the SPSS setup file, whitespace collapsed, for the tidier and the mapping. |

The `fwf` reader uses `name`, `start`, `end` and `type` and ignores the rest.

## What the script checks before writing a layout

The script stops, and writes nothing for that file, unless all of these hold:

- the number of fields equals the `Columns` count in the setup-file header;
- names are unique, the first field starts at 1, fields are ascending and gapless, and the last field ends at the `LRECL` in the `FILE HANDLE` line, so the layout describes every byte of the record;
- the Stata `infix` block and the SAS `INPUT` block give exactly the same names, positions and string fields;
- the SAS `ATTRIB` formats name every field and give the same decimals and string formats as the SPSS setup file;
- every field has a label;
- the data file has exactly `Rows` records (the header count), each exactly `LRECL` bytes, terminated by CRLF, and the file size is `Rows x (LRECL + 2)` bytes;
- every non-blank value of every numeric field is a number (a field that failed would have been typed `c`; none did).

All 68 layouts passed every check.
Because each record is `LRECL` bytes followed by CR and LF, the reader's record-width check (which reads the first line with the line end removed) sees exactly `LRECL` bytes, the end of the last field.

## Decimal formats

There are no implied decimals in these files, and nothing needs rescaling.

606 fields across 36 files carry a decimal format (weights, ages in months, hours, some scores), and in every one of them every non-blank value contains a literal decimal point, so the `fwf` reader parses them onto their natural scale.
For example, the first record of `1997/DEMOG1997.txt` holds `132.1` for `AGEATPCG` (format `F5.1`, age in months) and `  5.766` for `CH97PRWT` (`F7.3`, the child weight).
Neither the tidier nor `variables.csv` should divide by `10^decimals`; the `decimals` column is provenance from the setup file, and `decimal_point` records the measurement behind this statement for each field.

## Missing values

No field is blank on any record: the read of 2026-11-08 left no `NA` in any of the 68 tables.
The codebooks' missing-data and Inap. codes (such as 0, 9, 99, 98 or 999.9, which differ by item) are ordinary numbers in the data, so the tidier or the mapping must recode them item by item from the codebook.

## Files

The 68 layouts hold 19,445 fields; `Rows` and `LRECL` are the setup-file header values, which the script checked against each data file, and paths are relative to the study folder.

|Layout                     |Data file               |   Rows| Fields| LRECL| Decimal fields|
|:--------------------------|:-----------------------|------:|------:|-----:|--------------:|
|`cdsind_2021.csv`          |`2021/CDSIND2021.txt`   |  19686|    101|   259|              0|
|`demog_1997.csv`           |`1997/DEMOG1997.txt`    |   3563|     27|    84|              8|
|`id_map_1997.csv`          |`1997/IDMAP97.txt`      |   2233|      6|    19|              0|
|`pcg_child_1997.csv`       |`1997/PCG97_CHLD.txt`   |   3563|    806|  1301|             87|
|`pcg_hh_1997.csv`          |`1997/PCG97_HH.txt`     |   1536|    311|   545|              4|
|`child_1997.csv`           |`1997/CHILD97.txt`      |   2223|    339|   398|              3|
|`td_agg_1997.csv`          |`1997/TD97_ACT_AGG.txt` |   2904|    573|  2846|              3|
|`td_media_1997.csv`        |`1997/TD97MEDIA.txt`    |  14415|     16|    36|              0|
|`td_activity_1997.csv`     |`1997/TD97.txt`         | 131060|     34|    56|              0|
|`td_followup_1997.csv`     |`1997/TDFUP97.txt`      |   2904|     29|    54|              0|
|`teacher_ems_1997.csv`     |`1997/EMSTEACH97.txt`   |   1108|    261|   454|              4|
|`teacher_pd_1997.csv`      |`1997/PDTEACH97.txt`    |    172|    180|   384|              7|
|`school_diary_es_1997.csv` |`1997/ESDIARY97.txt`    |  11459|     81|   103|              0|
|`school_diary_ms_1997.csv` |`1997/MSDIARY97.txt`    |   3602|     63|    77|              0|
|`school_diary_pd_1997.csv` |`1997/PSDIARY97.txt`    |   1864|     81|    95|              0|
|`demog_2002.csv`           |`2002/DEMOG.txt`        |   2907|     22|    39|              1|
|`id_map_2002.csv`          |`2002/IDMAP02.txt`      |   2891|      8|    26|              0|
|`pcg_child_2002.csv`       |`2002/PCG_CHLD.txt`     |   2907|    921|  1562|             76|
|`pcg_hh_2002.csv`          |`2002/PCG_HHLD.txt`     |   2009|    218|   257|              4|
|`child_2002.csv`           |`2002/CHILD.txt`        |   2182|    508|   795|             43|
|`assess_2002.csv`          |`2002/ASSESSMT.txt`     |   2644|    256|   326|              4|
|`td_agg_2002.csv`          |`2002/TD02_ACT_AGG.txt` |   2569|   1311|  6536|              3|
|`td_media_2002.csv`        |`2002/TD02MEDIA.txt`    |  11975|     16|    37|              0|
|`td_activity_2002.csv`     |`2002/TD_ACTIVITY.txt`  |  99467|     34|    58|              0|
|`td_followup_2002.csv`     |`2002/TD_FOLLOWUP.txt`  |   2569|     75|   210|              0|
|`teacher_ems_2002.csv`     |`2002/EMSTEACH.txt`     |    691|    153|   221|              0|
|`demog_2007.csv`           |`2007/DEMOG07.txt`      |   1623|     18|    39|              2|
|`id_map_2007.csv`          |`2007/IDMAP07.txt`      |   1608|      8|    26|              0|
|`pcg_child_2007.csv`       |`2007/PCG_CHILD07.txt`  |   1608|    616|  1030|             43|
|`pcg_hh_2007.csv`          |`2007/PCG_HH07.txt`     |   1250|    255|   344|              6|
|`child_2007.csv`           |`2007/CHILD07.txt`      |   1506|    557|   927|             36|
|`assess_2007.csv`          |`2007/ASSESS07.txt`     |   1506|    255|   337|             10|
|`td_agg_2007.csv`          |`2007/TD_ACTAGG07.txt`  |   1442|   1327|  6616|              3|
|`td_media_2007.csv`        |`2007/TD07MEDIA.txt`    |   6188|     16|    37|              0|
|`td_activity_2007.csv`     |`2007/TD_ACT07.txt`     |  57813|     35|    61|              0|
|`td_followup_2007.csv`     |`2007/TD_FOLLOW07.txt`  |   1442|     43|    78|              0|
|`demog_2014.csv`           |`2014/DEMOG14.txt`      |   4333|    208|   304|              4|
|`id_map_2014.csv`          |`2014/IDMAP14.txt`      |   4379|      7|    22|              0|
|`hh_roster_2014.csv`       |`2014/HHROSTER14.txt`   |  10423|     11|    23|              0|
|`pcg_child_2014.csv`       |`2014/PCGCHILD14.txt`   |   4314|    539|   834|             33|
|`pcg_hh_2014.csv`          |`2014/PCGHH14.txt`      |   2517|    522|   793|             27|
|`child_2014.csv`           |`2014/CHILD14.txt`      |   1508|    381|   560|             14|
|`assess_2014.csv`          |`2014/ASSESS14.txt`     |   1498|    201|   247|              0|
|`td_agg_2014.csv`          |`2014/TD_ACTAGG14.txt`  |   1566|   1287|  6416|              3|
|`td_media_2014.csv`        |`2014/TD14MEDIA.txt`    |   5454|     17|    38|              0|
|`td_activity_2014.csv`     |`2014/TD_ACT14.txt`     |  60606|     41|    69|              0|
|`td_followup_2014.csv`     |`2014/TD_FOLLOW14.txt`  |   1566|     61|   114|              0|
|`demog_2019.csv`           |`2019/DEMOG2019.txt`    |   4629|    179|   302|              7|
|`hh_roster_2019.csv`       |`2019/HHROSTER2019.txt` |  10361|     15|    34|              0|
|`pcg_child_2019.csv`       |`2019/PCGCHILD2019.txt` |   4615|    608|   976|             33|
|`pcg_hh_2019.csv`          |`2019/PCGHH2019.txt`    |   2616|    616|   911|             33|
|`child_2019.csv`           |`2019/CHILD2019.txt`    |   1583|    391|   592|             14|
|`assess_2019.csv`          |`2019/ASSESS2019.txt`   |   1254|    333|   398|              6|
|`td_agg_2019.csv`          |`2019/TD_AGG2019.txt`   |   1541|   1289|  6426|              3|
|`td_media_2019.csv`        |`2019/TD19MEDIA.txt`    |   4692|     17|    38|              0|
|`td_activity_2019.csv`     |`2019/TD_ACT2019.txt`   |  61314|     38|    62|              0|
|`td_followup_2019.csv`     |`2019/TD_QN2019.txt`    |   1541|     62|   115|              0|
|`pcg_child_2020.csv`       |`2020/PCGCHILD2020.txt` |   1997|     61|    76|              0|
|`pcg_hh_2020.csv`          |`2020/PCGHH2020.txt`    |   1153|     89|   133|              5|
|`td_agg_2020.csv`          |`2020/TD_AGG2020.txt`   |    489|   1289|  6426|              3|
|`td_media_2020.csv`        |`2020/TD20MEDIA.txt`    |   1364|     17|    38|              0|
|`td_activity_2020.csv`     |`2020/TD_ACT2020.txt`   |  18956|     38|    62|              0|
|`td_followup_2020.csv`     |`2020/TD_QN2020.txt`    |    489|     62|   115|              0|
|`demog_2021.csv`           |`2021/DEMOG2021.txt`    |   2590|    163|   247|              2|
|`hh_roster_2021.csv`       |`2021/HHROSTER2021.txt` |   6201|     14|    33|              0|
|`pcg_child_2021.csv`       |`2021/PCGCHILD2021.txt` |   2590|    527|   852|             32|
|`pcg_hh_2021.csv`          |`2021/PCGHH2021.txt`    |   1576|    410|   642|             27|
|`child_2021.csv`           |`2021/CHILD2021.txt`    |    749|    392|   595|             13|
