# Fixed-width layout for BPIPD-1928 (PeNSE 2009)

`pense_2009.csv` gives the column positions for `PeNSE_2009/Dados/Base_Pense Completa_2009.txt`, the only PeNSE edition delivered as a fixed-width file.
`dataset.yaml` points the 2009 data resource at it through `col_positions`, and the `fwf` reader validates and applies it.

| Column | Meaning |
|--------|---------|
| `name` | Variable name (`CAMPO` in the dictionary). |
| `start`, `end` | 1-based, inclusive character positions (`INICIO`, `TÉRMINO`). |
| `type` | `d` for numeric fields (`TIPO` not beginning with `CARACTER`) that never hold the `.` missing marker; `c` for everything else. |
| `decimals` | The dictionary's `DECIMAIS`, kept for provenance; ignored by the reader, and not an instruction to rescale (see below). |
| `tipo` | The dictionary's `TIPO`, kept for provenance; ignored by the reader. |

The layout is gapless over positions 1 to 311 across 187 fields, and the dictionary describes every character of the record.
Each record is 311 characters followed by CRLF, so byte 312 is the carriage return rather than an undescribed field: the file is 19,847,643 bytes, which is exactly 63,411 records of 313 bytes, and the last five bytes of the first record are `2 2 2 \r \n`.

## Missing values

The file writes `.` for a missing value in every kind of field, character and numeric alike.
Typing a numeric field that contains `.` as `d` would read correctly but raise a parse warning on every build, so the 59 numeric fields that hold `.` are read as character instead.
The tidier recodes `.` to `NA` across the table and converts those fields to numeric.

## Decimal formats

There are no implied decimals in this file, and nothing needs rescaling.

Seven fields carry `decimals > 0`, but the decimal point is physically present in the data, so `readr::read_fwf()` parses them onto their natural scale.
Neither the reader, the tidier nor `variables.csv` should divide by `10^decimals`.

This was measured, not assumed.
Across all 63,411 records, every non-missing value of all seven fields contains a literal `.`: `Q2B01P01` reads `52.90`, `PESO_ESCOLA` reads `2.4571428571`, `PROBSELTURMA` reads `.500000000`.
Dividing `PESO_ESCOLA` by `10^10` would turn a school weight of 2.46 into 0.000000000246.

The `decimals` column is therefore provenance from the dictionary, not an instruction.

## How the layout was produced

The dictionary is the `DICIONARIO` sheet of the `Dicionario_Base Pense_Completa.xls` workbook that the spec already declares as a codebook resource.
The script below converted it once and was run from the repository root; re-run it if the dictionary is ever replaced.

```r
dict <- readxl::read_excel(
  file.path(
    "/data/BPIPD-1928 - Brazilian National Health Survey/PeNSE_2009",
    "Documentaç╞o/Dicionario_Base Pense_Completa.xls"
  ),
  sheet = "DICIONARIO",
  .name_repair = "minimal"
)
d <- tibble::tibble(
  name = trimws(dict$CAMPO),
  start = as.integer(dict$INICIO),
  end = as.integer(dict[["TÉRMINO"]]),
  type = "c",
  decimals = as.integer(dict$DECIMAIS),
  tipo = trimws(dict$TIPO)
)

# Read once as character to see which numeric fields carry the `.` marker
raw <- readr::read_fwf(
  file.path(
    "/data/BPIPD-1928 - Brazilian National Health Survey/PeNSE_2009",
    "Dados/Base_Pense Completa_2009.txt"
  ),
  col_positions = readr::fwf_positions(d$start, d$end, d$name),
  col_types = readr::cols(.default = "c"),
  show_col_types = FALSE
)
has_dot <- vapply(raw, function(x) any(x == ".", na.rm = TRUE), logical(1))
numeric_field <- !startsWith(toupper(d$tipo), "CARACTER")
d$type <- ifelse(numeric_field & !has_dot[d$name], "d", "c")

stopifnot(
  !anyDuplicated(d$name),
  all(d$start[-1] == head(d$end, -1) + 1L) # gapless
)
readr::write_csv(
  d,
  "harmonisation/datasets/BPIPD-1928/layouts/pense_2009.csv",
  na = ""
)
```
