# Fixed-width layout for BPIPD-1928 (PeNSE 2009)

`pense_2009.csv` gives the column positions for `PeNSE_2009/Dados/Base_Pense Completa_2009.txt`, the only PeNSE edition delivered as a fixed-width file.
`dataset.yaml` points the 2009 data resource at it through `col_positions`, and the `fwf` reader validates and applies it.

| Column | Meaning |
|--------|---------|
| `name` | Variable name (`CAMPO` in the dictionary). |
| `start`, `end` | 1-based, inclusive character positions (`INICIO`, `TÉRMINO`). |
| `type` | `d` for numeric fields (`TIPO` not beginning with `CARACTER`) that never hold the `.` missing marker; `c` for everything else. |
| `decimals` | The dictionary's `DECIMAIS`, kept for the tidier; ignored by the reader. |
| `tipo` | The dictionary's `TIPO`, kept for provenance; ignored by the reader. |

The layout is gapless over positions 1 to 311 across 187 fields.
Every record is 312 characters plus CRLF; position 312 is a space on every record checked and is not described by the dictionary, so it is left unread.

## Missing values

The file writes `.` for a missing value in every kind of field, character and numeric alike.
Typing a numeric field that contains `.` as `d` would read correctly but raise a parse warning on every build, so the 59 numeric fields that hold `.` are read as character instead.
The tidier recodes `.` to `NA` across the table and converts those fields to numeric.

## Implied decimals

Seven fields have `decimals > 0`, meaning the field holds an integer with that many implied decimal places.
The reader does not apply them; the tidier divides by `10^decimals` for those fields.

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
