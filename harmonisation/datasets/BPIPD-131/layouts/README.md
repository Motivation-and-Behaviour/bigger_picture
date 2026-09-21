# Fixed-width layouts for BPIPD-131 (NHES 2007)

`pfi_2007.csv` and `sr_2007.csv` give the column positions for `PFI07Asc.dat` and `SR07Asc.dat`, which are fixed-width files with no delimiters.
`dataset.yaml` points each data resource at its layout through `col_positions`, and the `fwf` reader validates and applies it.

| Column | Meaning |
|--------|---------|
| `name` | Variable name, as printed in the codebook. |
| `start`, `end` | 1-based, inclusive character positions. |
| `type` | `c` for codebook formats beginning with `C`, otherwise `d`. |
| `fmt` | The codebook's `Format` string, kept for provenance; ignored by the reader. |

Both layouts are gapless: PFI covers positions 1 to 2431 across 916 variables, SR covers 1 to 1790 across 640.
Records in both files are CRLF-terminated, so each line is two bytes longer than the layout.

## Implied decimals

Formats such as `N8.3` mean the field holds an integer with three implied decimal places.
The reader does not apply them; the tidier divides by `10^d` for every field whose `fmt` ends in `.d` with `d > 0`.
That is 82 PFI and 81 SR variables, almost all of them replicate weights.

## How the layouts were produced

The codebook PDFs in the study folder are machine-readable and regular.
Each variable's entry has `Variable Name:`, `Position:` and `Format:` lines that `pdftotext -layout` preserves.
The script below extracted both layouts and was run once from the repository root; re-run it if the codebooks are ever replaced.

```r
extract <- function(pdf) {
  txt <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)
  nm <- grep("^\\s*Variable Name\\s*:", txt)
  po <- grep("^\\s*Position\\s*:", txt)
  fm <- grep("^\\s*Format\\s*:", txt)
  stopifnot(length(nm) == length(po), length(nm) == length(fm))
  pos <- trimws(sub("^\\s*Position\\s*:\\s*", "", txt[po]))
  fmt <- trimws(sub("^\\s*Format\\s*:\\s*", "", txt[fm]))
  tibble::tibble(
    name = sub("^\\s*Variable Name\\s*:\\s*(\\S+).*$", "\\1", txt[nm]),
    start = as.integer(sub("-.*$", "", pos)),
    end = as.integer(sub("^.*-", "", pos)),
    type = ifelse(startsWith(fmt, "C"), "c", "d"),
    fmt = fmt
  )
}

for (f in c("PFI", "SR")) {
  d <- extract(sprintf("/data/BPIPD-131 - NHES/NHES_2007_%s_Codebook.pdf", f))
  stopifnot(
    !anyDuplicated(d$name),
    all(d$start[-1] == head(d$end, -1) + 1L) # gapless
  )
  readr::write_csv(
    d,
    sprintf("harmonisation/datasets/BPIPD-131/layouts/%s_2007.csv", tolower(f))
  )
}
```
