# DR-0006: `analysis_data` is stored as Parquet with categorical columns as factors

**Status:** Proposed  
**Date:** 2026-10-20  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

`analysis_data` is the row-bound table of every harmonised dataset: millions of rows and several hundred columns, at one row per participant, wave and screen-time measure (DR-0003).
It was stored in the pipeline's default `qs` format, with every categorical variable as a character column.
Loading it took about a minute, and every query had to load the whole table first.

The file container was not the bottleneck.
Loading spent most of its time creating one R string for every cell of every character column, close to a billion strings in all.

The dataschema lists `allowed_values` for many categorical variables, but nothing validates them and they are not kept in step with the mappings, so they cannot be relied on to define each variable's categories.

---

## Decision

After the harmonised tibbles are row-bound, `factorise_analysis_data()` converts each character column with at most 1,000 distinct non-missing values to a factor.
Character columns with more distinct values stay character; in practice these are identifiers such as `participant_id`, `survey_cluster` and `survey_strata`.
A factor's levels are the values observed in the data, in byte (C-locale) order.
`allowed_values` plays no part.

The `analysis_data` target is stored with `format = "parquet"`.
Every other target keeps the pipeline default (`qs`).

---

## Rationale

- A factor stores each distinct value once, so loading creates a few thousand strings instead of hundreds of millions.
  This gain holds in any storage format.
- Parquet reads use multiple threads, can read only the columns asked for, and support lazy queries through `arrow::open_dataset()`.
  It gave the smallest file of the formats compared, and stores factors as dictionary columns that keep their levels and level order.
- Choosing columns by their number of distinct values keeps the rule data-driven: no list of columns or categories has to be maintained alongside the mappings.
  Categorical columns have at most a few hundred distinct values, and identifier columns have tens of thousands or more, and the threshold of 1,000 sits in that gap.
  A small dictionary also stays well inside Parquet's dictionary page limit, which is what preserves level order when the file is written.
- Byte-order levels are the same on every machine, whatever its locale.
- The other targets gain little or cannot use Parquet: the raw reads are nested lists that only `qs` or `rds` can store, the tidied tables carry `haven_labelled` and `hms` classes, and each harmonised table is read only once, by the combine step.

---

## Alternatives considered

- Keep `qs` with character columns – rejected: slow to load, and every query loads the whole table.
- `fst` – rejected: files about 13 times larger than Parquet, and it drops column attributes.
- Feather (lz4) – rejected: read speed similar to Parquet, but files about 7 times larger.
- `nanoparquet` – rejected: slower when reading a few columns, and `arrow` (already a dependency) also gives lazy queries.
- Choose the columns from the dataschema's `character` variables and take levels from `allowed_values` – rejected: `allowed_values` is not maintained, so levels would follow an out-of-date list, values missing from it would need special handling, and a hand-kept list of identifier columns would be needed.
- `factor(levels = allowed_values)` – rejected: values missing from the list would silently become `NA`.
- Convert to factors per dataset inside `harmonise_from_spec()` – rejected: each dataset would get its own levels, which the combine step would then have to reconcile.

---

## Consequences

### Positive consequences

- `targets::tar_read(analysis_data)` loads in seconds rather than about a minute.
- Analyses can read only the columns they need, or filter and summarise lazily before loading anything.
- The stored file is smaller.

### Negative consequences or trade-offs

- Categorical variables arrive as factors, not character:
  - comparisons with strings (`sex == "Female"`, `%in%`) work as before;
  - `as.numeric()` on a factor returns level codes, not values, so use `as.numeric(as.character(x))`;
  - levels list every value seen in any dataset, so a subset carries empty levels until `droplevels()` is applied;
  - `dplyr::if_else()` and `dplyr::case_when()` that mix a factor with strings need `as.character()` first.
- `targets::tar_read(analysis_data)` returns lazily converted columns: `arrow` turns each column into an ordinary R vector only when it is first used.
  Selecting columns first keeps this cheap, but an operation over every column of the full table is about three times slower than on ordinary vectors.
  Setting `options(arrow.use_altrep = FALSE)` before reading converts everything up front, at the cost of a transient peak of about twice the table's memory.
- Building `analysis_data` takes slightly longer and needs more memory while it runs, for the factor conversion; the faster Parquet write makes up most of the extra time.
- Models use the first level as the reference, which in byte order puts `FALSE` before `TRUE` and `No` before `Yes`, and upper case before lower case; relevel where another reference is wanted.
- The set of factor columns depends on the data: a categorical column that grows past 1,000 distinct values, or an identifier in a build with very few rows (such as a development build with one small dataset), changes type.
  Revisit the threshold if a categorical variable approaches it.
- Reading `analysis_data` needs `arrow`, which is already a pipeline dependency.

---

## Implementation

```md
R/harmonise/factorise_analysis_data.R
_targets.R: the analysis_data target (tar_combine command and format = "parquet")
tests/testthat/test-factorise-analysis-data.R
docs/guides/harmonisation.qmd: "The combined analysis table"
```

---

## Related decisions

```md
DR-0003: One harmonised row per participant, wave and screen-time measure
```
