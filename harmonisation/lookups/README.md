# Shared Lookup Tables

This folder stores lookup tables that are intended to be reviewed and reused
across studies.

Use shared lookup tables for recurring recodes such as ethnicity, where audit
clarity matters more than keeping each mapping inline in one study's
`variables.csv`.

Keep short study-specific recodes inline in
`harmonisation/datasets/BPIPD-<id>/variables.csv`.
Use `harmonisation/datasets/BPIPD-<id>/lookups/` only for large one-off
dataset-specific mappings that are not shared across studies.

Each shared lookup CSV must include these columns:

- `dataset_id`
- `target_variable`
- `source_value`
- `harmonised_value`
- `mapping_status`
- `notes`

Only lookup files named in a dataset's `lookup_table` column are tracked and loaded for that dataset's harmonisation step.

Recommended usage from `variables.csv`:

```r
lookup_values(
  source_vector,
  lookup_ethnicity,
  dataset_id = spec$dataset_id,
  target_variable = "ethnicity"
)
```

The `lookup_table` column in `variables.csv` should contain the basename of the
shared lookup file. For example, `harmonisation/lookups/ethnicity.csv` is
available in harmonisation expressions as `lookup_ethnicity`.
