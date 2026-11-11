# DR-0007: The analysis runs in this repository's pipeline, on the data the pipeline holds

**Status:** Accepted  
**Date:** 2026-11-09  
**Decision makers:** Taren  
**Related issues:** NA

---

## Context

The analysis needs a home, and a way to say which data each result rests on.
Three layouts were considered: a separate analysis repository, a second `targets` project in this repository that reads versioned snapshots ("data cuts") of `analysis_data`, and a section of the existing pipeline downstream of `analysis_data`.

A second project with data cuts was first planned so that merging a dataset would not invalidate every model.
It also meant the analysis could fall out of step with the harmonised data, and it added a second project, store and run command to set up and maintain.

The code is public so that what was done can be scrutinised; analyses are not blinded, so the public history and a freeze before confirmatory models separate exploration from confirmation.

---

## Decision

The analysis is a section of `_targets.R`, downstream of `analysis_data`, with its functions in `R/analysis/` and its spec files in `analysis/`.
There are no data snapshots.
A merge that changes the included data invalidates what depends on it.

Which data a result rests on is recorded rather than copied: an inclusion step selects the datasets the analysis uses, and a data manifest (dataset IDs, row counts and content hashes, never names) is recorded for each delivered report and at the freeze.

The report to the funder is the one exception to public sources: its Quarto sources live in a separate private repository, cloned or linked into the project as `DoEreport/`.
The pipeline renders it only where that folder is present, and every table, figure and model it shows is a public target.

---

## Rationale

- Results always describe the data in the pipeline, with no snapshot to fall out of step with it.
- Only the parts of the analysis a change touches rerun: each analysis target reads a slice of the shared table scoped to what it needs, and a slice whose content is unchanged leaves everything downstream of it alone.
  Tested on 9 November: with outcome slices that kept `analysis_data`'s factor levels, a dataset feeding one outcome reran every outcome's model; with `droplevels()`, only its own.
- One project and one run command keep a regular clone simple to set up and maintain.
- Public code and specs let readers check the analysis; the private report holds only prose and formatting.

---

## Alternatives considered

- A separate analysis repository: rejected, because it duplicates the setup and separates the analysis from the harmonisation it depends on.
- A second `targets` project reading versioned data cuts: rejected, because manual cuts let the analysis drift from the included data and add a project to maintain; its benefit, limited invalidation, comes from slicing instead.

---

## Consequences

### Positive consequences

- Every build brings the analysis up to date with the merged data.
- A regular clone needs only `targets::tar_make()`; public clones and CI build everything except the private report.

### Negative consequences or trade-offs

- Every `tar_make()` also builds the parts of the analysis a change touches; `targets::tar_make(analysis_data)` builds the harmonisation part alone.
- `tar_outdated()` lists the whole analysis as outdated after any data change, until a build has run.
- Every slice of a shared table must end with `droplevels()`, because `factorise_analysis_data()` takes factor levels from the data (DR-0006).
- A delivered report cannot be regenerated later from the live pipeline, so its rendered copy and recorded manifest are kept.

---

## Implementation

```md
_targets.R                               analysis section after analysis_data
R/analysis/                              analysis functions
analysis/                                registries and report sources (see analysis/README.md)
R/analysis/private_report_targets.R      adds the private report's targets when DoEreport/ exists
```

---

## Related decisions

```md
DR-0006: analysis_data is stored as Parquet with categorical columns as factors
DR-0008: The outcome registry
```
