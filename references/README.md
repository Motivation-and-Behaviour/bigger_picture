# Dataset references

The citations and required acknowledgements for each dataset, as a committed snapshot.
Every file here is generated: never edit one by hand, because the next sync overwrites it.

| File | One row per | Comes from |
| --- | --- | --- |
| `datasets.bib` | Reference (a Better BibLaTeX entry) | The project's Zotero collection, exported by Better BibTeX |
| `dataset_citations.csv` | Dataset and citation (`dataset_id`, `citekey`) | Airtable: each Datasets record's `Citations` links and their `Citation Key` |
| `dataset_acknowledgements.csv` | Dataset with a required acknowledgement (`dataset_id`, `acknowledgement`) | Airtable: the Datasets record's `Required Acknowledgements` |

`dataset_id` is the repository form, digits only (`9999`, not `BPIPD-9999`), so it joins with `analysis_data`.
Only datasets whose Airtable status is `Included` are listed.

## Refreshing

1. Add the reference to the Zotero collection, then link its citation key to the dataset in Airtable.
2. Run the sync from the project root:

   ```bash
   Rscript -e 'targets::tar_source(); sync_dataset_references()'
   ```

3. Review the diff and commit the changed files in a pull request.

The sync reads `AIRTABLE_TOKEN` and `AIRTABLE_BASE_ID` from `.env` (see `.env.example`).
If `BP_BIB_SOURCE` is set, it copies that Better BibTeX export into `datasets.bib` first; leave it unset when Better BibTeX exports straight into this folder.
It requests only the fields named in `bp_airtable_reference_fields()` (`R/config.R`), so nothing else in the base is ever fetched.
If any citation key is missing from the bib, it stops before writing anything and lists the keys and their datasets.
It ends with a summary of the pipeline's datasets that still have no citation.

## In the pipeline

`_targets.R` tracks the three files as `dataset_bib_file`, `dataset_citations_file` and `dataset_acknowledgements_file`, and reads them into `dataset_citations` and `dataset_acknowledgements`.
`read_dataset_citations()` fails if a key is not in `datasets.bib`.
No dataset target depends on these, so a citation change never rebuilds a dataset.
The pipeline, the tests and CI never call Airtable.

Tables should carry citation keys rather than formatted citations: a `knitr::kable()` cell holding `[@key1; @key2]` is formatted by pandoc when a Quarto document lists `datasets.bib` in its `bibliography`, in whatever citation style the document uses.

`bp_citation_required_statuses()` (`R/config.R`) names the spec statuses whose datasets must have a citation; it is empty until the backfill is done, and then the tests fail for any such dataset without one.

## What belongs here

This repository is public.
Only citation keys, references and acknowledgement text that will be printed belong here; contacts, data sharing agreements and data conditions stay in Airtable.
The public docs site does not show these files.
