# W04: portable adjudication of unresolved screening decisions

For a **completed W04 consensus run** whose status is
`HUMAN_REVIEW_REQUIRED`, export only the unresolved publications. This
differs from `export_w04_colleague_review()`, which is designed for
independent validation of **all** screened records and is not automatically
accepted by `import_w04_review()`.

```r
# In a fresh R session, reinstall GitHub main:
remotes::install_github("nealhaddaway/evidencelandscaper",
  ref="main",dependencies=FALSE,upgrade="never",force=TRUE)
library(evidencelandscaper)

map <- open_map("Ostara_collective_action_map")
full10_w04 <- file.path(
  map,"runs","W01",
  "prepare-20261024T082400-filed39f726c-direct-de00ec5100c1",
  "W04_local",
  "selected-0d292ffe1eab-e26b461b7ab9-model-gpt-5.6-luna"
)

stopifnot(file.exists(file.path(full10_w04,"consensus","summary.json")))
summary<-jsonlite::fromJSON(file.path(full10_w04,"consensus","summary.json"))
print(summary[c("status","records_screened","final_unresolved","model")])

bundle_zip<-export_w04_adjudication_bundle(
  w04=full10_w04,
  destination_zip=file.path(path.expand("~/Downloads"),
                            "ostara_broad_10percent_W04_adjudication.zip")
)
print(bundle_zip)
```

## For the colleague

1. Unzip to a writable directory.
2. In RStudio install once: `install.packages(c("shiny","jsonlite","digest"))`.
3. Open `w04_adjudication_bundle/app.R` and click Run App,
   or use `shiny::runApp("/path/to/w04_adjudication_bundle")`.
4. Enter reviewer name. Work through unresolved records, reviewing the title,
   abstract, screening rationale, and model consensus.
5. Choose Retain / Exclude / Uncertain, add optional reasons and Save.
6. Export `w04_human_decisions.jsonl` and return it to the project lead.

The bundle includes the exact original undecided-record queue. Saved reviews
are stored as `decisions.jsonl` locally; they do not alter model results.

## Import and finalise after receiving the JSONL

```r
receipt <- import_w04_review(
  w04=full10_w04,
  decisions_file=file.choose(),
  include_all=FALSE
)
print(receipt)

final <- finalize_w04(
  map_path=map,
  w04=full10_w04,
  uncertain="adjudicate"
)
```

The importer validates the native decision schema, record IDs, queue checksum,
model prompt and input fingerprints. It requires a completed human judgment for
**every** queued case. Unresolved human decisions cannot be finalised until
resolved or an explicitly different policy is chosen.

**No changes** to W04 source consensus, first-pass provenance or archived
Luna/Sol voting ledgers are made by exporting or reviewing the ZIP.
