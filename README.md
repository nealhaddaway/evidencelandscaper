# evidencelandscaper

An R package for reproducible evidence maps, with review data stored locally
and separately from package source code. Under active development.

## Local storage

Choose an **existing** directory on your computer once (for example, an
offline-available Google Drive for desktop folder):

```r
library(evidencelandscaper)
set_map_root("~/Google Drive/EvidenceMaps")  # replace with your actual path
map_root()
map <- create_map("example_review")
```

The preference is stored in your user-specific R configuration; neither the
package repository nor the installed package contains your review data.

## W00 planning (offline)

`plan_w00()` executes the **unchanged** upstream R scripts for source
selection, strategy validation, topic-agnostic query translation, and
SHA-256 query archiving. It makes no database/API calls.

Prepare a JSON search strategy (named concepts and Boolean expression or
advanced_boolean), following the upstream W00 configuration contract.
A simple example is bundled as `system.file("w00",
"example-strategy.json", package = "evidencelandscaper")`.

```r
strategy <- system.file("w00", "example-strategy.json",
                        package = "evidencelandscaper")
plan <- plan_w00(map, strategy = strategy, mode = "full",
                 sources = c("lens", "openalex"))
plan$search_plan
```

This produces a local run directory containing source selection,
translated queries, immutable query archive and checksums.
Fortnightly/expansion modes require an earlier full-run source selection
and their respective strategy inputs.

**Not yet supported:** fetching records through the database APIs, W01
handover, or the separate scoping workflow. No Zenodo deposits are made
during local processing. A single curated Zenodo record per review is
planned only for final archival.

## Provenance

Planning scripts and source catalogue were copied without changes from
`thesalmonandthetomato/LivingEvidenceMap` **main**, audited at commit
`fe0fbf4dfb6aa5313abc99877c32726ce3f0b8c2` (22 October 2026).
This project follows only the GitHub Actions execution pathway, not the
Shiny execution pathway. Upstream production code remains untouched.
The source repository's `.github/workflows/workflow_00_search_orchestrator.yml`
and `_workflow_00_orchestrated_source_child.yml` define the behaviour
to reproduce in subsequent work.

Use `devtools::test()` and `devtools::check()` locally to verify package
behaviour. Runtime tests have not yet been executed in this environment.


## Unified bounded W00 retrieval

After creating a fresh `plan_w00()` result containing only `lens` and/or
`openalex`, run:

```r
result <- run_w00(plan, max_records = 10L)
jsonlite::fromJSON(result$manifest)
```

This runs the existing source adapters sequentially and writes
`w00_retrieval_manifest.json` in the map's W00 run directory.
The parent refuses unported sources, preflights credentials, retains existing
outputs, and stops on any failed source. It does **not** perform a full-corpus
harvest or provide a W01 handover. Create a new plan for each new retrieval:
completed or failed source directories are not overwritten.

The user must explicitly call `run_w00()`; planning itself makes no API calls.
