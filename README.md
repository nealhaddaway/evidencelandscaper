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

After creating a fresh `plan_w00()` result containing supported automated sources (`lens`, `openalex`, `agricola`,
`pubmed`, `ethos`, or `epmc_preprints`), run:

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

CAB Abstracts (`cba`) and ProQuest Dissertations & Theses (`pqdt`) are **manual RIS sources**. The W00 parent explicitly refuses automatic retrieval for either source. A local, provenance-preserving RIS import and W01 handover are separate pending steps; do not substitute Europe PMC's CBA collection for the licensed CAB Abstracts database.


## Manual RIS imports

The same local ingester handles any RIS-exporting database, including CAB
Abstracts and ProQuest Dissertations & Theses:

```r
ris_dir <- import_ris(
  map = open_map("example_review"),
  files = "/path/to/export.ris",
  source = "cba",
  database = "CAB Abstracts",
  search_date = "2026-10-22",
  search_string = "the actual database-specific query",
  keyword_semantics = "unknown"
)
```

Pass a vector of files for a multi-file export. Each import has its own
immutable output directory under `runs/W00/manual/`; it contains the
original RIS files, search metadata, SHA-256 checksums, duplicate-source-ID
audits, a manifest and `handoff/records.jsonl`. The existing upstream RIS
parser is reused; only its hardcoded Zenodo provenance description was
replaced with the local raw-file location. Conflicting source IDs cause
the import to fail instead of silently choosing records. RIS import does not
publish to Zenodo or initiate W01. The generated handoff still needs
integration and validation against the future consolidated W00 dataset.


## Accepting manual imports for a specific W00 run

Manual imports remain outside individual W00 search runs and are **not**
automatically included. Select exactly one validated import directory per
source with `accept_w00_ris(plan, imports)`, where `imports` is a named vector
(e.g. `c(cba = cab_dir, pqdt = pqdt_dir)`). This writes an immutable
`accepted_manual_imports.json` to that W00 run, recording the accepted source,
import location, record count, and manifest/handoff SHA-256 checksums.
The function independently verifies archived raw RIS and handoff checksums
and rejects source mismatches or repeated source IDs in the selection.
A mistaken previous import is excluded simply by not selecting it.
It does not alter either import or automatically execute W01.
To change an accepted selection, prepare a new W00 run rather than rewriting
a historical acceptance record.

By default, `accept_w00_ris(plan)` automatically discovers the latest successful local CAB (`cba`) and PQD&T (`pqdt`) import. With multiple successful imports for a source it selects the newest and reports which was selected. All selected import files are hash-validated. Pass `sources=` for another set of database identifiers or an explicit named `imports=` vector to override automatic selection. This does not silently accept older imports.

## EBSCOhost W00 sources

The 17 configured EBSCO databases use the unchanged upstream EBSCO EIT R ingestion script via the local `run_w00_ebsco()` adapter, or the unified `run_w00()` parent. Provide `EBSCO_EHOST_UID` and `EBSCO_EHOST_PWD` in your user `.Renviron`. EBSCO database entitlements and configured search fields are checked against the live EIT Info response. Each selected EBSCO database has separate `sources/<source-id>/` output and provenance. Only bounded 1–100-record retrieval is enabled; this adapter has not yet been runtime-tested in RStudio.

## Local W00 consolidation

`consolidate_w00(plan)` reads the **specified** run's successful automated outputs and its `accepted_manual_imports.json` (if present), rechecks the manual import checksums and source record counts, and writes `consolidated/records.jsonl` and `consolidated/manifest.json` without changing any original files. The output contains source-labelled original-record envelopes, **not a W01-ready canonical schema**. This is deliberate: downstream W01 format mapping and validation are separate work. A run with accepted RIS only can be consolidated; historical manual imports are never auto-included. The function refuses overwriting a previous consolidation.

For convenience, `consolidate_w00("example_review")` discovers the map's most recent run with accepted manual RIS imports (or, if none exist, the latest run with retrieval results). The selected run ID is printed. Supply an explicit `plan_w00()` object to target another run. Neither method overwrites a completed consolidation.

## Review-local scoping

Put the strategy JSON in `<map>/inputs/search/`. With one JSON it is automatically selected; with several versions, set `inputs/search/active_strategy.txt` to the selected JSON filename (one line). `plan_w00("map_name")` now defaults to all configured **automated** databases including Scopus, Web of Science and all 17 EBSCO sources. `scope_w00("map_name")` creates a new immutable plan and runs count-only searches against all these sources, saving counts locally. CAB Abstracts and PQD&T remain manual RIS databases. Pass explicit `sources=` and/or `strategy=` to `plan_w00()` only when overriding these defaults. Full record ingestion for Scopus and Web of Science is not yet ported; count-only access is implemented. Each scope operation performs live count requests and may fail for absent credentials or entitlements; failures are recorded as failures, not zero hits.

## Full W00 retrieval

`run_w00_full("Ostara_collective_action_map")` creates a new immutable W00 plan and runs **all configured automated sources** without the sample limit (Lens, OpenAlex, Scopus, Web of Science Starter, four Europe PMC sources, and 17 EBSCO databases). Uses original upstream R retrieval adapters. Requires source-specific credentials, network access, API entitlements and sufficient local disk space. It performs a credential preflight before requests and stops at the first failed source without deleting existing results. The source adapters may checkpoint pages but this wrapper does **not yet provide automatic resume** of an interrupted full run. Never restart after an incomplete run without assessing what has already been collected. Scopus and WoS full retrieval save raw API responses, which are not yet included in `consolidate_w00()`; full search success does not mean W01-ready consolidation. Do not use `run_w00()` for full harvest; that function remains bounded to 100 records per source. Manual CAB Abstracts and ProQuest remain outside API retrieval.

## W01-main first-run deduplication preparation

For a fully consolidated map, run `inputs <- prepare_w01("Ostara_collective_action_map")`. It reads the latest complete W00 consolidation (including accepted CAB/PQDT RIS files), uses W01-main source adapters for Scopus, OpenAlex, WoS and Europe PMC, reuses supported Lens/EBSCO sidecars, creates source-specific stable IDs for manual RIS, and writes an original W01 source-input manifest. It does not run candidate scoring. Once preparation has been validated, use `candidates <- run_w01_initial(inputs)` to execute the original W01-main first-run union and candidate-generation scripts. **Neither command establishes a deduplicated corpus**; classification, human review, clustering and canonical publication remain separate. Both fail on output collisions and do not modify W00 source files, `LivingEvidenceMap`, or cloud publication. These commands have not yet received live RStudio validation.

## Local W01 adjudication prototype

prepare_w01_review(scored) runs the unchanged W01-main identifier rescoring and creates a review-case queue. This is NOT yet filtered through the LLM, and does not finalise any duplicates.

adjudicate_w01(queue) launches a local Shiny app, prompts for reviewer identity, displays bibliographic metadata and DOI/Google Scholar links, and allows Edit/Delete abstract corrections. It appends locally attributed decision events and abstract correction events. Original W00 and W01 records remain unchanged.

package_w01_review(queue, destination, case_ids=...) creates a portable reviewer folder containing only assigned cases, a standalone app.R, launch.R and task manifest. A collaborator needs R plus shiny, jsonlite and digest; from the assigned folder they run source('launch.R'). Return the whole folder including reviewer_state.

import_w01_review(package_dir, destination) verifies assigned case identifiers, reviewer identity and queue checksums, then archives a separate attributed decision layer. It does not merge the decisions with the corpus or finalise clusters.

Current limitations: the LLM and cluster-finalisation stages are not integrated; W02 and W04-W07 interfaces are not yet implemented. The new local Shiny app has not been runtime-tested in RStudio here. Do not treat this as a complete W01 pipeline.

## W01 local continuation and finalisation

Use `continue_w01(scored, run_llm=TRUE, model="gpt-5.6")` after a successful full W01 scoring run. It reuses the scored candidate pairs, prepares identifier-reviewed W01 cases, calls the unmodified W01-main OpenAI adjudicator in checksummed chunks, and opens the local human reviewer only for cases that need human decisions. API charges apply. Set `run_llm=FALSE` to prepare cases without any model calls. To return to the human interface after completing the LLM, use `adjudicate_w01(file.path(dirname(scored), "review_preparation", "llm_adjudication", "human_review_cases.jsonl"))`. Once all required human decisions are either duplicate or not_duplicate, run `finalize_w01(scored)`. This calls the original W01-main reclustering algorithm with verified source-identity SHA pair keys and refuses incomplete human decisions. Outputs are local and immutable; no GitHub or Zenodo upload is performed. All functions require a single RStudio runtime validation on user data. The portable second-reviewer import layer is distinct and is not automatically substituted for the primary human decision layer.

## W02 ScienceDirect abstracts

`enrich_sciencedirect_abstracts(scored, max_requests=0)` makes a dry-run plan, using finalised W01 clusters and all source manifestations to find clusters without abstracts. A single DOI is required for automatic retrieval. Clusters without DOIs or with conflicting DOI values remain unqueried for review. To make a bounded trial, specify a positive `max_requests` (for example `5`), using the existing `SCOPUS_API_TOKEN` and `SCOPUS_INSTTOKEN` environment variables. Only Elsevier `view=META_ABS` is requested; full text is never requested. The adapter preserves response status, abstract, DOI identity and retrieval timestamp in a separate W02 folder and will not overwrite cached requests. Provider responses are not automatically merged into W01 records or accepted into W02. Further providers, metadata quality decisions and Shiny W02 integration remain to be implemented. This adapter has not yet been tested against the local Ostara corpus.

## Original W02-main with ScienceDirect fallback

`prepare_w02(scored)` reconstructs a 196-record canonical input from finalised W01 clusters and all 512 source manifestations, preserving W01 provenance. `run_w02_main(scored)` calls the unchanged upstream W02-main Europe PMC and Scopus enrichment algorithm using `SCOPUS_API_TOKEN` and `SCOPUS_INSTTOKEN`; successful outputs are cached and checksummed. `add_w02_sciencedirect(scored, max_requests=0)` checks only records still lacking abstracts after W02-main, reuses ScienceDirect DOI caches from the earlier test, and optionally performs bounded `META_ABS` retrieval. Its suggestions are separate and require identity review before acceptance. All source records and W01 clusters are read-only. Original W02 batch planning, patch application, Shiny enrichment adjudication and final W02 output reconciliation are not yet wired into the local package; do not treat the ScienceDirect suggestion file as accepted enrichment. This implementation has not been run on local Ostara data.

For subsequent ScienceDirect lookups after W02-main, use `add_w02_sciencedirect(scored, max_requests=10)`. The earlier zero-result overlay no longer prevents fresh requests; any prior overlay is validated and snapshotted under `sciencedirect_fallback/previous/` before regeneration. Previous DOI response caches are reused; no previous Europe PMC or Scopus calls are repeated. Returned abstracts are pending suggestions, not applied canonical fields.
