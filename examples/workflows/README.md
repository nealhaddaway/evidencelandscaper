# Run evidencelandscaper from RStudio

These are **operator scripts**, not implementations of W00-W10. They call
exported `evidencelandscaper` functions and preserve the original package
algorithms, prompts, and archived search data.

## Quick start: exact steps

1. **Pull main** in RStudio (Git pane > Pull), or clone
   `nealhaddaway/evidencelandscaper` at branch
   `main`.
2. In the repository, open `examples/workflows/evidencelandscaper-workflows.Rproj`.
   Confirm `getwd()` ends in `examples/workflows`.
3. Restart R, then install the **same branch** once:

   ```r
   if (!requireNamespace("remotes",quietly=TRUE)) install.packages("remotes")
   remotes::install_github(
     "nealhaddaway/evidencelandscaper@main",
     dependencies=FALSE,force=TRUE,upgrade="never")
   library(evidencelandscaper)
   ```

4. If `open_map("Ostara_collective_action_map")` already works, leave the map
   root alone. Otherwise set the parent of your map directory once with
   `set_map_root("/your/existing/map/parent")`. **Do not set the map root to this
   scripts directory.**
5. Edit `00_settings.R`: `map_name`, unique `run_name`, source databases,
   model, optional manual RIS exports, and approved input paths.
6. Source `01_create_map_and_scope.R` once. It creates an illustrative JSON
   strategy only if none exists, then stops for approval. Open the generated
   `inputs/search/<run_name>.json`, replace the example terms/expression,
   and check it carefully. Set `strategy_approved=TRUE`, then source 01 again.
7. Inspect gross hit counts. Source `02_download_search_results.R` to
   retrieve records, followed by 03-07 in sequence. At a deliberate stop,
   inspect the named artifact, make the human decision in `00_settings.R`,
   and rerun the **same numbered script**. Read `CHECKPOINTS.md`.
8. Stage 08 now creates a checksum-verified mechanism review directly from the
   eligibility-first named search and runs supplementary coding of all eligible
   case publications. Human review of mechanism evidence and novel proposals
   remains mandatory.
9. Stage 09 publishes verified case outputs. Publishing and combining
   different searches are distinct operations.

## What each script does

| File | Task | Package entry point / checkpoint |
|---|---|---|
| 00_settings.R | Shared configuration | Map, new search ID, sources, prompts and approvals |
| 01_create_map_and_scope.R | Create/open map, W00 planning and scoping | `run_evidence_map()`; `resume_evidence_map(step="scope")` |
| 02_download_search_results.R | Search and download W00 records | `step="retrieve"`; optional manual RIS |
| 03_deduplicate_repair_retractions.R | W01 deduplication, W02 enrichment, W03 checks | `deduplicate`, `finish_w01`, `enrich` |
| 04_screen_abstracts.R | W04 inclusion screening | `screen`, `finalize_screen` |
| 05_code_ontology_and_screen_cases.R | W07 ontology and W08 abstract cases | `code` runs both |
| 06_retrieve_and_verify_fulltexts.R | Select case cohort, W09 retrieval | `prepare_extraction`, `retrieve_fulltexts` |
| 07_screen_fulltexts_and_extract_cases.R | Full-text eligibility before detailed cases | `assess_fulltexts`, `finalize_eligibility`, `extract` |
| 08_screen_fulltexts_for_mechanisms.R | Supplementary full-text mechanisms | `prepare_ostara_mechanism_review_from_run()` then `extract_ostara_fulltext_mechanisms()` |
| 09_publish_outputs.R | Publish verified cases | `publish` |

## Essential safeguards

- A new search within Ostara requires a new `run_name`, **not** a new map.
  The named run and its archived query are immutable after creation.
- `create_map()` is only for a genuinely independent map; it refuses to
  overwrite an existing project.
- `strategy_approved`, sampling approvals and review flags are **human
  decisions**, not automatically certified by the package.
- W00 source counts are gross hits, not unique studies. W01 establishes a
  deduplicated publication population.
- Uncertain W01 duplicate pairs with `w01_uncertain="adjudicate"` block
  further progress until adjudicated. Do not silently quarantine them.
- W07 ontology coding and W08 abstract case detection are currently grouped
  into one orchestrated `step="code"`.
- W09 full-text availability is separate from identity and eligibility.
  Check original documents and provenance before assessing cases.
- For newly created named searches the W09 path applies full-text case
  eligibility **before** detailed extraction. Existing Ostara runs may use
  the earlier W09-extraction-then-W10-eligibility path.
- Stage 08 constructs a checksum-verified mechanism review from the
  eligibility-first W09 data and approved case IDs. It refuses to mix
  unrelated searches or substitute a legacy W10 decision ledger.
- The scripts use your installed package, not the code of the repository
  checkout, so **pull the branch and install the same branch**.

## Diagnostics

```r
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
str(s,max.level=2)
```

If a stage errors, first read the message and `CHECKPOINTS.md`.
Do not rename saved run folders, change an archived query to get past
validation, or launch a second W00 run to hide an incomplete first run.

API keys belong in environment variables and are subject to provider licences.
Do not commit credentials. W01/W04/W07/W08/full-text model stages may incur
costs. Site-level restrictions such as Cloudflare blocking SAGE TDM require
publisher-side resolution.

## Validation and scope

The wrappers were checked against the functions and orchestrator states
on main, but **have not been executed against your local map or API
credentials**. They deliberately block at human decisions. A synthetic
integration test exercises the new eligibility-first mechanism handover.
