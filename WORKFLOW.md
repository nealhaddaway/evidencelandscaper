# Ostara workflow: W00–W07

**Start here.** This guide describes the local RStudio workflow implemented in `evidencelandscaper` as of 23 October 2026. Review data remain in the locally configured map directory; this repository contains package code, **not** your project corpus. The pipeline is being developed iteratively, and previous model runs and human decisions must not be overwritten.

## 0. Set up once

Install R, RStudio and the package. Configure your local storage directory and API credentials only for stages that need them. Never commit tokens or confidential database exports.

```r
remotes::install_github("nealhaddaway/evidencelandscaper",
                        dependencies = FALSE, force = TRUE)
library(evidencelandscaper)
set_map_root("/path/to/existing/offline-available/Evidence Maps")
map <- open_map("Ostara_collective_action_map")
```

`set_map_root()` is only required when first configuring a new installation. Once set, use `open_map()`. Existing maps must **not** be recreated. Restarting R may clear R variables, but not locally saved map data.

## 1. W00: search and consolidate

1. Put the approved generic search strategy JSON in `<map>/inputs/search/`. When there are multiple versions, select one via `active_strategy.txt`.
2. Call `plan_w00("Ostara_collective_action_map")` to translate the queries and archive the plan without searching.
3. Optionally call `scope_w00("Ostara_collective_action_map")` for database hit counts.
4. Call `run_w00_full("Ostara_collective_action_map")` only when ready for live requests. This performs searches and incurs any applicable provider usage.
5. Import separately exported CAB Abstracts and ProQuest Dissertations & Theses RIS via `import_ris()`, specifying the actual database, search date and exact query; approve the appropriate imports with `accept_w00_ris()`.
6. Call `consolidate_w00()` for the intended run. Check the manifest and source counts before W01.

**Ostara run completed:** 512 records from 27 source sets, including manual databases. Searches and retrieval are distinct from deduplication. Previous partial search outputs must be inspected rather than blindly restarted.

## 2. W01: deduplicate

1. Prepare from the consolidated W00 run:
   ```r
   inputs <- prepare_w01("Ostara_collective_action_map")
   candidates <- run_w01_initial(inputs)
   ```
2. Score candidates and prepare review using `score_w01_candidates()` and `prepare_w01_review()`; inspect the resulting run directories and manifests.
3. Use `continue_w01(scored, run_llm=TRUE, model="gpt-5.6")` to obtain model-assisted decisions. This makes API requests; `run_llm=FALSE` prepares the human work without new requests.
4. Complete any remaining deduplication decisions in `adjudicate_w01()` (the function expects the produced human-review queue). Preserve corrected abstracts as separate edits.
5. Call `finalize_w01(scored)` once decisions are complete. Do not repeat finalisation against the same output directory.

**Ostara result:** 512 source manifestations consolidated into **196 canonical publications**. `scored` is the existing W01 full-scoring directory produced by the previous steps, not a newly invented file path.

## 3. W02: enrich and finalise

1. Run `prepare_w02(scored)` and `run_w02_main(scored)` to prepare the canonical records and perform supported metadata/abstract enrichment.
2. Optionally use `add_w02_sciencedirect(scored, max_requests=...)` for separately reviewable ScienceDirect suggestions. These are **not** silently accepted.
3. Call `finalize_w02(scored)` to apply validated enrichment and produce `W02_main/finalised/canonical.jsonl`.

**Ostara result:** 196 canonical publications, initially 190 with abstracts and six without. Five missing abstracts were subsequently supplied separately during W04; W02 source data were not overwritten.

## 4. W03: publication status

```r
run_w03(scored, limit = 0, batch_size = 100)
```

This performs full-corpus publication-status checks, with audit and provenance. `limit > 0` is a diagnostic subset only, not a full-corpus handover. The 196 Ostara publications passed to screening.

## 5. W04: screen, adjudicate, finalise

1. Archive an approved UTF-8 screening prompt with `archive_map_prompt(prompt_file, "W04", map)`.
2. Run a bounded screening trial with `run_w04(scored, prompt_file, max_records=20, model="gpt-5.6", open_app=FALSE)`. Test the prompt and manually check the outputs before a full run.
3. Use `run_w04_full()` for full-corpus screening with the approved prompt; it consumes API tokens. Do **not** restart a completed run to change the human review.
4. If abstracts are absent, use `edit_w04_missing_abstracts(w04)` and `rescreen_w04_shiny_abstracts(w04)` as appropriate, then `prepare_w04_reconciled_review(w04)`. This adds an immutable reconciled layer rather than modifying original model votes. Only run reconciliation if corrections/rescreening have actually been completed.
5. Open `adjudicate_w04(w04)`. The local Shiny app saves each decision immediately; reopen the same run to resume at the first unanswered case. Reasons are optional, but every outstanding record requires a decision. Resolve remaining `Uncertain` choices to `Retain` or `Exclude` before finalisation.
6. Close Shiny and run:

   ```r
   import_w04_review(w04)
   final <- finalize_w04()
   jsonlite::fromJSON(file.path(final, "summary.json"))
   ```

   The import validates the immutable queue and saved decisions. Never delete or manually rebuild the saved queue, as its checksum is part of human decision provenance.

**Verified Ostara final result (23 October 2026):** **196 screened, 170 retained, 26 excluded; status `final`, full run, corrected-abstract reconciliation `TRUE`.** This output is authoritative for local downstream coding; no W05 handover is automatically published.

For the existing Ostara full W04 run, if you have restarted R, you can locate it with:

```r
w04 <- file.path(
  open_map("Ostara_collective_action_map"),
  "runs/W01/prepare-20261022T141901-fileb007b4ea-27997dd03a0c",
  "W04_local/full-d20f2fa4c474"
)
```

This path is specific to the October 2026 Ostara run, not a reusable path for other maps.

## 6. W07: code collective action mechanisms

W07 presently codes retained abstracts directly from the **finalised W04 full layer**. Do not assume separate production W05/W06 transitions have happened.

1. Use the agreed **36-code v3 ontology** and approved W07 prompt. Earlier v2 experiments and candidate mechanisms must not be mixed into the approved classification.
2. The W07 runner can reuse a verified, archived ontology/prompt pair from earlier W07 trials. Inspect the printed source folder to confirm it is actually the approved v3 pair, because the package does not itself enforce v3.
3. Run the full corpus:

   ```r
   w07 <- trial_w07(n = 0, model = "gpt-5.6")
   print(w07)
   ```

4. The runner checks W04 final counts, requests model coding for retained records, and appends results to `<map>/runs/W07/full-170-.../coding.jsonl`. It archives the selected ontology and prompt, manifest, and `summary.json`; `candidate_mechanisms.csv` records possible missing mechanisms separately.
5. Review distribution of mechanism assignments, `no_supported_mechanism` and `insufficient_information` cases, and the candidate registry before any ontology revision. Do not claim that one model pass amounts to validated substantive coding.

**Status:** W04 full corpus is confirmed complete; the 170-record W07 full coding remains to be confirmed locally. The archived W07 ontology/prompt fallback chooses the latest verified pair, not necessarily the approved v3.

## 7. W08: independent abstract case-identification pilot

**Separate from W07.** This is stage **1 of 2** of the case registry project. Stage 2 (targeted full-text retrieval and detailed case-data extraction) will be designed only after the Stage 1 assessments have been examined and validated.

Run a reproducible random sample of **20** publications retained in finalised W04, with **one model call per abstract**. This operation does not require W07 mechanism coding or a mechanism ontology.

```r
remotes::install_github("nealhaddaway/evidencelandscaper",
                        dependencies=FALSE,force=TRUE)
library(evidencelandscaper)
w08 <- trial_w08_cases(n=20,seed=20261023,model="gpt-5.6")
print(w08)
jsonlite::fromJSON(file.path(w08,"summary.json"))
```

The dedicated prompt is packaged at `inst/w08/case_identification_prompt_v1.txt`; the field specification is `inst/w08/case_identification_fields_v1.csv`. No file selection is needed. The fixed seed, W04/W02 SHA-256 fingerprints, sample IDs, model and prompt are recorded. Outputs under `runs/W08/sample-20-...` include `case_assessments.jsonl`, `publication_assessments.csv`, `case_candidates.csv`, `manifest.json`, `summary.json`, `prompt.txt` and `fields.csv`. Reusing the same settings preserves completed JSONL responses and resumes missing records; do not delete or rewrite these files.

The model assesses specific cases, multiple cases, no identifiable case, or unclear case evidence. Candidate case descriptions and short supporting excerpts are retained separately. **A case flag is a provisional abstract-level signal, not proof that a full-text case analysis is feasible.** Manually review the 20 assessments and identify false positives/negatives before extending sampling or designing Stage 2. Original W04 and W07 artefacts remain unchanged.

## 8. Files, reproducibility and restrictions

- **Never alter** upstream `thesalmonandthetomato/LivingEvidenceMap` or `thesalmonandthetomato/LivingEvidenceMapAdjudication`; this project reuses selected upstream scripts without modifying those repositories.
- Map-level inputs, source records, API responses, human decision events, prompts, checksums and output manifests are saved under the user's **local map root**. GitHub code updates do not upload the local corpus.
- A resumed run must use the **same original map, queue, prompt and ontology checksums**. Do not delete results to fix a checksum error.
- `gpt-5.6` calls incur API costs; `plan_w00()`, reading manifests, and reopening local Shiny decisions do not rerun models.
- Trial outputs are not authoritative full-corpus results. W07 candidate mechanisms are **proposals**, not additions to the fixed ontology. Multiple codes per publication are supported.
- W05 and W06 are not represented here as completed independent local stages. Portable ZIP assignment/return for W04 colleagues has been discussed but **not implemented**; do not promise it as existing functionality.
- For API/reproducibility questions, read the relevant `R/` function and its `inst/` upstream script; the README is an overview rather than a complete function reference.

## Quick check after any run

```r
map <- open_map("Ostara_collective_action_map")
final <- finalize_w04(map)
jsonlite::fromJSON(file.path(final, "summary.json"))
```

For an already finalised run, `finalize_w04()` verifies and reuses the existing immutable final result. Do **not** call it before review completion.

---
*Maintained alongside the R code. Amend this guide whenever stage behaviour, entry points or validated statuses change.*

## W08 v2 and W09 full-text case pilot (23 October 2026)

The first W08 20-record run used prompt v1; it is retained unchanged. New `trial_w08_cases()` calls default to packaged **v2**, which distinguishes identifiable collective action from research studies, commercial service provision and broad social mobilisation. It records separate confidence in case identification and evidence of actor coordination. Check the prompt fingerprint for any new pilot; do not assume earlier v1 responses were revised.

W09 is independent of W07/W08 and runs in two explicit steps:

1. **Retrieve and verify.** `prepare_w09_cases()` produces a local six-publication queue (four promising candidates and two challenging examples), without external requests. `retrieve_w09_oa(w09)` optionally attempts open-access PDF retrieval via DOI/OpenAlex. It reports missing or unsuccessful downloads without changing the source records. Alternatively, place legally obtained PDFs under `<w09>/pdfs/<record_id>.pdf`. **Manually confirm the PDF matches the publication**; a valid PDF header alone does not validate source identity. Use `inventory_w09_pdfs(w09)` to inventory available files.
2. **Full-text verification and extraction.** Once a PDF's identity has been checked, explicitly pass its ID to `analyse_w09_cases(w09,record_ids=c("work-..."))`. This reads page-labelled PDF text with `pdftools`, uses the archived W09 prompt and makes one OpenAI call per unprocessed, confirmed PDF. Output is append-only `fulltext_extraction.jsonl`. Author/participant claims are extracted as attributed, quotation-supported observations, **not evidence-map judgements of effectiveness**. This is an experimental pilot; manually validate extracted quotations, page numbers, case boundaries and source claims. No automated publication-case deduplication or relational export is implemented yet.

```r
remotes::install_github("nealhaddaway/evidencelandscaper", dependencies=FALSE, force=TRUE)
library(evidencelandscaper)
w09 <- prepare_w09_cases()
retrieve_w09_oa(w09, max_records=6)
inventory_w09_pdfs(w09)
# Only after checking a downloaded or supplied PDF matches its publication:
# analyse_w09_cases(w09, record_ids=c("work-e35e229f44c00c48"))
```

Avoid running W08 v2 or W09 full-text model calls until their respective pilot inputs and cost are understood; neither is triggered by preparation. W09 does not overwrite or revise the completed W08 v1, W07, or W04 layers.

### W09 retrieval update: OpenAlex, then ScienceDirect only

Use `retrieve_w09_fulltexts(w09, max_records=6)` to retry **only missing PDFs** in the existing W09 pilot. OpenAlex attempts DOI identification first and exact-normalised-title matching when DOI lookup fails. Candidates with conflicting DOIs or ambiguous identities are not downloaded. When OpenAlex provides no downloadable PDF, the function attempts Elsevier's authorised ScienceDirect Full Text Article Retrieval API by DOI, using `SCOPUS_API_TOKEN` and optional `SCOPUS_INSTTOKEN`. ScienceDirect PDF availability requires separate full-text/API entitlement; the API key alone is insufficient. A record without a usable DOI cannot be sent to the ScienceDirect DOI endpoint. No Europe PMC, Unpaywall or additional providers are queried.

Existing PDFs are never replaced; the prior `oa_retrieval.jsonl` is immutable. New attempts are logged in `w09_retrieval_v2.jsonl`. A downloaded file remains `downloaded_unverified` until a human confirms publication identity and that its contents are readable. Repeat calls skip successfully downloaded files and records already assessed by v2. `inventory_w09_pdfs()` archives its earlier inventory when counts change. Neither function initiates model analysis.

### W09 structured retrieval v3 (preferred)

`retrieve_w09_structured(w09, max_records=6)` continues the **existing** six-publication pilot. It does not overwrite old PDF files or retrieval logs. Its append-only log is `w09_retrieval_v3.jsonl`. OpenAlex first tries DOI, then exact normalised-title matching when necessary. An OpenAlex DOI may be used for Elsevier only with **one unambiguous matched work** and no conflicting queue DOI. The chosen DOI and provenance appear in each log entry. OpenAlex is used only for identification and OA PDFs; **ScienceDirect requests XML first, JSON second, PDF last**, requiring FULL view and available institutional entitlements. Every format's HTTP response/status or failure is logged. Failed or ambiguous responses are not saved as completed full-text extractions. Structured results are saved in `<w09>/structured/<record_id>.xml` or `.json`; files remain unverified until the user confirms identity and that the body (not just metadata) is present. `analyse_w09_cases()` currently reads PDF only; it must be extended and validated before analysing structured XML/JSON. The v3 retrieval function does not make any model calls.

```r
remotes::install_github("nealhaddaway/evidencelandscaper",dependencies=FALSE,force=TRUE)
library(evidencelandscaper)
w09 <- prepare_w09_cases()
log <- retrieve_w09_structured(w09,max_records=6)
records <- jsonlite::stream_in(file(log),verbose=FALSE)
print(records[, c("record_id","openalex_doi","doi_used","sciencedirect_status","retrieval_status")])
```

### W09 full-text analysis: PDF, XML or JSON

`analyse_w09_cases(w09, record_ids=...)` now reads `structured/<record_id>.xml`, then `structured/<record_id>.json`, then `pdfs/<record_id>.pdf` (in that preference order). XML parsing uses `xml2`; PDF uses `pdftools`. ScienceDirect structured files must contain substantive text, not just metadata or access-error bodies. Nothing is silently truncated; large text is reported for manual scoping. The existing `fulltext_extraction.jsonl` remains append-only, and already analysed record IDs are skipped.

**Evidence anchors:** PDFs have page numbers. XML and JSON do not automatically provide reliable print pagination. For these files W09 uses ordered `[SECTION n]` or `[TEXT BLOCK n]` anchors, and instructs the model to use `source_anchor` with `page_number=null`. The source file format, SHA-256 and anchors are stored in result provenance. This output needs human verification of case identity, quoted text and source anchors before research use. Do not assume a downloaded structured file matches the selected publication.

```r
remotes::install_github("nealhaddaway/evidencelandscaper",dependencies=FALSE,force=TRUE)
library(evidencelandscaper)
w09 <- prepare_w09_cases()
# Only supply IDs AFTER verifying that their full-text files match the publications:
# analyse_w09_cases(w09,record_ids=c("work-e35e229f44c00c48"))
```

### W09 v2: moderate case registry with geographic and mechanism validation

The original W09 v1 full-text cases remain untouched. New `analyse_w09_cases_v2()` creates its own versioned directory under the W09 pilot. The archived, checksummed full-corpus W07 ontology and coding are the exclusive mechanism reference, with one full-text status per abstract-assigned mechanism and additional supported mechanisms recorded per case. Geography uses separate country name and validated ISO 3166-1 alpha-3 code fields, plus distinct subnational location entries linked to their operational country. Commodities and controlled supply-chain stages are repeatable lists. Case summaries cover objectives, actors, collaboration, governance, resources, enablers, barriers, and only explicit author conclusions, not effectiveness assessment, significance tests or quantitative effect estimates. Country-code mismatches stop the run before it is appended.

Install `countrycode` before use. To protect the existing pilot, run only after verifying the source full-text identity, preferably on one record first:

```r
if (!requireNamespace("countrycode",quietly=TRUE)) install.packages("countrycode")
remotes::install_github("nealhaddaway/evidencelandscaper",dependencies=FALSE,force=TRUE)
library(evidencelandscaper)
w09 <- prepare_w09_cases()
# Will make ONE new model call, in a new W09 v2 directory; do not call until ready.
# v2 <- analyse_w09_cases_v2(w09,record_ids="work-e35e229f44c00c48")
```

The model response is archived per publication in `fulltext_case_registry.jsonl`, never overwriting v1 `fulltext_extraction.jsonl`. The extraction uses a single JSON object per publication with separate nested country, location, commodity, supply-chain stage and mechanism arrays. These outputs remain provisional pending human checks against source texts. W04 and W07 are read-only.

### W09 prompt v2.1 supply-chain clarification

The packaged `fulltext_case_prompt_v2_1.txt` replaces only the SUPPLY CHAIN section of v2, restricting stage assignment to stages undertaken or directly coordinated by the initiative. The original v2 prompt is preserved. `analyse_w09_cases_v2()` defaults to v2.1 and creates a different prompt-checksummed directory, so old v2 records remain intact. To test the next case, after source identity verification:

```r
remotes::install_github("nealhaddaway/evidencelandscaper", dependencies=FALSE, force=TRUE)
library(evidencelandscaper)
w09 <- prepare_w09_cases()
w09_v2_1 <- analyse_w09_cases_v2(w09, record_ids="work-a98a0ec08d809374", model="gpt-5.6")
print(w09_v2_1)
```
