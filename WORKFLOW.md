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

## 7. Files, reproducibility and restrictions

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
