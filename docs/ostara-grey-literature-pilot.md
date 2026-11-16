# OSTARA grey-literature case-extraction pilot (filename-only)

Put exactly 10 PDF or XML full-text files in an input directory. No CSV, bibliographic metadata, dates, URLs or manual identity flags are required. The **filename** (without extension) is the source label, not a verified bibliographic title. The software registers the original filename, SHA-256 file hash, file size and format.

## Example

```r
library(evidencelandscaper)
map <- "/path/to/Ostara_collective_action_map"
input <- file.path(map,"grey_literature_inbox","pilot_10")

dry <- run_ostara_grey_literature_pilot(map,input,"grey_pilot_10",run_extraction=FALSE)
# When all 10 are readable and you choose to make API calls:
result <- run_ostara_grey_literature_pilot(map,input,"grey_pilot_10",run_extraction=TRUE)
```

The staging call makes no API requests. The extraction call reuses the exact client-approved prompt/schema/model and the existing W09 validator. PDF documents must have extractable text; structured XML must contain article-body text. Scanned image-only PDFs, DOCX and TXT are not supported by this initial test. Successful records are checkpointed; failures are logged. Zero-case outputs are legitimate. Source labels are not verified publication titles.

## Outputs

All files are under `runs/W09/grey_literature/grey_pilot_10/`: `grey_sources_manifest.json`, `grey_source_readability.csv`, `grey_publication_status.csv`, `grey_case_data_wide.csv` (after a validated run), `grey_extraction_outcome.json`, and version-bound W09 JSONL checkpoints and audits.

The locked published journal-literature case-data snapshot is never changed. Any future grey-lit integration must deduplicate reports and cases, review source identity and evidence and produce a separately versioned combined manifest.

## Client-revised grey-literature 41-field test (same ten files)

Use the same `grey_literature_inbox/pilot_10` input folder and the new
`grey_pilot_10_revised` batch ID. Place the original client-supplied revised
text prompt and 41-row CSV schema there, or anywhere on the local filesystem.

```r
result <- run_ostara_grey_literature_pilot(
  map_path=map,
  input_dir=file.path(map,"grey_literature_inbox","pilot_10"),
  batch_id="grey_pilot_10_revised",
  prompt_file=file.path(map,"grey_literature_inbox","pilot_10",
                        "grey_prompt_revised.txt"),
  schema_file=file.path(map,"grey_literature_inbox","pilot_10",
                        "grey_case_schema_revised.csv"),
  run_extraction=TRUE
)
```

The W09 case validator recognises the 41-row grey-specific CSV, including
source type in field 001, `proposed` status in field 003, publication date
format in field 004, and the case-actor source-authorship/funding/commissioning
question in field 041. The new field is exported with answer, evidence and
reporting status. The original journal schema's validator remains unchanged.

**Documented client-input discrepancy:** the revised prompt illustrates
`systemic or other` for the operating-level fixed choice, but the new CSV
specifies `systemic`. The CSV is authoritative under the client prompt;
the validator applies its choice without changing the client files. The
revised run is isolated by distinct extraction prompt/schema hashes and batch
ID, so earlier attempts on the same PDFs are not silently reused. Source
identity is still filename-only; missing bibliographic data are not invented.
