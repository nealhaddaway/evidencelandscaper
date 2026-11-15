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
