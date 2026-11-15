# OSTARA grey-literature case-extraction pilot

This workflow stages exactly ten *human-identity-verified* grey-literature
full texts and runs the **unchanged approved pilot prompt and schema** through
the existing `run_w09_case_data_extraction()` model/validator.

## Source inputs

Make an input folder with exactly the ten intended PDF or XML files and a
`sources.csv` file with these headers:

```csv
file_name,title,source_url,access_date,human_verified_identity
```

Enter a complete row for each document. `source_url` is the original
publisher/institution URL or `doi:...`; `access_date` is `YYYY-MM-DD`;
`human_verified_identity` must be `TRUE` only when the operator has
confirmed that the attached full text matches its title and provenance.
PDFs must contain extractable text; scanned image-only documents are logged
as unreadable, not treated as having zero cases. XML must contain recognizable
full-text article-body elements. Neither DOCX nor TXT is supported by this
initial pilot. Convert those formats beforehand with an auditable method.

## Invocation

```r
library(evidencelandscaper)
map <- "/path/to/Ostara_collective_action_map"
input <- file.path(map,"grey_literature_inbox","pilot_10")

# Stage all ten, check source readability and write immutable source manifest:
dry <- run_ostara_grey_literature_pilot(
  map_path=map,input_dir=input,batch_id="grey_pilot_10",
  run_extraction=FALSE)
print(dry)

# Once approved, incur OpenAI API calls for only unvalidated sources:
result <- run_ostara_grey_literature_pilot(
  map_path=map,input_dir=input,batch_id="grey_pilot_10",
  run_extraction=TRUE)
print(result)
```

Validated publications are checkpointed using the same prompt/schema/model
hashes, so re-running the same batch does not re-extract completed records.
Malformed model responses have the same original three-attempt limit and
remain unvalidated with their failures logged. All ten full texts are submitted
without assuming case-positive classification; zero-case responses are
legitimate outcomes.

## Provenance and reports

Outputs live in
`runs/W09/grey_literature/grey_pilot_10/`. Important files:

* `grey_sources_manifest.json`: immutable source identity, title, URL,
  access date, file SHA-256, prompt/schema hashes.
* `grey_source_readability.csv`: readable/unreadable by publication.
* `grey_publication_status.csv`: successful, still pending, or unreadable.
* `grey_case_data_wide.csv`: cases with answers, quotes, anchors and statuses.
* `grey_extraction_outcome.json`: current extraction totals and source hashes.
* `case_data_extraction/.../case_data.jsonl`: append-only validated records.
* `case_data_extraction/.../invalid_responses.jsonl`,
  `batch_failures.jsonl`, and `evidence_validation_audit.jsonl`:
  model-response failures and evidence-review flags.

The final peer-reviewed 100%-cohort snapshot is **not edited**. A future
grey-lit integration release must separately deduplicate by document identity,
adjudicate inclusion, preserve the published snapshot and issue a new
versioned combined manifest.
