# W07 v4: Run client mechanism ontology and revised prompt

Use the client-supplied five-column ontology as-is. The current `trial_w07()`
accepts this native format (28 unique mechanisms) and the old W07 format.
The code sends the ontology's `mechanism_id`, `collective_action_type`,
`high_level_mechanism`, `fine_level_mechanism`, and `definition` columns
directly to the model, without manufacturing old indicator fields.

Required prerequisites:
- An explicitly selected W04 run whose results have already been finalised.
- The revised client ontology CSV and the approved UTF-8 W07 prompt TXT.
- `OPENAI_API_KEY` configured in the R session.
- `evidencelandscaper` reinstalled from `main` after pulling latest changes.

```r
library(evidencelandscaper)
map <- open_map('Ostara_collective_action_map')

# Select the revised ontology CSV, then the revised prompt TXT.
ontology_v4 <- file.choose()
prompt_v4 <- file.choose()

# Set to the explicitly verified finalised W04 run.
# This example is the original narrow-search W04 baseline.
narrow_w04 <- file.path(map, 'runs', 'W01',
  'prepare-20261022T141901-fileb007b4ea-27997dd03a0c',
  'W04_local', 'full-d20f2fa4c474')
stopifnot(file.exists(file.path(narrow_w04, 'finalised', 'summary.json')))

# Pilot: first 20 of the retained records (NOT a random sample).
v4_pilot <- trial_w07(ontology_file=ontology_v4,
  prompt_file=prompt_v4, n=20L, model='gpt-5.6',
  map_path=map, w04_run=narrow_w04)

# Entire set of retained records, AFTER the pilot is assessed:
# v4_full <- trial_w07(ontology_file=ontology_v4,
#   prompt_file=prompt_v4, n=0L, model='gpt-5.6',
#   map_path=map, w04_run=narrow_w04)
```

Important: n=0 codes all W04-finalised RETAINED records, not every screened
record. To compare a newly revised W04 run, first complete human review and
`finalize_w04()` on that exact run; do not silently substitute the published
baseline. Running `trial_w07()` does not alter W04 data.

New W07 v4 paths are keyed to ontology/prompt/model AND the exact W04 final
layer checksum. The same files can therefore be trialled on narrow and
broader-search cohorts without mixing outputs. Archived prior W07 directories
and previous six-column ontologies remain unchanged.

Validation: each assigned mechanism needs an existing mechanism ID, status
(implemented / proposed / mentioned_only / unclear), confidence, a
justification, and a verbatim source excerpt traceable to the record.
Ontology gaps require a proposal and reason; the nearest code, if supplied,
must already exist. No new codes are silently added.

Use a separate human validation review before interpreting model outputs.
