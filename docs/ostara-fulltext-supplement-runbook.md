# Ostara comprehensive full-text mechanism supplement

The prior W09 v2.5 extraction has abstract mechanism validation,
case-level additional mechanisms and ontology gaps. It is **not** a
comprehensive inventory of all mechanisms present in the full text.

The new supplement makes one checkpointed OpenAI Responses API call per
W10-eligible publication. It reads the **entire existing W09 PDF/XML/JSON**,
the exact archived W07 ontology and its original abstract coding.
It does not re-run W07, W09 case extraction, W10 case eligibility or
change their original artefacts.

## Fresh R session

```r
remotes::install_github(
 "nealhaddaway/evidencelandscaper@ostara-fulltext-mechanism-feedback-20261027",
 dependencies=FALSE,force=TRUE,upgrade="never"
)
library(evidencelandscaper)
map <- open_map("Ostara_collective_action_map")
review_dir <- file.path(map,"reports","mechanism_review","review-43843f1a4977")

# One previously eligible publication: first costly API smoke test
supplement <- extract_ostara_fulltext_mechanisms(
 review_dir=review_dir,max_records=1L,model="gpt-6-luna")
print(supplement)
```

Inspect the appended `mechanism_assessments.jsonl`. Check an actual
full-text quotation, source anchor, case ID, every approved mechanism
status, and W07 novelty follow-up before processing the rest.

After this has passed:

```r
supplement <- extract_ostara_fulltext_mechanisms(
 review_dir=review_dir,max_records=0L,model="gpt-6-luna")
review <- prepare_ostara_supplement_review(
 review_dir=review_dir,supplement_dir=supplement)
print(review)
```

The full run automatically skips successfully checkpointed publications.
Because the output directory includes prompt and ontology hashes, changing
either creates a distinct run. Failed validation does not append an invalid
model response.

The new review worksheets are **not automatically approved**. Verify W10
case eligibility, implementation versus mention/proposal, and ontology
novelty in their source publications. Complete reviewer fields. Then:

```r
finished <- finalise_ostara_mechanism_review_for_map(
 map=map,review_dir=review$review_dir)
print(finished)
```

Harmonise `finished$website_inputs$harmonisation_review` across all
full-text suggestions against the 36 abstract candidate hierarchy and
the original approved W07 ontology, and publish via the existing
`publish_ostara_mechanism_feedback()`.

**Coverage:** only the W10-eligible case publications with retrievable,
verified full texts. The output is not a mechanism prevalence census
for all 50,289 records.

**Important validation boundary:** Tests and R runtime are not available
in the assistant's environment. The new supplement has not yet been
run or verified on the local case files. Do not merge until a one-record
smoke test and the full package tests pass.
