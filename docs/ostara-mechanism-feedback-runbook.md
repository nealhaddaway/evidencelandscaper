# Ostara mechanism feedback: complete local RStudio workflow

This change is **not** a new screening or extraction run. It reuses original
W07 coding, previously archived W09 v2 extraction, W10 first-pass and (when
present) second-pass case-eligibility judgements.

Historical W07, W09 and W10 data are never overwritten. Human review is
required before a model-proposed override becomes authoritative.

## Step A: fresh R session, prepare review worksheets

```r
remotes::install_github(
  "nealhaddaway/evidencelandscaper@ostara-fulltext-mechanism-feedback-20261027",
  dependencies = FALSE, force = TRUE
)
library(evidencelandscaper)
map <- open_map("Ostara_collective_action_map")

review <- prepare_ostara_mechanism_review_for_map(map)
print(review)
```

The function locates complete W09/W10 pairs using checksums and rejects
ambiguous matches rather than guessing. It reads the second-pass W10
eligibility ledger if there is a unique matching one. It creates
`mechanism_review.csv`, `novelty_review.csv`, and a provenance manifest
under the map's reports folder. No model API call is made.

For each eligible publication, a reviewer must check every assigned W07
mechanism, each W09 additional case mechanism, and each W07/W09 novel proposal.
Fill in `human_decision` with
`confirmed_case`, `mentioned_only`, `rejected`, or `uncertain`.
Set `review_complete=TRUE` for every mechanism of a completed publication.
Add reviewer, timestamp, supporting source quotation, exact source anchor,
and case IDs for confirmed mechanisms. Do not label a mechanism as implemented
merely because it is mentioned.

In `novelty_review.csv`, review `human_novelty_decision` as
`confirmed_new`, `refined_new`, `covered_by_existing`, `rejected`,
or `uncertain`. Supply exact approved codes for proposals mapped to
an existing mechanism. For confirmed/refined candidates, supply a precise
label, definition, exact source quotation/anchor and case link. Map W07
abstract suggestions to their full-text assessment. A human reviewer must
complete the reviewer field for every novelty row.

## Step B: after editing the two review CSVs, finalise feedback

Restart R if desired, install and open the package using the same installation
commands. Then specify the **review directory**, which is the parent folder
of both CSVs and `manifest.json`:

```r
review_dir <- dirname(review$mechanisms)
finished <- finalise_ostara_mechanism_review_for_map(map, review_dir)
print(finished)
```

This refuses incomplete human mechanism reviews. It writes source-preserving
W07 versus effective-coding tables, reconciled novelty decisions, source
hashes and review-ready website files in a new `finalised_mechanism_feedback`
subfolder. The original W07 classification is still accessible.

## Step C: harmonise reviewed full-text candidates

Open
`finished$website_inputs$harmonisation_review`.
It is deliberately a **draft**, one row per reviewed FT proposal.
Consolidate duplicate source suggestions into proposals, checking every
proposal against the complete approved W07 ontology and the existing
abstract-level 36-proposal harmonisation. Retain all candidate ID mappings.

Each final row requires either a valid existing `parent_category_code` or
a `proposed_new_parent_category`. Use `existing_mechanism`,
`refine_existing`, or `new_lower_level`; never label individual
mechanisms as new higher-level categories. Provide approved code when
classifying as existing/refinement. Change `review_status` from
`pending_harmonisation` to a reviewed status. A pending row is rejected
by the website publisher.

## Step D: publish dual-tab meeting website

Save the reviewed abstract harmonisation at
`file.path(map, "ostara_harmonised_mechanisms_hierarchy_review_v2.csv")`,
and the harmonised FT CSV at a known location. Then:

```r
abstract_review <- file.path(
  map, "ostara_harmonised_mechanisms_hierarchy_review_v2.csv"
)
fulltext_review <- finished$website_inputs$harmonisation_review
website <- publish_ostara_mechanism_feedback(
  map = map,
  abstract_harmonised_csv = abstract_review,
  review_dir = review_dir,
  fulltext_harmonised_csv = fulltext_review
)
browseURL(website)
```

If there are **no** full-text novelty candidates, omit
`fulltext_harmonised_csv`; the fifth tab reports that no candidate set
was supplied.

## Implementation and test boundaries

The existing W09 v2.5 model schema already has
`abstract_mechanism_validation`, per-case `additional_mechanisms`, and
`ontology_gaps`. This change uses those fields; it does not repeat model
extraction. W10 first/second pass remains responsible for **case eligibility**,
while the human review worksheet handles **mechanism verification**.

Regression tests were written for original W07 immutability, FT-confirmed
addition, FT rejection, novelty classification and exclusion of W10-ineligible
cases. They have **not** been executed in this environment, which has no R
interpreter. Before merging, run `testthat::test_local()` on the package
and one real eligible publication end-to-end. Do not infer success from
file existence or a passed JavaScript syntax check.
