# Ostara full-text mechanism feedback: integration contract

Status: website presentation layer implemented on this branch. W09/W10 ingestion
and adjudication are **not yet implemented**. Do not treat a model suggestion
as verified or overwrite historic W07 files.

## Source of truth

- W07 completed run: original immutable ontology.csv, coding.jsonl, manifest.json.
- W09 original model results: immutable source candidates and case annotations.
- W10: human-verified decisions, each tied to record_id, case_id, source quote,
  anchor and original W09 output/checksum.
- Effective mechanism coding: separate derived table, only after adjudication.

## Required W10 review fields (to be connected to existing interface)

Publication-level mechanism rows:
record_id, mechanism_id, abstract_assigned, fulltext_assessment,
human_decision, case_ids, evidence_quote, source_anchor,
reason, w07_run_sha256, w09_output_sha256, reviewer, review_timestamp.

Allowed human_decision:
confirmed_case, mentioned_only, rejected, uncertain.
Additional full-text-confirmed mechanisms can have abstract_assigned=false.
The absence of a row is NEVER interpreted as rejection.

Abstract-proposed novelty follow-up:
record_id, abstract_candidate_id, fulltext_novelty_assessment,
human_novelty_decision, closest_approved_mechanism_id,
revised_label, revised_definition, case_ids, evidence_quote, source_anchor,
reviewer, review_timestamp.

All original W07 abstract proposals must receive a full-text verdict when
case-based full-text verification is marked complete. New proposals discovered
only at full text have a stable case/fulltext proposal ID and no invented
approved ontology code.

## Effective view rules

- Use full-text decisions only when the publication has eligible, human-verified
  cases and a completed mechanism review.
- confirmed_case takes precedence and adds/retains the approved code.
- rejected removes a previously abstract-assigned code from the effective
  coding view for that publication, but not from its immutable W07 source.
- mentioned_only stays in the publication mentions inventory but not in
  case-implemented coding.
- uncertain preserves W07 as provisional; it does not count as confirmation.
- Unprocessed publications stay abstract-only.
- Preserve one record per original and effective decision, with source hashes.

## Website: common harmonisation structure

Both Candidate mechanisms (abstracts) and Candidate mechanisms (case FTs)
use the same layout and provisional classification scheme. The latter accepts
`fulltext_candidates_csv` plus `fulltext_harmonised_csv` through
`publish_ostara_mechanisms()`.

Full-text source candidate CSV columns:
candidate_id,source_search,original_description,original_gap_reason,case_id.

Full-text harmonisation CSV must include:
proposal_id,proposed_label,working_definition,hierarchy_action,
source_candidate_ids,review_status,parent_category_code,
proposed_new_parent_category.

Additional provenance and cross-reference fields should include:
record_ids,case_ids,source_anchors,w07_abstract_candidate_ids,
fulltext_evidence_quotes,classification_rationale.

Every source-candidate ID must appear exactly once across harmonised proposals.
The approved parent code must exist in the W07 ontology; otherwise use a
proposed parent label. Do not label mechanisms as higher-level categories.

No automated model calls, re-screening, or re-extraction are triggered by the
website publisher. W09/W10 integration should be implemented by inspecting
the actual current extraction and human-review function schemas rather than
inventing duplicate form fields.
