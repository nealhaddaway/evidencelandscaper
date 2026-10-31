# Human review and recovery checkpoints

**These scripts do not perform scientific judgement on your behalf.**
Each approval in `00_settings.R` is a human methodological decision.

| Checkpoint | When | What you must inspect | What to do |
|---|---|---|---|
| Search strategy | Stage 01 | `inputs/search/<run_name>.json`; source list | Edit terms/Boolean expression; set `strategy_approved=TRUE` before the first run |
| W00 | Stage 02 | W00 source logs, retrieval manifest | An incomplete harvest is not a complete search; resume or explicitly document omissions |
| W01 | Stage 03 | W01 review queue, scored pairs | Decide or adjudicate duplicates; `w01_llm_approved` permits LLM classification, not automatic human review |
| W04 sample | Stage 03 | W01 eligible unique-record count | Choose `abstract_sampling` and set `abstract_sample_approved=TRUE` |
| W04 decisions | Stage 04 | Screening output and uncertain cases | Resolve uncertainties per protocol; only then set `abstract_finalisation_approved=TRUE` |
| W07/W08 | Stage 05/06 | Ontology proposals and uncertain cases | Review before deciding full-text cohort; set `case_sample_approved=TRUE` |
| W09 | Stage 06 | Download and identity-verification reports | Repair missing/mismatched files; never treat an HTML error page as a PDF/XML |
| Full-text eligibility | Stage 07 | `candidate_case_eligibility.csv` and cited evidence | Finish required review, then set `fulltext_eligibility_review_complete=TRUE` |
| Full-text mechanisms | Stage 08 | Mechanism review and candidate novelty worksheets | Existing legacy W09/W10 input only; requires human review; new named runs not yet integrated |

## Status and recovery

From the RStudio project folder:

```r
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
str(s,max.level=2)
```

If the run already exists, **do not** delete its folder, change its archived search
strategy or invoke `run_evidence_map()` again. Use the same `run_name`
and the numbered stage for its current state.

A stopped script does not automatically mean a failed stage. Many scripts
stop deliberately when they need a decision. A nonrecoverable or malformed
archive should be investigated rather than overwritten.

## API credentials and licences

Some selected databases require API keys, subscriptions, and permissions.
Keep secrets in environment variables, never in the project or Git.
Model-based stages can incur charges. SAGE TDM links are not a substitute
for whitelisted publisher access: a Cloudflare 403 must be resolved with SAGE.
