# STAGE 03: W01 deduplication, W02 metadata repair, W03 status/retractions.
# Human review and sampling are explicit approval checkpoints.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (s$status %in% c("retrieved","consolidated","w01_prepared","w01_candidates"))
  s <- resume_evidence_map(run_name,map=map,step="deduplicate")
if (s$status %in% c("w01_scored","w01_classifying","w01_review_pending")) {
  if (!isTRUE(w01_llm_approved)) {
    if (identical(s$status,"w01_scored"))
      s <- resume_evidence_map(run_name,map=map,step="prepare_w01_review")
    stop("Inspect W01 review queue. For billed LLM classification, set ",
         "w01_llm_approved=TRUE and rerun. Human adjudication may still be required.")
  }
  if (!identical(s$status,"w01_review_pending"))
    s <- resume_evidence_map(run_name,map=map,step="finish_w01",
                             allow_w01_llm=TRUE)
}
if (identical(s$status,"w01_review_pending"))
  stop("Unresolved duplicate pairs require W01 human adjudication. ",
       "See CHECKPOINTS.md; do not alter archived decision files.")
if (identical(s$status,"awaiting_screening_scope")) {
  message("W01 eligible unique records: ",s$scope$eligible_records)
  if (!isTRUE(abstract_sample_approved))
    stop("Approve abstract_sampling in 00_settings.R after checking counts.")
  # Passing screening= records a sample; it does NOT yet execute W02.
  s <- resume_evidence_map(run_name,map=map,step="enrich",
                           screening=abstract_sampling)
}
if (identical(s$status,"screening_scope_saved"))
  s <- resume_evidence_map(run_name,map=map,step="enrich")
message("Stage 03 status: ",s$status,"; target w03_complete.")
if (!identical(s$status,"w03_complete"))
  message("Do not advance to abstract screening until W02/W03 finish.")
