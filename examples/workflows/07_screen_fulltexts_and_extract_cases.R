# STAGE 07: New eligibility-FIRST case screening then W09 extraction.
# DO NOT run legacy post-extraction W10 for a newly created named run.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (s$status %in% c("fulltexts_ready","assessing_fulltexts"))
  s <- resume_evidence_map(run_name,map=map,step="assess_fulltexts",model=model)
if (identical(s$status,"preextraction_verified")) {
  message("Eligibility review: ",s$preextraction$run)
  message("Inspect candidate_case_eligibility.csv and resolve human decisions.")
  if (!isTRUE(fulltext_eligibility_review_complete))
    stop("Complete actual human review first; set ",
         "fulltext_eligibility_review_complete=TRUE and rerun.")
  s <- resume_evidence_map(run_name,map=map,step="finalize_eligibility")
}
if (s$status %in% c("preextraction_final","extracting"))
  s <- resume_evidence_map(run_name,map=map,step="extract",model=model)
message("Stage 07 status: ",s$status,"; target extracted.")
if (!is.null(s$w09$extraction_dir))
  message("W09 extraction: ",s$w09$extraction_dir)
