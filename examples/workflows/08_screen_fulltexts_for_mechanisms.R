# STAGE 08: Full-text mechanism extraction for the named search.
# Uses the SAME checksum-verified, human-approved W09 case documents as
# stage 07. Does not depend on a legacy W10 run and does not change W09.
# Supplementary LLM calls are billed. Completed records are checkpointed.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (!(s$status %in% c("extracted","published")))
  stop("Finish stage 07 (human-approved full-text case eligibility ",
       "and W09 extraction) before stage 08. Current status: ",s$status)

# Construct a provenance-verified review baseline with NO auto-approval.
# Reuses an identical existing review, preserving edits.
if (is.null(mechanism_review_dir))
  mechanism_review_dir <- prepare_ostara_mechanism_review_from_run(
    run_name=run_name,map=map)

message("Mechanism source review: ", mechanism_review_dir)
if (!dir.exists(mechanism_review_dir))
  stop("Cannot locate mechanism review: ",mechanism_review_dir)

# One model assessment per eligible publication, checking EVERY ontology code.
# max_records=0 means all; completed work resumes without duplicate calls.
mechanism_supplement_dir <- extract_ostara_fulltext_mechanisms(
  review_dir=mechanism_review_dir,max_records=0L,model=model)
message("Mechanism extraction: ",mechanism_supplement_dir)

# Make review worksheets once; never overwrite human edits.
existing <- list.dirs(mechanism_review_dir,recursive=FALSE,
                      full.names=TRUE)
existing <- existing[startsWith(basename(existing),"supplement_review-")]
if (length(existing)) {
  message("Existing supplementary review(s), preserving edits:\n",
          paste(existing,collapse="\n"))
} else {
  worksheets <- prepare_ostara_supplement_review(
    review_dir=mechanism_review_dir,
    supplement_dir=mechanism_supplement_dir)
  print(worksheets)
}
message("Review mechanism statuses, evidence anchors and novelty proposals ",
        "before accepting findings or publishing.")
