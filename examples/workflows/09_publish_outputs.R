# STAGE 09: Publish verified case evidence from the named run.
# Does not silently merge cohorts or approve candidate mechanisms.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (!(s$status %in% c("extracted","published")))
  stop("Publication not allowed at status ",s$status,
       "; finish case review and extraction first.")
s <- resume_evidence_map(run_name,map=map,step="publish")
message("Publication status: ",s$status)
message("Two-search candidate exports are SEPARATE verified operations.")
# For an existing combined Ostara atlas:
# exports <- export_ostara_all_cases_candidates(map)
