# STAGE 06: Approve W08 candidate cohort, retrieve and verify full texts.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (identical(s$status,"awaiting_extraction_scope")) {
  message("Case-bearing publications: ",s$case_bearing_publications,
          " | unclear assessments: ",s$uncertain_case_publications)
  if (!isTRUE(case_sample_approved))
    stop("Inspect W08 cases, approve case_sampling in 00_settings.R, rerun.")
  # Saves the exact cohort only; W09 is launched by the next call.
  s <- resume_evidence_map(run_name,map=map,step="prepare_extraction",
                           extraction=case_sampling)
}
if (identical(s$status,"extraction_scope_saved"))
  s <- resume_evidence_map(run_name,map=map,step="prepare_extraction")
if (s$status %in% c("w09_prepared","retrieving_fulltexts","fulltexts_ready"))
  s <- resume_evidence_map(run_name,map=map,step="retrieve_fulltexts")
message("Stage 06 status: ",s$status,"; target fulltexts_ready.")
if (!is.null(s$w09$run)) message("W09 run: ",s$w09$run)
if (!is.null(s$w09$inventory)) message("Verified inventory: ",s$w09$inventory)
