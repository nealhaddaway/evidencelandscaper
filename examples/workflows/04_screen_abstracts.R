# STAGE 04: W04 title/abstract relevance screening and finalisation.
# Requires a verified prompt and completed W01-W03.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (s$status %in% c("w03_complete","screening"))
  s <- resume_evidence_map(run_name,map=map,step="screen",
                           screening_prompt=screening_prompt,model=model)
if (identical(s$status,"w04_screened")) {
  message("Review W04 decisions and uncertainty in: ",s$w04$run)
  if (!isTRUE(abstract_finalisation_approved))
    stop("Review W04 outputs, adjudicate as needed, then set ",
         "abstract_finalisation_approved=TRUE and rerun this script.")
  s <- resume_evidence_map(run_name,map=map,step="finalize_screen")
}
message("Stage 04 status: ",s$status,"; target w04_final.")
