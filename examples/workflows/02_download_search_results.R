# STAGE 02: W00 retrieval (API calls; may take time).
# First complete 01 and approve its count/source selection.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (s$status %in% c("scoped_counts_only","retrieving"))
  s <- resume_evidence_map(run_name,map=map,step="retrieve")
if (identical(s$status,"retrieving"))
  stop("W00 is incomplete. Inspect source logs and retrieval manifest; ",
       "rerun this stage to resume where supported, not W01.")
if (identical(s$status,"retrieved") && !is.null(manual_ris))
  s <- resume_evidence_map(run_name,map=map,step="import_manual")
if (identical(s$status,"manual_importing"))
  stop("Manual RIS import incomplete. Inspect and retry.")
message("W00 status: ",s$status)
if (!is.null(s$w00$retrieval_manifest))
  message("Retrieval manifest: ",s$w00$retrieval_manifest)
