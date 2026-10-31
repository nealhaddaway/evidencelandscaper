# STAGE 05: W07 ontology coding AND W08 abstract case identification.
# They are one orchestrated step, 'code', in the current package.
source("00_settings.R")
s <- resume_evidence_map(run_name,map=map,step="status")
if (s$status %in% c("w04_final","coding"))
  s <- resume_evidence_map(run_name,map=map,step="code",model=model,
                           w07_prompt=w07_prompt,ontology_file=ontology_file,
                           w08_prompt=w08_prompt)
message("Stage 05 status: ",s$status,"; target awaiting_extraction_scope.")
if (identical(s$status,"awaiting_extraction_scope")) {
  message("W07 output: ",s$w07$run)
  message("W08 output: ",s$w08$run)
  message("Case-bearing publications: ",s$case_bearing_publications)
  message("Unclear case assessments: ",s$uncertain_case_publications)
  message("Review those outcomes before selecting full texts in stage 06.")
}
