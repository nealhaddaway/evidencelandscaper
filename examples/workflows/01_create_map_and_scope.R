# STAGE 01: Create/open a map, archive a named W00 strategy, count hits.
# Use the .Rproj so this source() resolves 00_settings.R.
# No bibliographic records or LLM outputs are downloaded here.
source("00_settings.R")
if (!file.exists(strategy_file)) {
  template <- system.file("w00","example-strategy.json",
                          package="evidencelandscaper")
  if (!nzchar(template) || !file.copy(template,strategy_file))
    stop("Cannot create the example JSON strategy at: ",strategy_file)
  message("Created ILLUSTRATIVE strategy at ",strategy_file)
}
if (!isTRUE(strategy_approved))
  stop("Edit and check the JSON strategy first, then set ",
       "strategy_approved=TRUE in 00_settings.R and rerun stage 01.")
strategy <- jsonlite::fromJSON(strategy_file,simplifyVector=FALSE)
stopifnot(is.list(strategy$concepts),is.character(strategy$expression))
print(strategy)
state_path <- file.path(map,"runs","orchestrated",run_name,"state.json")
if (!file.exists(state_path)) {
  run_evidence_map(map=map,run_name=run_name,search_file=strategy_file,
                   sources=sources,execute_scoping=FALSE,model=model,
                   w01_uncertain=w01_uncertain,
                   screening_prompt=screening_prompt,w07_prompt=w07_prompt,
                   ontology_file=ontology_file,w08_prompt=w08_prompt,
                   manual_ris=manual_ris,
                   manual_search_strings=manual_search_strings)
}
s <- resume_evidence_map(run_name,map=map,step="status")
if (identical(s$status,"planned"))
  s <- resume_evidence_map(run_name,map=map,step="scope",sources=sources)
if (!is.null(s$scope$counts_path) && file.exists(s$scope$counts_path)) {
  counts <- utils::read.csv(s$scope$counts_path,stringsAsFactors=FALSE)
  print(counts)
}
message("Status: ",s$status,". These are GROSS database hits.")
