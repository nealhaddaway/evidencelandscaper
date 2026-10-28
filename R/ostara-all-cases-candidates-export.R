#' Export Ostara cases and original W07 mechanism candidates across both searches
#'
#' Exports source-level rows, not a deduplicated or harmonised summary.
#' Cases retain the separate search and eligibility provenance recorded in
#' the combined evidence atlas. Original W07 candidate mechanism proposals
#' retain their publication and search provenance.
#' @param map Ostara map path from open_map().
#' @param output_dir Directory for the two CSVs.
#' @return Named paths, invisibly.
#' @export
export_ostara_all_cases_candidates <- function(
 map=open_map("Ostara_collective_action_map"),
 output_dir=file.path(map,"reports","all_cases_and_candidates")) {
 map<-normalizePath(map,mustWork=TRUE)
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 combined<-file.path(map,"reports","combined_atlas")
 files<-if(dir.exists(combined))
  list.files(combined,pattern="^case_evidence\\.json$",recursive=TRUE,full.names=TRUE)
 else character()
 files<-files[vapply(files,function(p) {
  z<-tryCatch(jsonlite::fromJSON(p,simplifyVector=FALSE),error=function(e)NULL)
  !is.null(z)&&identical(z$schema,"ostara-combined-search-atlas-v1")&&
  is.list(z$cases)&&length(z$cases)>0L
 },logical(1))]
 if(!length(files))stop("No completed combined-search atlas case_evidence.json found")
 if(length(files)>1L){
  dates<-file.info(file.path(dirname(files),"provenance.json"))$mtime
  if(anyNA(dates))stop("Cannot date multiple combined atlas versions")
  files<-files[which(dates==max(dates))]
  if(length(files)!=1L)stop("Multiple equally recent combined atlases; select one explicitly")
 }
 atlas<-jsonlite::fromJSON(files[[1L]],simplifyVector=FALSE)
 cases<-atlas$cases
 stringify<-function(x){
  if(is.null(x)||!length(x))return("")
  if(is.atomic(x))return(paste(as.character(x),collapse="; "))
  as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null"))
 }
 keys<-sort(unique(unlist(lapply(cases,names),use.names=FALSE)))
 case_rows<-lapply(cases,function(x){
  v<-setNames(vapply(keys,function(k)stringify(x[[k]]),character(1)),keys)
  prov<-x$search_provenance
  c(source_search=stringify(prov$id),
    search_label=stringify(prov$label),
    eligibility_basis=stringify(prov$eligibility_basis),v)
 })
 cases_df<-as.data.frame(do.call(rbind,case_rows),stringsAsFactors=FALSE,
                         check.names=FALSE)
 if(anyDuplicated(cases_df$observation_id))stop("Combined atlas case IDs are not unique")
 if(!all(c("targeted","forest_commodity")%in%unique(cases_df$source_search)))
  stop("Combined atlas does not contain BOTH search sources")
 # The two completed W07 full runs are identified by source-specific corpus
 # sizes, not an arbitrary most-recent ontology; don't blend trial runs.
 root<-file.path(map,"runs","W07")
 runs<-list.dirs(root,recursive=FALSE,full.names=TRUE)
 runs<-runs[grepl("^full-",basename(runs))&
  file.exists(file.path(runs,"coding.jsonl"))&
  file.exists(file.path(runs,"manifest.json"))&
  file.exists(file.path(runs,"summary.json"))]
 selected<-list()
 for(prefix in c("full-170-","full-631-")){
  matches<-runs[startsWith(basename(runs),prefix)]
  if(length(matches)!=1L)stop("Expected exactly one completed W07 ",prefix,
                                " archive; found ",length(matches))
  selected[[prefix]]<-matches[[1L]]
 }
 readjl<-function(p)lapply(Filter(nzchar,readLines(p,warn=FALSE,encoding="UTF-8")),
                      jsonlite::fromJSON,simplifyVector=FALSE)
 candidate_rows<-list()
 for(i in seq_along(selected)){
  folder<-selected[[i]]
  manifest<-jsonlite::fromJSON(file.path(folder,"manifest.json"),
                                simplifyVector=FALSE)
  coding_path<-file.path(folder,"coding.jsonl")
  summ<-jsonlite::fromJSON(file.path(folder,"summary.json"),
                           simplifyVector=FALSE)
  if(!identical(summ$coding_sha256,sha(coding_path)))
   stop("W07 coding checksum mismatch for ",folder)
  src<-if(i==1L)"targeted" else "expanded"
  rows<-readjl(coding_path)
  for(z in rows){
   if(!isTRUE(z$ontology_gap))next
   candidate_rows[[length(candidate_rows)+1L]]<-c(
    candidate_id=paste0("W07-",sprintf("%04d",length(candidate_rows)+1L)),
    source_search=src,record_id=stringify(z$record_id),
    original_description=stringify(z$proposed_new_mechanism),
    original_gap_reason=stringify(z$ontology_gap_reason),
    initiative_development_stage=stringify(z$initiative_stage),
    w07_run=basename(folder),
    w07_coding_sha256=sha(coding_path),
    w07_ontology_sha256=stringify(manifest$ontology_sha256))
  }
 }
 if(!length(candidate_rows))stop("No W07 candidate proposals found")
 candidates_df<-as.data.frame(do.call(rbind,candidate_rows),
                               stringsAsFactors=FALSE,check.names=FALSE)
 # Include the separately detected novel FULL-TEXT proposals, retaining
 # distinct provenance and evidence. Do not collapse into W07 suggestions.
 supplement_root<-file.path(map,"reports","mechanism_review")
 supplement_files<-list.files(supplement_root,
   pattern="^mechanism_assessments\\.jsonl$",recursive=TRUE,full.names=TRUE)
 if(length(supplement_files)>1L) {
   timestamps<-file.info(supplement_files)$mtime
   supplement_files<-supplement_files[which(timestamps==max(timestamps))]
 }
 if(length(supplement_files)==1L){
  extra<-list()
  for(z in readjl(supplement_files[[1L]])) {
   for(j in seq_along(z$new_novelty)){
    v<-z$new_novelty[[j]]
    extra[[length(extra)+1L]]<-data.frame(
      candidate_id=paste0("FT-",z$record_id,"-",sprintf("%03d",j)),
      source_search="expanded_fulltext",
      record_id=stringify(z$record_id),
      original_description=stringify(v$proposed_label),
      original_gap_reason=stringify(v$working_definition),
      initiative_development_stage="",
      w07_run="",w07_coding_sha256="",w07_ontology_sha256="",
      case_ids=stringify(v$case_ids),
      evidence_quote=stringify(v$source_quote),
      source_anchor=stringify(v$source_anchor),
      stringsAsFactors=FALSE)
   }
  }
  if(length(extra)){
   candidates_df$case_ids<-""
   candidates_df$evidence_quote<-""
   candidates_df$source_anchor<-""
   candidates_df<-rbind(candidates_df,do.call(rbind,extra))
  }
 }
 dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
 cases_path<-file.path(output_dir,"all_cases_both_searches.csv")
 candidates_path<-file.path(output_dir,"all_candidate_mechanisms_both_searches.csv")
 utils::write.csv(cases_df,cases_path,row.names=FALSE,na="")
 utils::write.csv(candidates_df,candidates_path,row.names=FALSE,na="")
 message("Exported ",nrow(cases_df)," cases (combined atlas), and ",
         nrow(candidates_df)," original W07 mechanism candidates.")
 message("Cases: ",cases_path,"\nCandidates: ",candidates_path)
 invisible(list(cases=cases_path,candidates=candidates_path))
}
