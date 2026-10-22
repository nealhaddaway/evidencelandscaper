#' Compare the most recent isolated W04 abstract rescreen with original consensus
#' @param w04 Optional full W04 path; auto-detected from Ostara project.
#' @return Data frame of affected records with old/new decisions.
#' @export
compare_w04_rescreen <- function(w04=NULL) {
 if(is.null(w04)) {
   map<-open_map("Ostara_collective_action_map")
   roots<-list.dirs(file.path(map,"runs","W01"),recursive=FALSE,full.names=TRUE)
   runs<-unlist(lapply(roots,function(root) {
     p<-file.path(root,"W04_local")
     if(dir.exists(p))list.dirs(p,recursive=FALSE,full.names=TRUE) else character()
   }),use.names=FALSE)
   runs<-runs[grepl("^full-",basename(runs)) &
      dir.exists(file.path(runs,"abstract_rescreens"))]
   if(length(runs)!=1L)stop("Expected one full W04 run with rescreens; supply w04.")
   w04<-runs[[1L]]
 }
 w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
 base<-file.path(w04,"abstract_rescreens")
 if(!dir.exists(base))stop("No rescreens.")
 candidates<-list.dirs(base,recursive=FALSE,full.names=TRUE)
 candidates<-candidates[file.exists(file.path(candidates,"manifest.json"))]
 complete<-candidates[vapply(candidates,function(p) {
   m<-jsonlite::fromJSON(file.path(p,"manifest.json"))
   identical(m$status,"completed")
 },logical(1))]
 if(length(complete)!=1L)stop("Expected one completed rescreen; found ",length(complete),".")
 rescreen<-complete[[1L]]
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 manifest<-jsonlite::fromJSON(file.path(rescreen,"manifest.json"),simplifyVector=FALSE)
 oldfile<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
 newfile<-file.path(rescreen,"consensus","workflow04_consensus_layer.jsonl")
 if(!identical(manifest$source_w04_sha256,sha(oldfile)) ||
    !identical(manifest$consensus_sha256,sha(newfile)))
   stop("Rescreen provenance mismatch.")
 readjl<-function(p) {
   v<-readLines(p,warn=FALSE,encoding="UTF-8")
   lapply(v[nzchar(trimws(v))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 old<-readjl(oldfile);new<-readjl(newfile)
 oldmap<-setNames(old,vapply(old,function(x)x$record_id,character(1)))
 ids<-vapply(new,function(x)x$record_id,character(1))
 if(anyDuplicated(ids)||any(!ids%in%names(oldmap)))stop("Rescreen IDs do not reconcile.")
 status<-function(x)as.character(x$screening$decision)
 result<-data.frame(record_id=ids,
   previous=vapply(ids,function(id)status(oldmap[[id]]),character(1)),
   rescreened=vapply(new,status,character(1)),
   stringsAsFactors=FALSE)
 result$changed<-result$previous!=result$rescreened
 print(result,row.names=FALSE)
 message("[W04] ",sum(result$changed)," decisions changed. No results have been merged.")
 invisible(result)
}
