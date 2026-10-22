#' Build a non-destructive reconciled W04 review queue after abstract rescreening
#' @param w04 Full W04 folder; defaults to the sole full project run.
#' @return Reconciled review queue path invisibly.
#' @export
prepare_w04_reconciled_review <- function(w04=NULL) {
 if(is.null(w04)) {
  map<-open_map("Ostara_collective_action_map")
  roots<-list.dirs(file.path(map,"runs","W01"),recursive=FALSE,full.names=TRUE)
  all<-unlist(lapply(roots,function(p){d<-file.path(p,"W04_local");if(dir.exists(d))list.dirs(d,recursive=FALSE,full.names=TRUE) else character()}),use.names=FALSE)
  all<-all[grepl("^full-",basename(all)) & dir.exists(file.path(all,"abstract_rescreens"))]
  if(length(all)!=1L)stop("Expected one full W04 run with rescreens.")
  w04<-all[[1L]]
 }
 w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
 base<-file.path(w04,"abstract_rescreens")
 dirs<-list.dirs(base,recursive=FALSE,full.names=TRUE)
 dirs<-dirs[file.exists(file.path(dirs,"manifest.json"))]
 dirs<-dirs[vapply(dirs,function(x)identical(jsonlite::fromJSON(file.path(x,"manifest.json"))$status,"completed"),logical(1))]
 if(length(dirs)!=1L)stop("Expected exactly one completed selective rescreen.")
 source<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
 changed<-file.path(dirs[[1]],"consensus","workflow04_consensus_layer.jsonl")
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 meta<-jsonlite::fromJSON(file.path(dirs[[1]],"manifest.json"),simplifyVector=FALSE)
 if(!identical(meta$source_w04_sha256,sha(source))||!identical(meta$consensus_sha256,sha(changed)))stop("Rescreen hash mismatch.")
 readjl<-function(p){z<-readLines(p,warn=FALSE);lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)}
 orig<-readjl(source);replacement<-readjl(changed)
 ids<-vapply(orig,function(x)x$record_id,character(1))
 mids<-vapply(replacement,function(x)x$record_id,character(1))
 if(anyDuplicated(ids)||anyDuplicated(mids)||any(!mids%in%ids))stop("Rescreen IDs invalid.")
 if(any(vapply(replacement,function(x)!identical(x$screening$decision,"retain"),logical(1))))stop("Not all rescreen decisions are retain; manual reconciliation required.")
 orig_dec<-vapply(orig,function(x)x$screening$decision,character(1))
 if(any(orig_dec[match(mids,ids)]!="uncertain"))stop("Target record was not originally uncertain.")
 reviewed<-file.path(w04,"human_review_returned","receipt.json")
 if(file.exists(reviewed))stop("Existing human review receipt; do not silently supersede adjudications.")
 for(i in seq_along(mids)){
   j<-match(mids[[i]],ids)
   replacement[[i]]$screening$decision_origin<-"selective_abstract_rescreen"
   replacement[[i]]$screening$original_screening_input_sha256<-orig[[j]]$screening$screening_input_sha256
   replacement[[i]]$screening$original_consensus_sha256<-sha(source)
   orig[[j]]<-replacement[[i]]
 }
 dest<-file.path(w04,"reconciled",paste0("abstract-rescreen-",substr(sha(changed),1,12)))
 if(dir.exists(dest))stop("Reconciled output already exists; preserve previous files: ",dest)
 dir.create(dest,recursive=TRUE,showWarnings=FALSE)
 out<-file.path(dest,"workflow04_reconciled_layer.jsonl")
 writeLines(vapply(orig,function(x)jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),character(1)),out,useBytes=TRUE)
 remaining<-sum(vapply(orig,function(x)identical(x$screening$decision,"uncertain"),logical(1)))
 jsonlite::write_json(list(schema="local-w04-reconciled-v1",status="prepared",
   source_w04_sha256=sha(source),rescreen_sha256=sha(changed),merged_sha256=sha(out),
   substituted_records=as.list(mids),remaining_uncertain=remaining),
   file.path(dest,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
 message("[W04] Five corrected retain decisions reconciled in a NEW layer; ",remaining," uncertain records remain.")
 message("[W04] Original W04 consensus and human decisions have not been touched.")
 invisible(dest)
}
