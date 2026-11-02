#' Prepare a case-positive W09 full-text retrieval queue from provisional W08
#'
#' Selects publications with at least one case candidate in validated W08
#' assessments. Missing/invalid W08 results remain explicitly unresolved.
#' The original W04, W08 and canonical W02 data remain read-only.
#' @param w08 W08 run directory.
#' @param include_unclear Include candidate-positive unclear assessments.
#' @return Prepared W09 directory, invisibly.
#' @export
prepare_w09_w08_case_fulltexts <- function(w08,include_unclear=TRUE) {
  w08<-normalizePath(w08,winslash="/",mustWork=TRUE)
  if(!identical(basename(dirname(w08)),"W08") ||
     !identical(basename(dirname(dirname(w08))),"runs"))
    stop("W08 directory must be under map/runs/W08.")
  map<-dirname(dirname(dirname(w08)))
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p){
    z<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  if(!is.logical(include_unclear)||length(include_unclear)!=1L||
     is.na(include_unclear))stop("include_unclear must be TRUE or FALSE.")
  files<-file.path(w08,c("manifest.json","summary.json",
    "case_assessments.jsonl","prompt.txt","fields.csv"))
  if(!all(file.exists(files)))stop("Incomplete W08 provenance files.")
  m<-jsonlite::fromJSON(files[[1L]],simplifyVector=FALSE)
  sm<-jsonlite::fromJSON(files[[2L]],simplifyVector=FALSE)
  if(!sm$status%in%c("PILOT_COMPLETE","PILOT_PARTIAL")||
     !identical(sm$responses_sha256,sha(files[[3L]]))||
     !identical(m$prompt_sha256,sha(files[[4L]]))||
     !identical(m$schema_sha256,sha(files[[5L]])))
    stop("W08 status or checksum mismatch.")
  records<-readjl(files[[3L]])
  ids<-vapply(records,function(z)as.character(z$record_id),character(1))
  selected<-as.character(unlist(m$selected_record_ids,use.names=FALSE))
  missing<-setdiff(selected,ids)
  if(!length(ids)||anyDuplicated(ids)||anyDuplicated(selected)||
     any(!ids%in%selected)||length(selected)!=as.integer(m$sample_size))
    stop("W08 selected and validated IDs inconsistent.")
  if(identical(sm$status,"PILOT_COMPLETE")&&length(missing))
    stop("Completed W08 contains missing responses.")
  if(identical(sm$status,"PILOT_PARTIAL")&&
     (!length(missing)||
      !setequal(missing,unlist(sm$unresolved_record_ids,use.names=FALSE))||
      length(ids)!=as.integer(sm$validated_records)))
    stop("W08 partial status does not match unresolved IDs.")
  for(z in records) {
    if(is.null(z$cases)||!is.list(z$cases)||
       !z$case_evidence_status%in%c("specific_case","multiple_cases",
          "no_identifiable_case","unclear"))
      stop("Invalid W08 case assessment: ",z$record_id)
    .w08_validate_case_count(z$case_evidence_status,z$cases,z$record_id)
  }
  w04<-normalizePath(m$source_w04,winslash="/",mustWork=TRUE)
  provisional<-isTRUE(m$provisional_retains)
  layer<-file.path(w04,if(provisional)"consensus" else "finalised",
    if(provisional)"workflow04_consensus_layer.jsonl" else
      "workflow04_final_screening_layer.jsonl")
  if(!file.exists(layer)||!identical(sha(layer),m$source_w04_sha256))
    stop("Source W04 consensus/finalised layer changed.")
  retain<-vapply(Filter(function(z)
    identical(z$screening$decision,"retain"),readjl(layer)),
    function(z)as.character(z$record_id),character(1))
  if(any(!selected%in%retain))stop("W08 selected non-retained W04 publications.")
  canonical<-file.path(dirname(dirname(w04)),"W02_main","finalised","canonical.jsonl")
  if(!file.exists(canonical)||!identical(sha(canonical),m$canonical_w02_sha256))
    stop("Canonical W02 source checksum mismatch.")
  crows<-readjl(canonical)
  cids<-vapply(crows,function(z)as.character(z$identity$record_id),character(1))
  if(anyDuplicated(cids)||any(!selected%in%cids))
    stop("W08 IDs cannot be uniquely mapped to W02 bibliographic records.")
  byid<-setNames(crows,cids)
  candidates<-Filter(function(z)length(z$cases)>0L&&
    (include_unclear||!identical(z$case_evidence_status,"unclear")),records)
  if(!length(candidates))stop("No case-positive W08 publications.")
  candidate_ids<-vapply(candidates,function(z)z$record_id,character(1))
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  queue<-do.call(rbind,lapply(candidate_ids,function(id){
    z<-byid[[id]]$canonical
    data.frame(record_id=id,title=scalar(z$title),doi=scalar(z$doi),
      pdf_path="",retrieval_status="not_attempted",notes="",
      stringsAsFactors=FALSE)
  }))
  evidence<-do.call(rbind,lapply(candidates,function(z){
    data.frame(record_id=z$record_id,case_evidence_status=z$case_evidence_status,
      candidate_count=length(z$cases),fulltext_priority=scalar(z$fulltext_priority),
      stringsAsFactors=FALSE)
  }))
  dest<-file.path(map,"runs","W09",paste0("from-w08-cases-",
    length(candidate_ids),"-",substr(sha(files[[3L]]),1,12),
    if(include_unclear)"-with-unclear" else "-without-unclear"))
  provenance<-list(schema="w09-from-provisional-w08-cases-v1",
    source_w08=w08,w08_assessments_sha256=sha(files[[3L]]),
    source_w04=w04,source_w04_layer_sha256=sha(layer),
    canonical_w02_sha256=sha(canonical),
    include_unclear=include_unclear,
    selected_record_ids=as.list(candidate_ids),
    unresolved_w08_record_ids=as.list(missing),
    source_w08_partial=length(missing)>0L)
  mf<-file.path(dest,"manifest.json")
  if(file.exists(mf)) {
    old<-jsonlite::fromJSON(mf,simplifyVector=FALSE)
    for(k in names(provenance))
      if(!identical(old[[k]],provenance[[k]]))
        stop("W09 handover provenance conflict: ",k)
    if(!file.exists(file.path(dest,"retrieval_queue.csv"))||
       !identical(utils::read.csv(file.path(dest,"retrieval_queue.csv"),
          stringsAsFactors=FALSE),queue))
      stop("Existing W09 retrieval queue changed.")
    message("[W09] Verified existing case-positive retrieval queue: ",dest)
    return(invisible(dest))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("W09 target exists without provenance manifest.")
  prompt<-system.file("w09","fulltext_case_prompt_v1.txt",
                      package="evidencelandscaper")
  if(!nzchar(prompt))stop("Packaged W09 prompt missing.")
  dir.create(file.path(dest,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(dest,"structured"),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(queue,file.path(dest,"retrieval_queue.csv"),row.names=FALSE)
  utils::write.csv(evidence,file.path(dest,"w08_case_candidates.csv"),row.names=FALSE)
  utils::write.csv(data.frame(record_id=missing),
    file.path(dest,"unresolved_w08_records.csv"),row.names=FALSE)
  if(!file.copy(prompt,file.path(dest,"prompt.txt")))
    stop("Cannot archive W09 extraction prompt.")
  jsonlite::write_json(provenance,mf,auto_unbox=TRUE,pretty=TRUE)
  message("[W09] Prepared ",length(candidate_ids)," case-positive publications.",
    " Unresolved W08 records not classified: ",length(missing),".")
  invisible(dest)
}
