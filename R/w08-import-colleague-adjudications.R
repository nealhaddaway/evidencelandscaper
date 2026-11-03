#' Import an offline W08 colleague's human case-detection decisions
#'
#' Verifies the review history against the exact W08 review queue checksum,
#' using the original review_queue.jsonl if supplied, otherwise reproducibly
#' rebuilding the 93-unclear review queue from the saved W08 source.
#' Writes an immutable, versioned human adjudication layer. Original model
#' responses, W04 and W02 archives are never modified.
#'
#' @param decisions_file Exported ostara_w08_review_history.jsonl; if NULL,
#'   choose the file interactively.
#' @param w08 Exact W08 run. If NULL, uniquely discover a matching human-
#'   authoritative first-10-percent W08 run.
#' @param review_queue_file Optional original bundle data/review_queue.jsonl.
#' @param map_path Ostara map root, if not using the default map.
#' @return Immutable W08 adjudication snapshot directory, invisibly.
#' @export
import_w08_colleague_adjudications <- function(
    decisions_file=NULL,w08=NULL,review_queue_file=NULL,map_path=NULL) {
  if(is.null(decisions_file)) {
    if(!interactive())stop("Provide decisions_file for non-interactive import.")
    message("[W08 adjudication] Select Diana's exported review HISTORY (.jsonl).")
    decisions_file<-file.choose()
  }
  decisions_file<-normalizePath(decisions_file,winslash="/",mustWork=TRUE)
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x)if(is.character(x)&&length(x)==1L&&!is.na(x))x else ""
  events<-readjl(decisions_file)
  if(!length(events))stop("Empty W08 human decision history.")
  schemas<-vapply(events,function(x)scalar(x$schema),character(1))
  qhash<-unique(vapply(events,function(x)scalar(x$queue_sha256),character(1)))
  valid_statuses<-c("specific_case","multiple_cases","no_identifiable_case","unclear")
  if(any(schemas!="ostara-w08-colleague-decision-v1")||
     length(qhash)!=1L||!grepl("^[a-f0-9]{64}$",qhash))
    stop("W08 human history has invalid schema or mixed queue checksums.")
  if(any(!vapply(events,function(x)
      nzchar(scalar(x$record_id))&&nzchar(scalar(x$reviewer))&&
      x$decision%in%c("accept","reject")&&
      x$final_case_status%in%valid_statuses&&
      nzchar(scalar(x$created_at_utc)),logical(1))))
    stop("Incomplete/invalid W08 human decisions; no import attempted.")
  if(is.null(w08)) {
    root<-file.path(map_path,"runs","W08")
    dirs<-if(dir.exists(root))list.dirs(root,recursive=FALSE,full.names=TRUE)
           else character()
    eligible<-Filter(function(p) {
      f<-file.path(p,"manifest.json")
      if(!file.exists(f))return(FALSE)
      m<-tryCatch(jsonlite::fromJSON(f,simplifyVector=FALSE),
                  error=function(e)NULL)
      !is.null(m)&&identical(m$source_w04_layer,"human_authoritative")&&
        startsWith(basename(m$source_w04),"selected-")&&
        file.exists(file.path(p,"case_assessments.jsonl"))
    },dirs)
    if(length(eligible)!=1L)
      stop("Expected exactly one first-10% W08 run; found ",
           length(eligible),". Supply w08 explicitly.")
    w08<-eligible[[1L]]
  }
  w08<-normalizePath(w08,winslash="/",mustWork=TRUE)
  expected_dir<-file.path(map_path,"runs","W08")
  if(!identical(dirname(w08),normalizePath(expected_dir,mustWork=TRUE)))
    stop("W08 is not inside this map's runs/W08 directory.")
  mandatory<-file.path(w08,c("manifest.json","summary.json",
       "case_assessments.jsonl","prompt.txt","fields.csv"))
  if(!all(file.exists(mandatory)))stop("W08 source archive is incomplete.")
  m<-jsonlite::fromJSON(mandatory[[1L]],simplifyVector=FALSE)
  summary<-jsonlite::fromJSON(mandatory[[2L]],simplifyVector=FALSE)
  if(!identical(m$source_w04_layer,"human_authoritative")||
     !startsWith(basename(m$source_w04),"selected-")||
     !summary$status%in%c("PILOT_COMPLETE","PILOT_PARTIAL")||
     !identical(summary$responses_sha256,sha(mandatory[[3L]]))||
     !identical(m$prompt_sha256,sha(mandatory[[4L]]))||
     !identical(m$schema_sha256,sha(mandatory[[5L]])))
    stop("The selected W08 first-10% source or its checksums are invalid.")
  all<-readjl(mandatory[[3L]])
  ids<-vapply(all,function(x)scalar(x$record_id),character(1))
  selected<-as.character(unlist(m$selected_record_ids,use.names=FALSE))
  if(!length(ids)||anyDuplicated(ids)||anyDuplicated(selected)||
     any(!ids%in%selected)||length(selected)!=as.integer(m$sample_size))
    stop("W08 source selections and assessments are inconsistent.")
  missing<-setdiff(selected,ids)
  if(identical(summary$status,"PILOT_COMPLETE")&&length(missing))
    stop("Completed W08 run unexpectedly contains unvalidated publications.")
  if(identical(summary$status,"PILOT_PARTIAL")&&
     (!setequal(missing,as.character(unlist(summary$unresolved_record_ids,
      use.names=FALSE)))||length(ids)!=as.integer(summary$validated_records)))
    stop("W08 partial validation summary does not match saved assessments.")
  status<-vapply(all,function(x)scalar(x$case_evidence_status),character(1))
  if(any(!status%in%valid_statuses))
    stop("W08 source contains unknown model case statuses.")
  for(x in all).w08_validate_case_count(x$case_evidence_status,x$cases,
                                         x$record_id)
  # Source W04 and canonical provenance are checked inside export, which
  # rebuilds the exact review_queue.jsonl if the original is not available.
  temp<-NULL
  if(is.null(review_queue_file)) {
    temp<-tempfile("w08-import-review-")
    dir.create(temp)
    on.exit(unlink(temp,recursive=TRUE,force=TRUE),add=TRUE)
    zip<-file.path(temp,"w08-review.zip")
    export_w08_colleague_review(
      w08=w08,destination_zip=zip,
      allow_partial=identical(summary$status,"PILOT_PARTIAL"),
      review_statuses="unclear")
    utils::unzip(zip,files="w08_review_bundle/data/review_queue.jsonl",
                 exdir=temp)
    review_queue_file<-file.path(temp,"w08_review_bundle","data",
                                 "review_queue.jsonl")
    if(!file.exists(review_queue_file))
      stop("Could not reconstruct the archived W08 review queue.")
  } else review_queue_file<-normalizePath(review_queue_file,
                                         winslash="/",mustWork=TRUE)
  if(!identical(sha(review_queue_file),qhash[[1L]]))
    stop("Human review history checksum does not match the W08 review queue. ",
         "Supply the original bundle's data/review_queue.jsonl, or select ",
         "the matching W08 source run.")
  queue<-readjl(review_queue_file)
  qids<-vapply(queue,function(x)scalar(x$record_id),character(1))
  if(anyDuplicated(qids)||any(!qids%in%ids)||
     any(!vapply(queue,function(x)
       identical(scalar(x$detection$case_evidence_status),"unclear"),
       logical(1))))
    stop("Human-review queue is not a unique subset of W08 unclear records.")
  if(!setequal(qids,ids[status=="unclear"]))
    stop("Review queue does not cover exactly the unclear W08 assessments.")
  reviewed<-list()
  for(e in events) {
    id<-scalar(e$record_id)
    if(!id%in%qids)stop("Human decision is not in the matching review queue: ",id)
    # Append-only edits are permitted; the last event in file order wins.
    reviewed[[id]]<-e
  }
  if(!setequal(names(reviewed),qids))
    stop("Human adjudication is incomplete: ",
         length(qids)-length(reviewed)," unclear records have no final event.")
  index<-setNames(all,ids)
  for(id in names(reviewed)) {
    e<-reviewed[[id]]
    original_count<-length(index[[id]]$cases)
    if(!is.list(e$case_decisions)||
       length(e$case_decisions)!=original_count)
      stop("Candidate-level review count is inconsistent for ",id)
    per<-as.character(unlist(e$case_decisions,use.names=FALSE))
    if(any(!per%in%c("","accept","reject","unclear"))||
       (any(nzchar(per))&&!all(nzchar(per))))
      stop("Invalid/incomplete candidate-level decisions for ",id)
  }
  human_hash<-sha(decisions_file)
  key<-substr(digest::digest(c(sha(mandatory[[3L]]),qhash[[1L]],
                              human_hash),algo="sha256"),1L,12L)
  root<-file.path(w08,"human_adjudication")
  dest<-file.path(root,paste0("v1-",key))
  manifest<-list(schema="w08-human-adjudication-v1",
    source_w08=w08,source_w08_sha256=sha(mandatory[[3L]]),
    source_w08_manifest_sha256=sha(mandatory[[1L]]),
    source_w04_layer_sha256=m$source_w04_sha256,
    review_queue_sha256=qhash[[1L]],human_decisions_sha256=human_hash,
    reviewed_record_ids=as.list(sort(qids)),reviewed_records=length(qids),
    original_validated_records=length(ids),original_unvalidated_ids=as.list(missing))
  mf<-file.path(dest,"manifest.json")
  if(file.exists(mf)) {
    previous<-jsonlite::fromJSON(mf,simplifyVector=FALSE)
    if(!identical(previous,manifest))
      stop("Existing W08 human adjudication snapshot has different provenance.")
    if(!identical(sha(file.path(dest,"review_history.jsonl")),human_hash))
      stop("Stored human review history changed.")
    message("[W08 adjudication] Verified existing human snapshot: ",dest)
    return(invisible(dest))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Adjudication destination exists without a valid manifest.")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(decisions_file,file.path(dest,"review_history.jsonl"),
                overwrite=FALSE)||
     !file.copy(review_queue_file,file.path(dest,"review_queue.jsonl"),
                overwrite=FALSE))
    stop("Unable to preserve the original human review evidence.")
  effective<-do.call(rbind,lapply(all,function(x) {
    id<-x$record_id
    e<-reviewed[[id]]
    data.frame(record_id=id,model_case_status=x$case_evidence_status,
      final_case_status=if(is.null(e))x$case_evidence_status else
        e$final_case_status,
      decision_origin=if(is.null(e))"W08_model" else "human_adjudicated",
      reviewer=if(is.null(e))"" else e$reviewer,
      review_decision=if(is.null(e))"" else e$decision,
      reviewed_at_utc=if(is.null(e))"" else e$created_at_utc,
      model_candidate_count=length(x$cases),
      notes=if(is.null(e))"" else scalar(e$notes),
      alternative_cases=if(is.null(e))"" else scalar(e$alternative_cases),
      stringsAsFactors=FALSE)
  }))
  utils::write.csv(effective,file.path(dest,"effective_case_classifications.csv"),
                   row.names=FALSE,na="")
  utils::write.csv(data.frame(record_id=missing,
     status=rep("unvalidated",length(missing))),
     file.path(dest,"unvalidated_publications.csv"),row.names=FALSE)
  manifest$effective_case_classifications_sha256<-
    sha(file.path(dest,"effective_case_classifications.csv"))
  jsonlite::write_json(manifest,mf,pretty=TRUE,auto_unbox=TRUE)
  message("[W08 adjudication] Imported ",length(qids)," human classifications; ",
      sum(effective$final_case_status=="no_identifiable_case")," final no-case; ",
      sum(effective$final_case_status=="specific_case")," single-case; ",
      sum(effective$final_case_status=="multiple_cases")," multi-case; ",
      sum(effective$final_case_status=="unclear")," unclear; ",
      length(missing)," W08 unvalidated.")
  message("[W08 adjudication] Effective classifications: ",
       file.path(dest,"effective_case_classifications.csv"))
  invisible(dest)
}
