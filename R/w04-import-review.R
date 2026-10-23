#' Import a returned W04 human review export
#'
#' Validates queue identity, record fingerprints, prompt provenance and reviewer
#' attribution. Preserves the raw export and writes an immutable receipt. Does
#' not modify original consensus or finalise W04.
#' @param w04 Path returned by run_w04().
#' @param decisions_file Reviewer's exported decisions JSONL. If NULL, reads the
#'   locally saved reviewer_state/decisions.jsonl events.
#' @param include_all Whether importing optional whole-trial human validation.
#' @return Import receipt path, invisibly.
#' @export
import_w04_review <- function(w04, decisions_file=NULL, include_all=FALSE) {
  w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
  queue<-prepare_w04_review(w04,include_all=include_all)
  if(is.null(queue))stop("There are no W04 review cases to import.")
  reconciled<-identical(basename(queue),"reconciled_human_review_queue.jsonl")
  if(is.null(decisions_file))
    decisions_file<-file.path(w04,if(include_all)"validation_reviewer_state" else
                             if(reconciled)"reconciled_reviewer_state" else "reviewer_state","decisions.jsonl")
  decisions_file<-normalizePath(decisions_file,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x) if(is.null(x)||length(x)!=1L||is.na(x)||!is.character(x))
    "" else x
  cases<-readjl(queue);events<-readjl(decisions_file)
  if(!length(events))stop("The returned W04 decisions file is empty.")
  qids<-vapply(cases,function(x)scalar(x$review_case_id),character(1))
  if(anyDuplicated(qids))stop("Review queue contains duplicate case identifiers.")
  lookup<-setNames(cases,qids)
  final<-list()
  for(e in events) {
    id<-scalar(e$review_case_id)
    if(!identical(scalar(e$schema),"local-w04-review-decision-v1") ||
       !nzchar(id) || !id%in%qids ||
       !identical(scalar(e$queue_sha256),sha(queue)) ||
       !identical(scalar(e$record_id),scalar(lookup[[id]]$record_id)) ||
       !identical(scalar(e$record_input_sha256),
                  scalar(lookup[[id]]$model$screening_input_sha256)) ||
       !identical(scalar(e$model_prompt_sha256),
                  scalar(lookup[[id]]$model$prompt_sha256)))
      stop("Invalid or mismatched W04 human decision: ",id)
    if(!scalar(e$decision)%in%c("retain","exclude","uncertain") ||
       !nzchar(trimws(scalar(e$reviewer))))
      stop("Invalid review decision, reviewer or rationale: ",id)
    # Last event for a case is authoritative. Earlier edits remain in raw file.
    final[[id]]<-e
  }
  if(!setequal(names(final),qids))
    stop("Review return incomplete: ",length(final)," of ",length(qids),
         " decisions. All assigned cases must be completed.")
  dest<-file.path(w04,if(include_all)"validation_returned" else if(reconciled)"reconciled_human_review_returned" else "human_review_returned")
  receipt<-file.path(dest,"receipt.json")
  export_copy<-file.path(dest,"reviewer_export.jsonl")
  if(file.exists(receipt)) {
    prev<-jsonlite::fromJSON(receipt,simplifyVector=FALSE)
    if(!identical(scalar(prev$raw_export_sha256),sha(decisions_file)) ||
       !identical(scalar(prev$queue_sha256),sha(queue)) ||
       !file.exists(export_copy) || !identical(sha(export_copy),sha(decisions_file)))
      stop("A different W04 review return has already been imported. Existing return preserved.")
    message("[W04] Existing review return verified: ",receipt)
    return(invisible(receipt))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Existing partial W04 returned folder; inspect before retrying.")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  if(!file.copy(decisions_file,export_copy,overwrite=FALSE))
    stop("Cannot archive reviewer export.")
  resolved<-file.path(dest,"latest_decisions.jsonl")
  writeLines(vapply(qids,function(id)
      jsonlite::toJSON(final[[id]],auto_unbox=TRUE,null="null"),
      character(1)),resolved,useBytes=TRUE)
  reviewer_names<-unique(vapply(final,function(e)scalar(e$reviewer),character(1)))
  info<-list(schema="local-w04-review-return-v1",status="RETURNED",
    review_type=if(include_all)"human_validation" else "human_resolution",
    records=length(qids),reviewers=as.list(reviewer_names),
    queue_sha256=sha(queue),raw_export_sha256=sha(export_copy),
    latest_decisions_sha256=sha(resolved),
    decisions=as.list(table(vapply(final,function(e)e$decision,character(1)))))
  jsonlite::write_json(info,receipt,auto_unbox=TRUE,pretty=TRUE)
  message("[W04] Review returned: ",length(qids)," decisions verified and archived at ",dest)
  invisible(receipt)
}
