#' Quarantine manifestations involved in unresolved W01 duplicate pairs
#'
#' Existing W01 finalisation requires a yes/no value for each LLM-review pair.
#' This explicit policy uses not_duplicate only as a *temporary clustering
#' convention*, and excludes BOTH associated publication clusters from the
#' eligible screening population after finalisation. This is NOT a scientific
#' finding that the pairs are distinct and NOT human adjudication.
#' All original pair evidence remains archived.
#' @param scored Completed W01 scoring directory with LLM review queue.
#' @return Path to the source-identity quarantine CSV.
#' @export
quarantine_w01_uncertain <- function(scored) {
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  root<-dirname(scored)
  llm<-file.path(root,"review_preparation","llm_adjudication")
  queue<-file.path(llm,"human_review_cases.jsonl")
  if(!file.exists(queue))stop("Completed W01 LLM human review queue required.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  lines<-readLines(queue,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  records<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  ids<-vapply(records,function(z)as.character(z$review_case_id),character(1))
  if(anyDuplicated(ids))stop("Duplicate uncertain pair review IDs.")
  policy_file<-file.path(llm,"w01_uncertain_quarantined_sources.csv")
  reviewer_dir<-file.path(llm,"reviewer_state")
  reviewer_json<-file.path(reviewer_dir,"reviewer.json")
  decisions_jsonl<-file.path(reviewer_dir,"decisions.jsonl")
  policy_manifest<-file.path(llm,"quarantine_policy_manifest.json")
  if(any(file.exists(c(policy_file,reviewer_json,decisions_jsonl,policy_manifest))))
    stop("W01 review policy state already exists. Do not overwrite decisions.")
  if(dir.exists(reviewer_dir) &&
     length(list.files(reviewer_dir,all.files=TRUE,no..=TRUE)))
    stop("Existing human reviewer state present; refusing automated replacement.")
  if(!length(records))return(invisible(NULL))
  reviewer<-"AUTOMATED_QUARANTINE_POLICY_NOT_HUMAN"
  items<-list()
  take_side<-function(z,side) {
    rec<-z[[side]]
    src<-as.character(rec$source)
    rid<-as.character(rec$source_record_id)
    if(length(src)!=1L||length(rid)!=1L||is.na(src)||is.na(rid)||
       !nzchar(src)||!nzchar(rid))
      stop("Uncertain W01 case missing stable source identity; cannot quarantine.")
    data.frame(review_case_id=as.character(z$review_case_id),
               source=src,source_record_id=rid,stringsAsFactors=FALSE)
  }
  for(z in records) {
    items[[length(items)+1L]]<-take_side(z,"record_i")
    items[[length(items)+1L]]<-take_side(z,"record_j")
  }
  quarantine<-unique(do.call(rbind,items))
  dir.create(reviewer_dir,recursive=TRUE,showWarnings=FALSE)
  jsonlite::write_json(list(reviewer=reviewer,
    policy="Automated quarantine of both publication clusters; no human review"),
    reviewer_json,auto_unbox=TRUE,pretty=TRUE)
  events<-lapply(records,function(z)list(
    schema="local-w01-decision-v1",
    review_case_id=z$review_case_id,reviewer=reviewer,
    decision="not_duplicate",
    rationale=paste("AUTOMATED CLUSTERING CONVENTION ONLY.",
      "Both source publications are quarantined from eligibility.",
      "This is not evidence that the records are distinct."),
    queue_sha256=sha(queue),
    resolved_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")))
  writeLines(vapply(events,function(z)jsonlite::toJSON(z,
    auto_unbox=TRUE,null="null"),character(1)),decisions_jsonl,useBytes=TRUE)
  utils::write.csv(quarantine,policy_file,row.names=FALSE,na="")
  jsonlite::write_json(list(
    schema="evidencelandscaper-w01-uncertain-quarantine-v1",
    status="AUTOMATED_POLICY_NOT_HUMAN_REVIEW",
    rule="Treat uncertain pair as separate only for clustering; quarantine both resulting clusters before screening",
    uncertain_pairs=length(ids),source_occurrences=nrow(quarantine),
    queue_sha256=sha(queue),decisions_sha256=sha(decisions_jsonl),
    source_quarantine_sha256=sha(policy_file)),policy_manifest,
    auto_unbox=TRUE,pretty=TRUE)
  message("[W01] Policy quarantine: ",length(ids),
          " unresolved pairs; ",nrow(quarantine),
          " associated source manifestations marked for exclusion from screening.")
  invisible(policy_file)
}
