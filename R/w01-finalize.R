#' Finalise W01 clusters after completed LLM and human adjudication
#'
#' Applies the unmodified upstream W01-main finalisation script only after
#' checking the LLM case set, unique human decisions, queue checksums, and
#' stable source identities. Abstract corrections remain an independent layer
#' and are not silently written into the original bibliographic records.
#' @param scored Existing full-scoring folder.
#' @param llm_dir Completed W01 LLM directory; inferred by default.
#' @return Path to final W01 clustering folder, invisibly.
#' @export
finalize_w01 <- function(scored,llm_dir=NULL) {
  if(!dir.exists(scored)) stop("W01 full scoring directory missing.")
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  review_dir<-file.path(dirname(scored),"review_preparation")
  if(is.null(llm_dir)) llm_dir<-file.path(review_dir,"llm_adjudication")
  llm<-file.path(llm_dir,"adjudications.jsonl")
  human_queue<-file.path(llm_dir,"human_review_cases.jsonl")
  full_queue<-file.path(review_dir,"w01_review_cases.jsonl")
  rescored<-file.path(review_dir,"rescored","rescored_sample.csv")
  metadata<-file.path(scored,"normalised_metadata.csv")
  summary_file<-file.path(llm_dir,"summary.json")
  all_files<-c(llm,human_queue,full_queue,rescored,metadata,summary_file)
  if(any(!file.exists(all_files)))
    stop("W01 requires completed identifier rescoring and LLM adjudication.")
  sha<-function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  summary<-jsonlite::fromJSON(summary_file,simplifyVector=FALSE)
  if(!identical(summary$status,"success")||
     !identical(summary$input_sha256,sha(full_queue))||
     !identical(summary$output_sha256,sha(llm))||
     !identical(summary$human_review_sha256,sha(human_queue)))
    stop("LLM outputs changed or are incomplete.")
  jsonlines<-function(p) {
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  all_llm<-jsonlines(llm)
  pending<-Filter(function(x) identical(x$promotion,"human_review"),all_llm)
  pending_ids<-vapply(pending,function(x) x$review_case_id,character(1))
  original_ids<-vapply(jsonlines(full_queue),
                       function(x) x$review_case_id,character(1))
  if(anyDuplicated(original_ids)||
     !identical(vapply(all_llm,function(x) x$review_case_id,
                       character(1)),original_ids)||
     anyDuplicated(pending_ids)||
     !setequal(pending_ids,vapply(jsonlines(human_queue),
                                 function(x) x$review_case_id,character(1))))
    stop("W01 LLM review IDs do not reconcile with prepared review queue.")
  state_dir<-file.path(llm_dir,"reviewer_state")
  decision_path<-file.path(state_dir,"decisions.jsonl")
  reviewer_path<-file.path(state_dir,"reviewer.json")
  if(length(pending_ids)>0L &&
     (!file.exists(decision_path)||!file.exists(reviewer_path)))
    stop("W01 human adjudication is not complete. Open the local reviewer app.")
  decisions<-if(file.exists(decision_path)) jsonlines(decision_path) else list()
  reviewer<-if(file.exists(reviewer_path))
    jsonlite::fromJSON(reviewer_path,simplifyVector=FALSE)$reviewer else NULL
  if(length(decisions)>0L &&
     (is.null(reviewer)||!nzchar(trimws(reviewer))))
    stop("Reviewer identity missing.")
  by_id<-list()
  for(e in decisions) {
    if(!identical(e$schema,"local-w01-decision-v1")||
       !identical(e$queue_sha256,sha(human_queue))||
       !identical(e$reviewer,reviewer)||
       !e$review_case_id %in% pending_ids||
       !e$decision %in% c("duplicate","not_duplicate","uncertain")||
       !is.character(e$rationale)||!nzchar(trimws(e$rationale)))
      stop("Unexpected or invalid human adjudication event.")
    by_id[[e$review_case_id]]<-e
  }
  if(!setequal(names(by_id),pending_ids))
    stop("Human adjudication remains incomplete; missing ",
         length(setdiff(pending_ids,names(by_id)))," decisions.")
  if(any(vapply(by_id,function(z) identical(z$decision,"uncertain"),logical(1))))
    stop("Human adjudication contains uncertain decisions; resolve before finalisation.")
  dest<-file.path(dirname(scored),"finalised")
  if(dir.exists(dest)||file.exists(dest))
    stop("Final W01 output already exists, refusing overwrite: ",dest)
  staging<-file.path(review_dir,"finalisation_inputs")
  if(dir.exists(staging))stop("Finalisation input archive already exists.")
  dir.create(staging)
  pairs<-utils::read.csv(rescored,stringsAsFactors=FALSE,check.names=FALSE)
  required<-c("record_i","record_j","stable_pair_key",
              "source_id_i","source_record_id_i","source_id_j",
              "source_record_id_j","rescored_classification","rescored_rule")
  if(length(setdiff(required,names(pairs))))
    stop("Rescored decisions lack immutable W01 source IDs.")
  pairs$pair_key<-pairs$stable_pair_key
  if(anyDuplicated(pairs$pair_key))stop("Duplicate stable pair keys.")
  combined<-file.path(staging,"combined_pair_decisions.csv")
  utils::write.csv(pairs,combined,row.names=FALSE,na="")
  human_out<-file.path(staging,"human_decisions.jsonl")
  writeLines(vapply(by_id,function(x)
    jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null"),
    character(1)),human_out,useBytes=TRUE)
  integrity<-file.path(staging,"human_integrity.json")
  jsonlite::write_json(list(
    schema="local-w01-human-integrity-v1",resume_allowed=TRUE,
    active_decisions=length(by_id),reviewer=reviewer,
    human_review_queue_sha256=sha(human_queue),
    human_decisions_sha256=sha(human_out),
    llm_sha256=sha(llm)),integrity,auto_unbox=TRUE,pretty=TRUE)
  script<-system.file("w01","workflow_01_apply_adjudications_and_recluster.R",
                     package="evidencelandscaper")
  if(!nzchar(script))stop("Original W01 finalisation script unavailable.")
  args<-c(script,"--combined-decisions",combined,
          "--manifestation-map",metadata,
          "--llm-adjudications",llm,"--human-decisions",human_out,
          "--human-integrity-manifest",integrity,"--output-dir",dest)
  out<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
      args=vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
  status<-attr(out,"status")
  if(!is.null(status)&&status!=0L)
    stop("W01 original finalisation refused the input:\n",
      paste(tail(out,40L),collapse="\n"),call.=FALSE)
  if(!file.exists(file.path(dest,"summary.json")))
    stop("W01 finalisation summary missing.")
  message("[W01] PASS: original final clustering validated.")
  invisible(dest)
}
