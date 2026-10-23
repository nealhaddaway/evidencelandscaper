#' Finalise the latest returned W04 screening run
#'
#' Locates the latest returned local W04 trial or full run. For a trial it
#' produces a trial-only result, never an authoritative W05 handover.
#' @param map_path Existing map folder; omitted uses open_map("Ostara_collective_action_map").
#' @param w04 Explicit W04 run directory; required for reliable named-run orchestration.
#' @param uncertain One of adjudicate or exclude. Exclusion retains the model
#'   uncertainty and records a policy-based downstream exclusion.
#' @return Path to finalised trial/full result, invisibly.
#' @export
finalize_w04 <- function(map_path=NULL,w04=NULL,uncertain=c("adjudicate","exclude")) {
  uncertain<-match.arg(uncertain)
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(!is.null(w04)){
    w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
    if(!startsWith(w04,paste0(normalizePath(file.path(map_path,"runs","W01"),winslash="/",mustWork=TRUE),"/")))
      stop("Explicit W04 run is outside this evidence map.")
  }
  roots<-list.dirs(file.path(map_path,"runs","W01"),recursive=FALSE,full.names=TRUE)
  runs<-unlist(lapply(roots,function(p) {
    d<-file.path(p,"W04_local")
    if(!dir.exists(d))return(character())
    list.dirs(d,recursive=FALSE,full.names=TRUE)
  }),use.names=FALSE)
  runs<-runs[file.exists(file.path(runs,"local_manifest.json"))]
  if(!length(runs))stop("No completed local W04 screening runs found.")
  if(!is.null(w04))runs<-runs[runs==w04]
  received<-runs[vapply(runs,function(run) {
    if(file.exists(file.path(run,"human_review_returned","receipt.json")) ||
       file.exists(file.path(run,"reconciled_human_review_returned","receipt.json")))return(TRUE)
    if(dir.exists(file.path(run,"reconciled")))return(TRUE)
    manifest<-jsonlite::fromJSON(file.path(run,"local_manifest.json"))
    identical(manifest$screening_summary_status,"PASS") ||
      (identical(uncertain,"exclude") && identical(manifest$screening_summary_status,"HUMAN_REVIEW_REQUIRED"))
  },logical(1))]
  if(!length(received))stop("No W04 run ready for finalisation: import the required human decisions first.")
  full<-received[grepl("^full-",basename(received))]
  if(length(full))received<-full
  if(length(received)>1L) {
    received<-received[order(file.info(file.path(received,"local_manifest.json"))$mtime,decreasing=TRUE)]
    warning("Multiple returned W04 runs found; using most recently returned: ",received[[1L]])
  }
  w04<-received[[1L]]
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p){
    z<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x) if(is.null(x)||length(x)!=1L||is.na(x)) "" else as.character(x)
  manifest<-jsonlite::fromJSON(file.path(w04,"local_manifest.json"),simplifyVector=FALSE)
  if(!identical(scalar(manifest$status),"screened"))stop("Unfinished W04 model run.")
  original_layer<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  layer_path<-original_layer
  recon_root<-file.path(w04,"reconciled")
  reconciled<-FALSE
  if(dir.exists(recon_root)){
    dirs<-list.dirs(recon_root,recursive=FALSE,full.names=TRUE)
    dirs<-dirs[file.exists(file.path(dirs,"manifest.json"))]
    if(length(dirs)!=1L)stop("Expected exactly one reconciled W04 layer.")
    m<-jsonlite::fromJSON(file.path(dirs[[1]],"manifest.json"),simplifyVector=FALSE)
    target<-file.path(dirs[[1]],"workflow04_reconciled_layer.jsonl")
    if(!identical(scalar(m$source_w04_sha256),sha(original_layer))||
       !identical(scalar(m$merged_sha256),sha(target)))
      stop("Reconciled W04 checksum validation failed.")
    layer_path<-target
    reconciled<-TRUE
  }
  return_dir<-file.path(w04,if(reconciled)"reconciled_human_review_returned" else "human_review_returned")
  decision_path<-file.path(return_dir,"latest_decisions.jsonl")
  has_review<-file.exists(file.path(return_dir,"receipt.json"))
  receipt<-if(has_review)jsonlite::fromJSON(file.path(return_dir,"receipt.json"),simplifyVector=FALSE) else NULL
  source<-readjl(layer_path)
  if(!identical(scalar(manifest$consensus_sha256),sha(original_layer))||
     (has_review && (!identical(scalar(receipt$status),"RETURNED")||
     !identical(scalar(receipt$latest_decisions_sha256),sha(decision_path))||
     !identical(scalar(receipt$queue_sha256),sha(file.path(w04,if(reconciled)"reconciled_human_review_queue.jsonl" else "human_review_queue.jsonl"))))))
    stop("W04 return integrity checks failed.")
  decisions<-if(has_review)readjl(decision_path) else list()
  dmap<-setNames(decisions,vapply(decisions,function(z)scalar(z$record_id),character(1)))
  ids<-vapply(source,function(z)scalar(z$record_id),character(1))
  if(anyDuplicated(ids)||anyDuplicated(names(dmap)))stop("Duplicate W04 record IDs.")
  unresolved<-ids[vapply(source,function(z)
    identical(scalar(z$screening$decision),"uncertain"),logical(1))]
  if(!has_review && length(unresolved) && identical(uncertain,"adjudicate"))
    stop("W04 human adjudication is still required; run adjudicate_w04(w04), then import_w04_review(w04).")
  if(!has_review && identical(uncertain,"exclude")) {
    for(i in which(ids %in% unresolved)) {
      source[[i]]$screening$original_decision<-"uncertain"
      source[[i]]$screening$decision<-"exclude"
      source[[i]]$screening$decision_origin<-"policy_uncertain_exclusion"
      source[[i]]$screening$requires_human_review<-FALSE
      source[[i]]$screening$policy_reason<-"Uncertain relevance; excluded from downstream evidence under run policy"
    }
    unresolved<-character()
  }
  if(!setequal(names(dmap),unresolved))
    stop("Returned human decisions do not exactly match unresolved records.")
  for(i in seq_along(source)) {
    id<-ids[[i]]
    if(!id%in%unresolved)next
    decision<-dmap[[id]]
    choice<-scalar(decision$decision)
    if(!choice%in%c("retain","exclude"))
      stop("Human decision remains unresolved: ",id)
    source[[i]]$screening$decision<-choice
    source[[i]]$screening$decision_origin<-"human_adjudication"
    source[[i]]$screening$requires_human_review<-FALSE
    source[[i]]$screening$human_adjudication<-list(
      reviewer=decision$reviewer,reason=decision$reason,
      reviewed_at_utc=decision$created_at_utc)
  }
  result_decisions<-vapply(source,function(z)scalar(z$screening$decision),character(1))
  if(any(!result_decisions%in%c("retain","exclude")))
    stop("W04 still contains unresolved records.")
  resultdir<-file.path(w04,"finalised")
  outfile<-file.path(resultdir,"workflow04_final_screening_layer.jsonl")
  summarypath<-file.path(resultdir,"summary.json")
  if(file.exists(summarypath)) {
    previous<-jsonlite::fromJSON(summarypath,simplifyVector=FALSE)
    if(!file.exists(outfile)||
       !identical(scalar(previous$final_layer_sha256),sha(outfile))||
       !identical(scalar(previous$source_layer_sha256),sha(layer_path))||
       !identical(scalar(previous$review_receipt_sha256),
                  if(has_review)sha(file.path(return_dir,"receipt.json")) else ""))
      stop("Existing finalised W04 output differs; refusing overwrite.")
    message("[W04] Previously finalised output verified: ",resultdir)
    return(invisible(resultdir))
  }
  if(dir.exists(resultdir)&&length(list.files(resultdir,all.files=TRUE,no..=TRUE)))
    stop("Partial W04 finalisation folder exists; refusing overwrite.")
  dir.create(resultdir,recursive=TRUE,showWarnings=FALSE)
  writeLines(vapply(source,function(z)jsonlite::toJSON(z,auto_unbox=TRUE,
      null="null",na="null",digits=NA),character(1)),outfile,useBytes=TRUE)
  kind<-scalar(manifest$run_type)
  jsonlite::write_json(list(schema="local-w04-finalisation-v1",status="final",
     run_type=kind,authoritative_w05_handover=identical(kind,"full"),
     uncertain_policy=uncertain,
     policy_uncertain_excluded=sum(vapply(source,function(z)
       identical(scalar(z$screening$decision_origin),"policy_uncertain_exclusion"),logical(1))),
     records=length(source),retain=sum(result_decisions=="retain"),
     exclude=sum(result_decisions=="exclude"),
     source_layer_sha256=sha(layer_path),
     original_consensus_sha256=sha(original_layer),
     reconciled_abstracts=reconciled,
     review_receipt_sha256=if(has_review)sha(file.path(return_dir,"receipt.json")) else "",
     final_layer_sha256=sha(outfile)),
     summarypath,auto_unbox=TRUE,pretty=TRUE)
  message("[W04] Finalised ",kind,": ",sum(result_decisions=="retain"),
          " retain; ",sum(result_decisions=="exclude"),
          " exclude. No W05 handover was published.")
  invisible(resultdir)
}
