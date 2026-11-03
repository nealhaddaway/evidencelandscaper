#' Import authoritative W04 human screening decisions
#'
#' Accepts partial reviewer JSONL exports (including append-only edit events).
#' Human retain/exclude decisions supersede LLM decisions for matching IDs.
#' Original consensus, reviewer exports, and earlier effective layers remain
#' unchanged. Repeat this operation for later 90-percent adjudication batches.
#'
#' @param w04 Exact W04 run directory; if NULL, discover by review queue SHA.
#' @param decisions_file Returned decisions.jsonl; if NULL, choose interactively.
#' @param queue_file Matching immutable review_queue JSONL. If NULL, looks for
#'   a checksum-matching queue in the W04 directory.
#' @return Immutable effective-layer directory, invisibly.
#' @export
import_w04_authoritative_decisions <- function(w04=NULL, decisions_file=NULL,
                                                queue_file=NULL) {
  if(is.null(decisions_file)) {
    message("[W04] Select the colleague's HUMAN DECISIONS (.jsonl).")
    if(!interactive())stop("Supply decisions_file outside interactive R.")
    decisions_file<-file.choose()
  }
  decisions_file<-normalizePath(decisions_file,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(is.null(w04)) {
    first<-readLines(decisions_file,n=1L,warn=FALSE,encoding="UTF-8")
    if(!length(first))stop("Human decisions file is empty.")
    event<-jsonlite::fromJSON(first,simplifyVector=FALSE)
    expected<-event$queue_sha256
    map<-open_map("Ostara_collective_action_map")
    roots<-list.dirs(file.path(map,"runs","W01"),
                     recursive=FALSE,full.names=TRUE)
    runs<-unlist(lapply(roots,function(root) {
      p<-file.path(root,"W04_local")
      if(!dir.exists(p))return(character())
      list.dirs(p,recursive=FALSE,full.names=TRUE)
    }),use.names=FALSE)
    options<-c("validation_queue.jsonl","human_review_queue.jsonl",
               "reconciled_human_review_queue.jsonl")
    found<-Filter(function(run)any(vapply(file.path(run,options),
       function(p)file.exists(p)&&identical(sha(p),expected),
       logical(1))),runs)
    if(length(found)!=1L)
      stop("Unable to locate exactly one W04 run with a matching ",
           "review queue (found ",length(found),"). Specify w04 explicitly.")
    w04<-found[[1L]]
    message("[W04] Matched human decisions to W04 run: ",w04)
  }
  w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
  readjl<-function(p){
    z<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x)if(is.character(x)&&length(x)==1L&&!is.na(x))x else ""
  base<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  manfile<-file.path(w04,"local_manifest.json")
  if(!all(file.exists(c(base,manfile))))stop("W04 consensus/manifest not found.")
  manifest<-jsonlite::fromJSON(manfile,simplifyVector=FALSE)
  if(!identical(scalar(manifest$consensus_sha256),sha(base)))
    stop("W04 consensus checksum mismatch.")
  source<-readjl(base)
  original_ids<-vapply(source,function(z)scalar(z$record_id),character(1))
  if(anyDuplicated(original_ids)||any(!nzchar(original_ids)))
    stop("Duplicate/missing W04 consensus record IDs.")
  original<-setNames(source,original_ids)
  events<-readjl(decisions_file)
  if(!length(events))stop("Human decision file is empty.")
  hashes<-unique(vapply(events,function(e)scalar(e$queue_sha256),character(1)))
  if(length(hashes)!=1L||!nzchar(hashes))stop("Inconsistent review queue SHA256.")
  if(is.null(queue_file)) {
    candidates<-file.path(w04,c("validation_queue.jsonl",
       "human_review_queue.jsonl","reconciled_human_review_queue.jsonl"))
    candidates<-candidates[file.exists(candidates)]
    candidates<-candidates[vapply(candidates,function(p)
      identical(sha(p),hashes[[1L]]),logical(1))]
    if(length(candidates)!=1L)
      stop("Cannot identify a unique checksum-matching W04 review queue. ",
           "Supply queue_file from the original reviewer bundle.")
    queue_file<-candidates[[1L]]
  }
  queue_file<-normalizePath(queue_file,winslash="/",mustWork=TRUE)
  if(!identical(sha(queue_file),hashes[[1L]]))
    stop("Human decisions do not match the supplied review queue hash.")
  queue<-readjl(queue_file)
  qids<-vapply(queue,function(z)scalar(z$review_case_id),character(1))
  if(anyDuplicated(qids)||any(!nzchar(qids)))stop("Invalid review queue IDs.")
  lookup<-setNames(queue,qids)
  final<-list()
  for(e in events) {
    review_id<-scalar(e$review_case_id)
    id<-scalar(e$record_id)
    if(!identical(scalar(e$schema),"local-w04-review-decision-v1")||
       !nzchar(review_id)||!review_id%in%qids||
       !identical(id,scalar(lookup[[review_id]]$record_id))||
       !id%in%original_ids||
       !identical(scalar(e$record_input_sha256),
                  scalar(lookup[[review_id]]$model$screening_input_sha256))||
       !identical(scalar(e$model_prompt_sha256),
                  scalar(lookup[[review_id]]$model$prompt_sha256))||
       !identical(scalar(lookup[[review_id]]$model$screening_input_sha256),
                  scalar(original[[id]]$screening$screening_input_sha256))||
       !identical(scalar(lookup[[review_id]]$model$prompt_sha256),
                  scalar(original[[id]]$screening$prompt_sha256)))
      stop("W04 reviewer event does not match original model/queue: ",review_id)
    if(!scalar(e$decision)%in%c("retain","exclude")||
       !nzchar(trimws(scalar(e$reviewer)))||
       !nzchar(scalar(e$created_at_utc)))
      stop("W04 final human decision must be retain/exclude with reviewer and timestamp: ",id)
    final[[id]]<-e  # latest event for each record is authoritative within export
  }
  if(!length(final))stop("No final human decisions.")
  root<-file.path(w04,"human_authoritative")
  imports<-file.path(root,"imports")
  snapshots<-file.path(root,"snapshots")
  dir.create(imports,recursive=TRUE,showWarnings=FALSE)
  dir.create(snapshots,recursive=TRUE,showWarnings=FALSE)
  digest<-sha(decisions_file)
  stored<-file.path(imports,paste0(digest,".jsonl"))
  meta<-file.path(imports,paste0(digest,".json"))
  if(file.exists(stored)) {
    if(!identical(sha(stored),digest)||!file.exists(meta))
      stop("Existing authoritative human import is incomplete or changed.")
    old<-jsonlite::fromJSON(meta,simplifyVector=FALSE)
    if(!identical(scalar(old$queue_sha256),hashes[[1L]])||
       !identical(scalar(old$source_consensus_sha256),sha(base)))
      stop("Previously imported decisions conflict with W04 provenance.")
  } else {
    if(file.exists(meta))stop("Partial authoritative import detected.")
    # Archive exact source first; no original model file is modified.
    if(!file.copy(decisions_file,stored,overwrite=FALSE))
      stop("Cannot archive decisions.")
    jsonlite::write_json(list(schema="w04-authoritative-import-v1",
      source_consensus_sha256=sha(base),queue_sha256=hashes[[1L]],
      raw_decisions_sha256=digest,reviewers=as.list(unique(vapply(final,
      function(e)scalar(e$reviewer),character(1)))),
      unique_reviewed_records=length(final)),meta,pretty=TRUE,auto_unbox=TRUE)
  }
  all_files<-sort(list.files(imports,pattern="^[a-f0-9]{64}[.]jsonl$",
                             full.names=TRUE))
  combined<-list()
  for(p in all_files) {
    m<-paste0(tools::file_path_sans_ext(p),".json")
    if(!file.exists(m)||!identical(sha(p),tools::file_path_sans_ext(basename(p))))
      stop("Human import archive integrity failure: ",p)
    info<-jsonlite::fromJSON(m,simplifyVector=FALSE)
    if(!identical(scalar(info$source_consensus_sha256),sha(base)))
      stop("Human import belongs to another consensus.")
    ee<-readjl(p)
    resolved<-list()
    for(e in ee)resolved[[scalar(e$record_id)]]<-e
    for(id in names(resolved)) {
      e<-resolved[[id]]
      if(!id%in%original_ids||!scalar(e$decision)%in%c("retain","exclude"))
        stop("Invalid archived human decision: ",id)
      previous<-combined[[id]]
      if(!is.null(previous)&&
         (!identical(scalar(previous$decision),scalar(e$decision))||
          !identical(scalar(previous$reviewer),scalar(e$reviewer))))
        stop("Conflicting human decisions across imports for ",id,
             ". No effective layer was updated.")
      e$source_import_sha256<-tools::file_path_sans_ext(basename(p))
      combined[[id]]<-e
    }
  }
  effective<-source
  human_ids<-names(combined)
  disagreements<-0L
  for(i in seq_along(effective)) {
    id<-original_ids[[i]]
    e<-combined[[id]]
    if(is.null(e))next
    prior<-scalar(effective[[i]]$screening$decision)
    if(!identical(prior,scalar(e$decision)))disagreements<-disagreements+1L
    effective[[i]]$screening$original_decision<-prior
    effective[[i]]$screening$decision<-scalar(e$decision)
    effective[[i]]$screening$decision_origin<-"human_authoritative"
    effective[[i]]$screening$requires_human_review<-FALSE
    effective[[i]]$screening$human_adjudication<-list(
      reviewer=e$reviewer,reason=e$reason,
      reviewed_at_utc=e$created_at_utc,
      source_import_sha256=e$source_import_sha256)
  }
  # Store a stable digest of the set of immutable import hashes, independent
  # of file creation timestamps; every additive import makes a new snapshot.
  imports_hash<-digest::digest(paste(basename(all_files),collapse="\n"),
                               algo="sha256",serialize=FALSE)
  target<-file.path(snapshots,imports_hash)
  output<-file.path(target,"workflow04_effective_screening_layer.jsonl")
  summary_path<-file.path(target,"summary.json")
  decisions<-vapply(effective,function(z)scalar(z$screening$decision),character(1))
  complete<-all(decisions%in%c("retain","exclude"))
  retained<-sum(decisions=="retain")
  excluded<-sum(decisions=="exclude")
  if(dir.exists(target)) {
    if(!all(file.exists(c(output,summary_path))))
      stop("Existing W04 human snapshot is incomplete.")
    old<-jsonlite::fromJSON(summary_path,simplifyVector=FALSE)
    if(!identical(scalar(old$layer_sha256),sha(output))||
       !identical(scalar(old$source_consensus_sha256),sha(base)))
      stop("W04 authoritative snapshot integrity mismatch.")
    message("[W04] Existing authoritative snapshot verified: ",target)
    return(invisible(target))
  }
  dir.create(target,recursive=TRUE,showWarnings=FALSE)
  writeLines(vapply(effective,function(z)jsonlite::toJSON(z,
      auto_unbox=TRUE,null="null",na="null",digits=NA),character(1)),
      output,useBytes=TRUE)
  overrides<-if(length(human_ids))do.call(rbind,lapply(human_ids,function(id) {
    e<-combined[[id]]
    prior<-scalar(original[[id]]$screening$decision)
    data.frame(record_id=id,model_decision=prior,
      final_decision=scalar(e$decision),
      disagreement=!identical(prior,scalar(e$decision)),
      decision_origin="human_authoritative",
      reviewer=scalar(e$reviewer),reason=scalar(e$reason),
      reviewed_at_utc=scalar(e$created_at_utc),
      source_import_sha256=scalar(e$source_import_sha256),
      stringsAsFactors=FALSE)
  })) else data.frame()
  utils::write.csv(overrides,file.path(target,"human_override_audit.csv"),
                   row.names=FALSE,na="")
  jsonlite::write_json(list(schema="w04-authoritative-snapshot-v1",
    source_consensus_sha256=sha(base),
    import_sha256=as.list(basename(all_files)),
    records=length(effective),
    reviewed_records=length(human_ids),
    disagreements=disagreements,
    retain=retained,exclude=excluded,uncertain=sum(decisions=="uncertain"),
    status=if(complete)"final" else "partial",
    run_type=scalar(manifest$run_type),
    layer_sha256=sha(output)),summary_path,pretty=TRUE,auto_unbox=TRUE)
  message("[W04] Human authoritative snapshot: ",length(human_ids),
          " reviewed; ",disagreements," differ from model; ",
          retained," retained; ",excluded," excluded; ",
          sum(decisions=="uncertain")," unresolved.")
  message("[W04] Effective layer: ",output)
  if(!complete)message("[W04] Remaining uncertainties require adjudication ",
                       "before W07 can consume this as a final layer.")
  invisible(target)
}

# Internal: choose only a checksum-validated, complete human screening snapshot.
.w04_latest_authoritative <- function(w04) {
  root<-file.path(w04,"human_authoritative","snapshots")
  if(!dir.exists(root))return(NULL)
  dirs<-list.dirs(root,recursive=FALSE,full.names=TRUE)
  dirs<-dirs[file.exists(file.path(dirs,"summary.json"))]
  if(!length(dirs))return(NULL)
  info<-lapply(dirs,function(d)jsonlite::fromJSON(
    file.path(d,"summary.json"),simplifyVector=FALSE))
  count<-vapply(info,function(z)length(z$import_sha256),integer(1))
  top<-which(count==max(count))
  if(length(top)!=1L)stop("Ambiguous authoritative W04 snapshots.")
  d<-dirs[[top]]
  m<-info[[top]]
  src<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  out<-file.path(d,"workflow04_effective_screening_layer.jsonl")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(!file.exists(out)||
     !identical(m$source_consensus_sha256,sha(src))||
     !identical(m$layer_sha256,sha(out)))
    stop("Authoritative W04 snapshot fails checksum verification.")
  list(directory=d,layer=out,summary=m,
       complete=identical(m$status,"final"))
}
