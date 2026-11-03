#' Prepare full-text retrieval for case-negative abstracts in the first 10% sample
#'
#' Selects ONLY validated W08 publications with no_identifiable_case, from
#' the first human-adjudicated W04 selected cohort. An abstract-negative
#' assessment is a retrieval-priority classification, not confirmation that
#' the complete full text contains no qualifying cases.
#' All existing W04, W08 and W02 evidence is read-only. Missing and unclear
#' W08 records are excluded and recorded explicitly.
#'
#' @param w08 Completed/partial W08 first-10% run, or NULL to discover the
#'   sole eligible run. Never guesses between multiple candidates.
#' @param map_path Ostara evidence-map directory, or NULL to open the default.
#' @return Prepared W09 retrieval directory, invisibly.
#' @export
prepare_w09_no_case_fulltexts_first10 <- function(w08=NULL,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  w08_root<-file.path(map_path,"runs","W08")
  readjl<-function(f) {
    v<-readLines(f,warn=FALSE,encoding="UTF-8")
    lapply(v[nzchar(trimws(v))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  is_first10<-function(p) {
    mf<-file.path(p,"manifest.json")
    if(!file.exists(mf))return(FALSE)
    m<-tryCatch(jsonlite::fromJSON(mf,simplifyVector=FALSE),
                error=function(e)NULL)
    !is.null(m)&&identical(m$source_w04_layer,"human_authoritative")&&
      !is.null(m$source_w04)&&startsWith(basename(m$source_w04),"selected-")&&
      identical(basename(dirname(m$source_w04)),"W04_local")
  }
  if(is.null(w08)) {
    dirs<-if(dir.exists(w08_root))
      list.dirs(w08_root,recursive=FALSE,full.names=TRUE) else character()
    eligible<-dirs[vapply(dirs,is_first10,logical(1))]
    if(length(eligible)!=1L)
      stop("Expected exactly one first-10% human-authoritative W08 run; ",
           "found ",length(eligible),". Supply the exact w08 directory.")
    w08<-eligible[[1L]]
  }
  w08<-normalizePath(w08,winslash="/",mustWork=TRUE)
  if(!identical(dirname(w08),normalizePath(w08_root,winslash="/",mustWork=TRUE))||
     !is_first10(w08))
    stop("W08 must be a first-10% run based on the human-authoritative ",
         "selected W04 cohort in this map.")
  paths<-file.path(w08,c("manifest.json","summary.json",
                         "case_assessments.jsonl","prompt.txt","fields.csv"))
  if(!all(file.exists(paths)))stop("W08 manifest, summary, assessments, prompt or fields missing.")
  m<-jsonlite::fromJSON(paths[[1L]],simplifyVector=FALSE)
  sm<-jsonlite::fromJSON(paths[[2L]],simplifyVector=FALSE)
  if(!sm$status%in%c("PILOT_COMPLETE","PILOT_PARTIAL")||
     !identical(sm$responses_sha256,sha(paths[[3L]]))||
     !identical(m$prompt_sha256,sha(paths[[4L]]))||
     !identical(m$schema_sha256,sha(paths[[5L]])))
    stop("W08 validated assessments, status or prompt checksum mismatch.")
  assessments<-readjl(paths[[3L]])
  observed<-vapply(assessments,function(x)as.character(x$record_id),character(1))
  selected<-as.character(unlist(m$selected_record_ids,use.names=FALSE))
  if(!length(selected)||anyDuplicated(selected)||anyDuplicated(observed)||
     any(!observed%in%selected)||length(selected)!=as.integer(m$sample_size))
    stop("W08 selected IDs and validated assessments differ.")
  unresolved<-setdiff(selected,observed)
  if(identical(sm$status,"PILOT_COMPLETE")&&length(unresolved))
    stop("Complete W08 run has missing assessments.")
  if(identical(sm$status,"PILOT_PARTIAL")&&
     (!setequal(unresolved,as.character(unlist(
        sm$unresolved_record_ids,use.names=FALSE)))||
      !identical(length(observed),as.integer(sm$validated_records))))
    stop("Partial W08 summary does not match validated ledger.")
  statuses<-vapply(assessments,function(x)
    as.character(x$case_evidence_status),character(1))
  if(any(!statuses%in%c("specific_case","multiple_cases",
                         "no_identifiable_case","unclear")))
    stop("Unexpected W08 case classification.")
  for(x in assessments)
    .w08_validate_case_count(x$case_evidence_status,x$cases,x$record_id)
  w04<-normalizePath(m$source_w04,winslash="/",mustWork=TRUE)
  authoritative<-.w04_latest_authoritative(w04)
  if(is.null(authoritative)||!isTRUE(authoritative$complete)||
     !identical(sha(authoritative$layer),m$source_w04_sha256))
    stop("Original W04 human-authoritative layer is missing or changed.")
  w04rows<-readjl(authoritative$layer)
  retained<-vapply(Filter(function(x)
    identical(x$screening$decision,"retain"),w04rows),
    function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(retained)||!setequal(selected,retained))
    stop("W08 selection differs from human-adjudicated first-10% retains.")
  canonical<-file.path(dirname(dirname(w04)),"W02_main",
                       "finalised","canonical.jsonl")
  if(!file.exists(canonical)||
     !identical(sha(canonical),m$canonical_w02_sha256))
    stop("W02 canonical bibliographic source checksum changed.")
  source<-readjl(canonical)
  cids<-vapply(source,function(x)
    as.character(x$identity$record_id),character(1))
  if(anyDuplicated(cids)||any(!selected%in%cids))
    stop("Canonical W02 records do not cover the selected W08 cohort.")
  byid<-setNames(source,cids)
  # Human-adjudicated W08 status overrides the abstract model only when
  # backed by a checksum-verified immutable colleague-review snapshot.
  human_root<-file.path(w08,"human_adjudication")
  human_dirs<-if(dir.exists(human_root))
    list.dirs(human_root,recursive=FALSE,full.names=TRUE) else character()
  human_dirs<-human_dirs[file.exists(file.path(human_dirs,"manifest.json"))]
  if(length(human_dirs)>1L)
    stop("Multiple W08 human adjudication snapshots exist. Select and ",
         "reconcile an authoritative version before preparing retrieval.")
  human_path<-if(length(human_dirs))human_dirs[[1L]] else NULL
  final_statuses<-statuses
  origin<-rep("W08_model",length(observed))
  human_checksum<-NULL
  if(!is.null(human_path)) {
    hm<-jsonlite::fromJSON(file.path(human_path,"manifest.json"),
                           simplifyVector=FALSE)
    hp<-file.path(human_path,"effective_case_classifications.csv")
    if(!file.exists(hp)||
       !identical(hm$source_w08_sha256,sha(paths[[3L]]))||
       !identical(hm$effective_case_classifications_sha256,sha(hp)))
      stop("W08 human adjudication provenance/checksum mismatch.")
    human<-utils::read.csv(hp,stringsAsFactors=FALSE)
    if(anyDuplicated(human$record_id)||!setequal(human$record_id,observed)||
       !all(c("model_case_status","final_case_status","decision_origin")%in%names(human)))
      stop("W08 human classification table does not match model cohort.")
    human<-human[match(observed,human$record_id),,drop=FALSE]
    if(!identical(as.character(human$model_case_status),as.character(statuses))||
       any(!human$final_case_status%in%c("specific_case","multiple_cases",
         "no_identifiable_case","unclear")))
      stop("W08 human effective classifications contain invalid statuses.")
    final_statuses<-as.character(human$final_case_status)
    origin<-as.character(human$decision_origin)
    human_checksum<-sha(hp)
    message("[W09 no-case] Using W08 human adjudications: ",human_path)
  } else message("[W09 no-case] No imported W08 human decisions; ",
       "using validated model classifications only.")
  ids<-sort(observed[final_statuses=="no_identifiable_case"])
  if(!length(ids))stop("No validated case-negative W08 abstracts in this cohort.")
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  queue<-do.call(rbind,lapply(ids,function(id) {
    z<-byid[[id]]$canonical
    data.frame(record_id=id,title=scalar(z$title),doi=scalar(z$doi),
      pdf_path="",retrieval_status="not_attempted",notes="",
      stringsAsFactors=FALSE)
  }))
  # Keep frozen selection and source checksums in a separate, non-case W09 run.
  dest<-file.path(map_path,"runs","W09",
    paste0("first10-abstract-no-case-",length(ids),"-",
           substr(sha(paths[[3L]]),1L,12L),
           if(!is.null(human_checksum))paste0("-human-",
              substr(human_checksum,1L,12L)) else ""))
  manifest<-list(schema="w09-first10-abstract-no-case-v1",
    source_w08=w08,w08_assessments_sha256=sha(paths[[3L]]),
    human_adjudication_path=human_path,
    human_effective_classifications_sha256=human_checksum,
    source_w04=w04,source_w04_layer_sha256=sha(authoritative$layer),
    canonical_w02_sha256=sha(canonical),
    selected_record_ids=as.list(ids),unresolved_w08_ids=as.list(unresolved),
    excluded_unclear_ids=as.list(sort(observed[final_statuses=="unclear"])),
    source_w08_partial=length(unresolved)>0L)
  mf<-file.path(dest,"manifest.json")
  if(file.exists(mf)) {
    old<-jsonlite::fromJSON(mf,simplifyVector=FALSE)
    for(field in names(manifest))
      if(!identical(old[[field]],manifest[[field]]))
        stop("Existing first-10% no-case W09 run has different provenance: ",field)
    qf<-file.path(dest,"retrieval_queue.csv")
    if(!file.exists(qf))stop("Existing first-10% W09 queue is missing.")
    present<-utils::read.csv(qf,stringsAsFactors=FALSE)
    tmp<-tempfile(fileext=".csv")
    on.exit(unlink(tmp),add=TRUE)
    utils::write.csv(queue,tmp,row.names=FALSE)
    expected<-utils::read.csv(tmp,stringsAsFactors=FALSE)
    identity_cols<-c("record_id","title","doi")
    if(!all(identity_cols%in%names(present))||
       !identical(present[,identity_cols,drop=FALSE],
                  expected[,identity_cols,drop=FALSE]))
      stop("Existing first-10% W09 queue bibliographic identity changed.")
    message("[W09 no-case] Verified existing queue: ",dest)
    return(invisible(dest))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Existing W09 directory lacks expected manifest; refusing overwrite.")
  dir.create(file.path(dest,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(dest,"structured"),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(queue,file.path(dest,"retrieval_queue.csv"),row.names=FALSE)
  idx<-match(ids,observed)
  utils::write.csv(data.frame(record_id=ids,
      model_w08_status=statuses[idx],
      final_w08_status=final_statuses[idx],
      decision_origin=origin[idx]),
    file.path(dest,"w08_abstract_no_case_selection.csv"),row.names=FALSE)
  excluded<-rbind(
    data.frame(record_id=unresolved,
               status=rep("unvalidated",length(unresolved))),
    data.frame(record_id=observed[final_statuses=="unclear"],
               status=rep("unclear",sum(final_statuses=="unclear"))))
  excluded<-excluded[order(excluded$record_id),,drop=FALSE]
  utils::write.csv(excluded,file.path(dest,"not_selected_unresolved.csv"),
                   row.names=FALSE)
  jsonlite::write_json(manifest,mf,auto_unbox=TRUE,pretty=TRUE)
  message("[W09 no-case] Prepared ",length(ids),
    " case-negative ABSTRACT publications of ",length(selected),
    " first-10% retained publications; ",sum(final_statuses=="unclear"),
    " unclear and ",length(unresolved)," unvalidated excluded.")
  invisible(dest)
}

#' Retrieve full texts for first-10% abstracts with no identified case
#'
#' Prepares a provenance-verified W09 no-case retrieval queue and runs the
#' existing OpenAlex-hosted content/ScienceDirect retrieval backend. Existing
#' documents are preserved; earlier W09 files can be reused only when record
#' ID, title and DOI match. Downloaded documents remain identity-unverified
#' until the separate source-identity audit.
#' @param w08 W08 first-10% case run, or NULL for unique auto-discovery.
#' @param map_path Existing Ostara map directory, or NULL for default.
#' @param retry_failed Reattempt previously unsuccessful retrievals.
#' @return W09 full-text retrieval report path, invisibly.
#' @export
retrieve_w09_no_case_fulltexts_first10 <- function(
    w08=NULL,map_path=NULL,retry_failed=TRUE) {
  w09<-prepare_w09_no_case_fulltexts_first10(w08=w08,map_path=map_path)
  if(!nzchar(Sys.getenv("OPENALEX_API_KEY")))
    stop("[W09 no-case] Prepared the queue, but OPENALEX_API_KEY is not set. ",
         "The queue is saved at ",w09,
         ". The retrieval backend requires this key.")
  report<-retrieve_w09_openalex_content(w09,retry_failed=retry_failed)
  message("[W09 no-case] Retrieval report: ",report,
          ". Full-text case presence is not yet validated.")
  invisible(report)
}
