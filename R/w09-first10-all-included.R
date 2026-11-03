#' Prepare a full-text retrieval queue for every retained publication in the first 10%
#'
#' Uses the completed human-authoritative W04 screening snapshot and its
#' checksum-matched W02 bibliographic source. This selection deliberately does
#' not depend on W08 case classifications or their human adjudication status:
#' every included first-10% publication is eligible for full-text retrieval.
#'
#' The new queue has its own immutable selection manifest. Documents already
#' held in other W09 runs will be reused by the downloader only when the ID,
#' title, and DOI agree; reused files remain subject to separate content and
#' identity review. Does not change previous W09 runs.
#'
#' @param map_path Ostara map directory, or NULL for the default map.
#' @param w04_run Exact human-adjudicated first-10% W04 directory; NULL selects
#'   the unique complete selected cohort. Never selects ambiguously.
#' @return W09 run directory, invisibly.
#' @export
prepare_w09_first10_all_included <- function(map_path=NULL,w04_run=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    if(!file.exists(p))stop("Missing W04/W02 source: ",p)
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  roots<-list.dirs(file.path(map_path,"runs","W01"),
                   recursive=FALSE,full.names=TRUE)
  candidates<-unlist(lapply(roots,function(root) {
    parent<-file.path(root,"W04_local")
    if(!dir.exists(parent))return(character())
    list.dirs(parent,recursive=FALSE,full.names=TRUE)
  }),use.names=FALSE)
  candidates<-candidates[startsWith(basename(candidates),"selected-")]
  if(!is.null(w04_run)) {
    w04_run<-normalizePath(w04_run,winslash="/",mustWork=TRUE)
    candidates<-candidates[candidates==w04_run]
  }
  good<-lapply(candidates,function(p) {
    res<-tryCatch(.w04_latest_authoritative(p),error=function(e)e)
    if(inherits(res,"error"))
      stop("[W09 first10] W04 human snapshot validation failed for ",
           p,": ",conditionMessage(res))
    res
  })
  ready<-which(vapply(good,function(x)!is.null(x)&&isTRUE(x$complete),
                      logical(1)))
  if(length(ready)!=1L)
    stop("Expected exactly one complete human-adjudicated first-10% W04 cohort; ",
         "found ",length(ready),". Supply w04_run explicitly.")
  w04<-candidates[[ready]]
  auth<-good[[ready]]
  source_sha<-sha(auth$layer)
  included<-Filter(function(z)identical(z$screening$decision,"retain"),
                   readjl(auth$layer))
  ids<-sort(vapply(included,function(z)as.character(z$record_id),
                   character(1)))
  if(!length(ids)||any(!nzchar(ids))||anyNA(ids)||anyDuplicated(ids))
    stop("No uniquely identified W04 retained publications.")
  canonical<-file.path(dirname(dirname(w04)),"W02_main",
                       "finalised","canonical.jsonl")
  source<-readjl(canonical)
  source_ids<-vapply(source,function(z)
    as.character(z$identity$record_id),character(1))
  if(anyDuplicated(source_ids)||any(!ids%in%source_ids))
    stop("First-10% retained IDs cannot be mapped uniquely to W02.")
  by_id<-setNames(source,source_ids)
  scalar<-function(z)if(is.null(z)||!length(z))"" else
    paste(as.character(unlist(z,use.names=FALSE)),collapse="; ")
  queue<-do.call(rbind,lapply(ids,function(id) {
    x<-by_id[[id]]$canonical
    data.frame(record_id=id,title=scalar(x$title),doi=scalar(x$doi),
      pdf_path="",retrieval_status="not_attempted",notes="",
      stringsAsFactors=FALSE)
  }))
  # Include all W04 retains, even if W08 was incomplete, uncertain, or absent.
  # This lets full-text assessment independently determine case presence.
  dest<-file.path(map_path,"runs","W09",
    paste0("first10-all-included-",length(ids),"-",
           substr(source_sha,1L,12L)))
  manifest<-list(schema="w09-first10-all-included-v1",
    source_w04=w04,source_w04_snapshot=auth$directory,
    source_w04_layer_sha256=source_sha,
    canonical_w02_sha256=sha(canonical),
    selected_record_ids=as.list(ids),selected_count=length(ids),
    selection_rule="all human-adjudicated W04 retain publications",
    case_status_filter="none")
  mf<-file.path(dest,"manifest.json")
  if(file.exists(mf)) {
    old<-jsonlite::fromJSON(mf,simplifyVector=FALSE)
    for(field in names(manifest))
      if(!identical(old[[field]],manifest[[field]]))
        stop("Existing first10-all W09 provenance changed: ",field)
    qf<-file.path(dest,"retrieval_queue.csv")
    if(!file.exists(qf))stop("Existing first10-all W09 queue missing.")
    oldq<-utils::read.csv(qf,stringsAsFactors=FALSE)
    temp<-tempfile(fileext=".csv")
    on.exit(unlink(temp),add=TRUE)
    utils::write.csv(queue,temp,row.names=FALSE)
    expected<-utils::read.csv(temp,stringsAsFactors=FALSE)
    key<-c("record_id","title","doi")
    if(!all(key%in%names(oldq))||
       !identical(oldq[,key,drop=FALSE],expected[,key,drop=FALSE]))
      stop("Existing first10-all W09 queue identities changed.")
    message("[W09 first10] Reusing unchanged all-included queue (",
            length(ids)," publications): ",dest)
    return(invisible(dest))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Destination already contains data without a provenance manifest.")
  dir.create(file.path(dest,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(dest,"structured"),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(queue,file.path(dest,"retrieval_queue.csv"),
                   row.names=FALSE,na="")
  jsonlite::write_json(manifest,mf,auto_unbox=TRUE,pretty=TRUE)
  message("[W09 first10] Prepared all ",length(ids),
          " retained first-10% publications, independently of W08 case status.")
  message("[W09 first10] Queue: ",file.path(dest,"retrieval_queue.csv"))
  invisible(dest)
}

#' Download missing full texts for all retained first-10% publications
#'
#' Preserves previous case-positive full texts by reusing a matching local W09
#' file before attempting any network request. Searches OpenAlex-hosted XML/
#' PDF first, then entitled ScienceDirect full texts. Saves an auditable report
#' and explicitly lists missing files. Retrieved files are not automatically
#' considered publication-identity verified or full-text-case-negative.
#'
#' @param map_path Evidence-map path, or NULL for the default map.
#' @param w04_run Optional explicit first-10% human W04 cohort.
#' @param retry_failed Retry previously unsuccessful retrieval attempts.
#' @return CSV retrieval report, invisibly.
#' @export
retrieve_w09_first10_all_included <- function(map_path=NULL,
                                               w04_run=NULL,
                                               retry_failed=TRUE) {
  w09<-prepare_w09_first10_all_included(map_path=map_path,w04_run=w04_run)
  if(!nzchar(Sys.getenv("OPENALEX_API_KEY")))
    stop("[W09 first10] OPENALEX_API_KEY is needed for missing downloads. ",
         "The complete first-10% retrieval queue is saved at ",w09)
  report<-retrieve_w09_openalex_content(w09,retry_failed=retry_failed,
                                       reuse_saved_without_network=TRUE)
  tab<-utils::read.csv(report,stringsAsFactors=FALSE)
  if(anyNA(tab$record_id)||anyDuplicated(tab$record_id)||
     !setequal(tab$record_id,
       utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                       stringsAsFactors=FALSE)$record_id))
    stop("The retrieval report does not cover every first-10% retained record.")
  missing<-tab[is.na(tab$file)|!nzchar(tab$file)|
    !file.exists(ifelse(is.na(tab$file),"",tab$file)),
    c("record_id","doi","outcome","openalex_status",
      "sciencedirect_status"),drop=FALSE]
  dest<-file.path(w09,"fulltexts_still_missing.csv")
  utils::write.csv(missing,dest,row.names=FALSE,na="")
  present<-nrow(tab)-nrow(missing)
  message("[W09 first10] ",present,"/",nrow(tab),
          " retained publications have a saved file; ",
          nrow(missing)," still missing.")
  message("[W09 first10] Missing-file report: ",dest)
  invisible(report)
}
