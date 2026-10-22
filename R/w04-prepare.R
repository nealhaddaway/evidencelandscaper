#' Validate local W04 inputs before incurring model costs
#'
#' @param scored Existing W01 full scoring folder.
#' @param prompt_file UTF-8 plain text screening prompt.
#' @return Input counts and prompt hash.
#' @export
prepare_w04 <- function(scored,prompt_file) {
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  canonical<-file.path(dirname(scored),"W02_main","finalised","canonical.jsonl")
  statuses<-file.path(dirname(scored),"W03_publication_status","full","publication_status.jsonl")
  statusmeta<-file.path(dirname(scored),"W03_publication_status","full","local_manifest.json")
  if(!all(file.exists(c(canonical,statuses,statusmeta))))
    stop("Finalised W02 canonical and complete W03 status inputs are required.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  sm<-jsonlite::fromJSON(statusmeta)
  if(!identical(sm$status,"final")||!isTRUE(sm$full_corpus)||
     !identical(sm$w02_canonical_sha256,sha(canonical))||
     !identical(sm$status_sha256,sha(statuses)))
    stop("W03 handover checksum validation failed.")
  parse<-function(file) {
    lines<-readLines(file,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  crows<-parse(canonical);srows<-parse(statuses)
  cid<-vapply(crows,function(x)x$identity$record_id,character(1))
  sid<-vapply(srows,function(x)x$record_id,character(1))
  if(length(cid)!=length(sid)||anyDuplicated(cid)||anyDuplicated(sid)||
     !setequal(cid,sid))
    stop("W02 canonical and W03 status IDs do not reconcile.")
  `%||%`<-function(x,y)if(is.null(x))y else x
  excluded<-vapply(srows,function(x)
    isTRUE((x$publication_status %||% list())$exclude_from_workflow04 %||%
           x$exclude_from_workflow04 %||% FALSE),logical(1))
  plines<-readLines(prompt_file,warn=FALSE,encoding="UTF-8")
  if(!length(plines)||!nzchar(trimws(paste(plines,collapse=""))))
    stop("Screening prompt is empty.")
  result<-list(publications=length(cid),w03_excluded=sum(excluded),
               w04_eligible=length(cid)-sum(excluded),
               prompt_sha256=digest::digest(paste(plines,collapse="\n"),
                                          algo="sha256",serialize=FALSE),
               w02_sha256=sha(canonical),w03_sha256=sha(statuses))
  print(result)
  invisible(result)
}
