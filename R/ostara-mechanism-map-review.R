#' Find one complete W09/W10 case dataset from an existing Ostara map
#'
#' Matches W10 to W09 by the archived W09 registry checksum, rather than by
#' filesystem timestamps. When multiple valid runs exist, asks for an explicit
#' selection instead of guessing or silently mixing separate search cohorts.
#' @param map Existing path returned by open_map().
#' @param extraction_dir Optional exact W09 extraction directory.
#' @param output_dir New review worksheet destination.
#' @return Paths to review worksheets.
#' @export
prepare_ostara_mechanism_review_for_map <- function(map,
  extraction_dir=NULL,output_dir=NULL) {
  map<-normalizePath(map,mustWork=TRUE)
  root<-file.path(map,"runs","W09")
  if(!dir.exists(root))stop("No W09 directory in map: ",root)
  if(is.null(extraction_dir)) {
    registries<-list.files(root,pattern="^fulltext_case_registry_normalised\\.jsonl$",
                           recursive=TRUE,full.names=TRUE)
  } else registries<-file.path(normalizePath(extraction_dir,mustWork=TRUE),
                                "fulltext_case_registry_normalised.jsonl")
  registries<-registries[file.exists(registries)]
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  matches<-list()
  for(reg in registries){
    extraction<-dirname(reg)
    base<-file.path(dirname(extraction),"W10")
    if(!dir.exists(base))next
    dirs<-list.dirs(base,recursive=FALSE,full.names=TRUE)
    for(w10 in dirs){
      m<-file.path(w10,"manifest.json")
      ledger<-file.path(w10,"w10_decisions.jsonl")
      if(!file.exists(m)||!file.exists(ledger))next
      meta<-tryCatch(jsonlite::fromJSON(m,simplifyVector=FALSE),error=function(e)NULL)
      if(is.null(meta)||!identical(meta$w09_registry_sha256,sha(reg)))next
      records<-lapply(Filter(nzchar,readLines(reg,warn=FALSE,encoding="UTF-8")),
                      jsonlite::fromJSON,simplifyVector=FALSE)
      count<-sum(vapply(records,function(x)length(x$cases),integer(1)))
      judged<-sum(nzchar(readLines(ledger,warn=FALSE,encoding="UTF-8")))
      if(count!=judged)next
      matches[[length(matches)+1L]]<-list(extraction=extraction,w10=w10,count=count)
    }
  }
  if(!length(matches))
    stop("No complete, checksum-matched W09/W10 pair found. Review existing runs.")
  if(length(matches)!=1L) {
    desc<-vapply(matches,function(x)paste(x$count,"cases:",x$extraction,
                                        "| W10:",x$w10),character(1))
    stop("Multiple matching W09/W10 runs. Supply extraction_dir explicitly or ",
         "identify one review run:\n",paste(desc,collapse="\n"))
  }
  pair<-matches[[1L]]
  w09_manifest<-file.path(pair$extraction,"manifest.json")
  w07<-NULL
  if(file.exists(w09_manifest)){
    m<-jsonlite::fromJSON(w09_manifest,simplifyVector=FALSE)
    if(!is.null(m$w07_run)&&dir.exists(m$w07_run))w07<-m$w07_run
  }
  if(is.null(output_dir))output_dir<-file.path(map,"reports","mechanism_review",
       paste0("review-",substr(sha(file.path(pair$w10,"w10_decisions.jsonl")),1,12)))
  candidates<-list.dirs(pair$w10,recursive=FALSE,full.names=TRUE)
  second<-Filter(function(p) {
    ledger<-file.path(p,"fulltext_decisions.jsonl")
    manifest<-file.path(p,"manifest.json")
    if(!all(file.exists(c(ledger,manifest))))return(FALSE)
    m<-tryCatch(jsonlite::fromJSON(manifest,simplifyVector=FALSE),
                error=function(e)NULL)
    !is.null(m)&&identical(m$first_pass_sha256,
       sha(file.path(pair$w10,"w10_decisions.jsonl")))
  },candidates)
  if(length(second)>1L)
    stop("Multiple second-pass W10 audits match the same first pass; explicit review needed")
  prepare_ostara_mechanism_review(
    extraction_dir=pair$extraction,w10_run=pair$w10,
    second_pass_dir=if(length(second))second[[1L]] else NULL,
    output_dir=output_dir,w07_run=w07)
}
