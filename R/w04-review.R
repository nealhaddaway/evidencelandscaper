#' Prepare local human adjudication queue for W04 consensus
#' @param w04 W04 run directory returned by run_w04().
#' @param include_all Include consensus records as optional human validation.
#' @return Path to queue JSONL, or NULL if there are no review cases.
#' @export
prepare_w04_review <- function(w04,include_all=FALSE,missing_abstracts=FALSE) {
  w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
  layer<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  votes<-lapply(1:3,function(i)file.path(w04,"consensus",
                    c("pass1.jsonl","pass2.jsonl","pass3_conflicts.jsonl")[[i]]))
  canonical<-file.path(dirname(dirname(w04)),"W02_main","finalised","canonical.jsonl")
  if(!all(file.exists(c(layer,canonical))))
    stop("W04 consensus and W02 canonical files required.")
  readjl<-function(path){
    if(!file.exists(path))return(list())
    x<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  model<-readjl(layer);source<-readjl(canonical)
  rid<-function(x)as.character(x$identity$record_id)
  src<-setNames(source,vapply(source,rid,character(1)))
  included<-Filter(function(x)include_all ||
    isTRUE(x$screening$requires_human_review) ||
    identical(x$screening$decision,"uncertain"),model)
  if(missing_abstracts)included<-Filter(function(x) {
    id<-as.character(x$record_id)
    !nzchar(trimws(paste(unlist(src[[id]]$canonical$abstract),collapse=" ")))
  },model)
  destination<-file.path(w04,if(missing_abstracts)"missing_abstract_review_queue.jsonl" else if(include_all)"validation_queue.jsonl" else "human_review_queue.jsonl")
  metadata<-function(x) {
    c<-x$canonical
    scalar<-function(y)if(is.null(y)||!length(y))"" else
      paste(as.character(unlist(y,use.names=FALSE)),collapse="; ")
    list(title=scalar(c$title),authors=scalar(c$authors),
         year=scalar(c$year),journal=scalar(c$source_title),
         doi=scalar(c$doi),abstract=scalar(c$abstract),
         keywords=scalar(c$keywords))
  }
  all_votes<-unlist(lapply(votes,readjl),recursive=FALSE)
  rationale<-split(all_votes,vapply(all_votes,function(x)
    as.character(x$record_id),character(1)))
  cases<-lapply(included,function(x){
    id<-as.character(x$record_id)
    if(is.null(src[[id]]))stop("W04 review record missing in W02: ",id)
    list(schema="local-w04-review-case-v1",
         review_case_id=paste0("w04-",id),record_id=id,
         bibliographic=metadata(src[[id]]),model=x$screening,
         model_reasons=lapply(rationale[[id]],function(v)
           list(pass=v$pass,decision=v$decision,reason=v$reason)))
  })
  if(file.exists(destination)){
    existing<-readjl(destination)
    if(!identical(existing,cases))
      stop("W04 review queue already exists with different content; refusing overwrite.")
  } else {
    writeLines(vapply(cases,function(x)jsonlite::toJSON(x,
      auto_unbox=TRUE,null="null"),character(1)),destination,useBytes=TRUE)
  }
  message("[W04] Prepared ",length(cases)," human review cases at ",destination)
  if(!length(cases))return(invisible(NULL))
  invisible(destination)
}

#' Launch local W04 Shiny adjudication from an existing model trial
#' @param w04 W04 run directory.
#' @param include_all Display all screened records for validation instead of
#'   only unresolved consensus records.
#' @export
adjudicate_w04 <- function(w04,include_all=FALSE,missing_abstracts=FALSE) {
  queue<-prepare_w04_review(w04,include_all=include_all,missing_abstracts=missing_abstracts)
  if(is.null(queue))return(invisible(NULL))
  state<-file.path(w04,if(missing_abstracts)"missing_abstract_reviewer_state" else if(include_all)"validation_reviewer_state" else "reviewer_state")
  dir.create(state,recursive=TRUE,showWarnings=FALSE)
  script<-system.file("w04","review_app.R",package="evidencelandscaper")
  if(!nzchar(script))stop("W04 Shiny reviewer app is not packaged.")
  status<-system2(file.path(R.home("bin"),"Rscript"),
    args=vapply(c(script,queue,state),shQuote,character(1)),wait=TRUE)
  if(as.integer(status)!=0L)stop("W04 review app exited unsuccessfully.")
  invisible(state)
}
