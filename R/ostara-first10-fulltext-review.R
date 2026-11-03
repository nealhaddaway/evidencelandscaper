#' Export independent full-text mechanism and insights findings for human review
#'
#' Creates reproducible publication-level review CSVs from the first-10%
#' full-text Luna runs, with no case-ID or W07 abstract-coding prerequisite.
#' Every model finding remains unapproved until a human review is recorded.
#' The original append-only extraction ledgers are never overwritten.
#'
#' @param mechanism_dir Versioned directory from the all-full-text extractor.
#' @param insights_dir Optional versioned abstract-no-case insights directory.
#' @return Named review CSV paths and review directory, invisibly.
#' @export
prepare_ostara_first10_fulltext_review <- function(mechanism_dir,
                                                     insights_dir=NULL) {
  mechanism_dir<-normalizePath(mechanism_dir,winslash="/",mustWork=TRUE)
  if(!is.null(insights_dir))
    insights_dir<-normalizePath(insights_dir,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    if(!file.exists(p))return(list())
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sc<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  mfile<-file.path(mechanism_dir,"manifest.json")
  ledger<-file.path(mechanism_dir,"mechanism_assessments.jsonl")
  inventory<-file.path(mechanism_dir,"source_inventory.csv")
  if(!all(file.exists(c(mfile,inventory))))
    stop("Full-text mechanism manifest and inventory are required")
  meta<-jsonlite::fromJSON(mfile,simplifyVector=FALSE)
  if(!identical(meta$schema,"ostara-fulltext-mechanism-all-v4"))
    stop("Expected independent full-text mechanism schema v4")
  m<-readjl(ledger)
  mids<-vapply(m,function(x)sc(x$record_id),character(1))
  if(anyDuplicated(mids))stop("Duplicate full-text mechanism publications")
  inames<-utils::read.csv(inventory,stringsAsFactors=FALSE)
  if(anyDuplicated(inames$record_id)||any(!mids%in%inames$record_id))
    stop("Full-text mechanism results differ from source inventory")
  insights<-list();ifile<-NULL;imeta<-NULL
  if(!is.null(insights_dir)) {
    ip<-file.path(insights_dir,"manifest.json")
    ifile<-file.path(insights_dir,"insights_assessments.jsonl")
    if(!file.exists(ip))stop("Full-text insights manifest missing")
    imeta<-jsonlite::fromJSON(ip,simplifyVector=FALSE)
    if(!identical(imeta$schema,"ostara-first10-ft-insights-v1")||
       !identical(imeta$source_mechanism_run,mechanism_dir))
      stop("Insights run does not match the mechanism inventory")
    insights<-readjl(ifile)
  }
  ids<-vapply(insights,function(x)sc(x$record_id),character(1))
  if(anyDuplicated(ids)||any(!ids%in%inames$record_id))
    stop("Insights records must belong to the same full-text cohort")
  mrows<-list();nrows<-list();crows<-list();irows<-list()
  for(z in m) {
    id<-sc(z$record_id)
    for(v in z$mechanism_inventory)
      mrows[[length(mrows)+1L]]<-data.frame(
        record_id=id,mechanism_id=sc(v$mechanism_id),
        status=sc(v$status),evidence_quote=sc(v$source_quote),
        source_anchor=sc(v$source_anchor),model_reason=sc(v$reason),
        human_decision="",reviewer="",review_complete=FALSE,
        stringsAsFactors=FALSE)
    for(v in z$new_novelty)
      nrows[[length(nrows)+1L]]<-data.frame(
        record_id=id,proposed_label=sc(v$proposed_label),
        working_definition=sc(v$working_definition),
        support_status=sc(v$support_status),
        closest_approved_mechanism_id=sc(v$closest_approved_mechanism_id),
        reason=sc(v$reason),evidence_quote=sc(v$source_quote),
        source_anchor=sc(v$source_anchor),
        human_decision="",reviewer="",review_complete=FALSE,
        stringsAsFactors=FALSE)
    c<-z$case_presence_assessment
    crows[[length(crows)+1L]]<-data.frame(
      record_id=id,fulltext_case_status=sc(c$case_status),
      candidate_case_count=if(is.null(c$candidate_case_count))NA_integer_
                           else as.integer(c$candidate_case_count),
      candidate_labels=paste(vapply(c$candidate_cases,function(x)
        sc(x$candidate_label),character(1)),collapse="; "),
      assessment_reason=sc(c$assessment_reason),
      human_decision="",reviewer="",review_complete=FALSE,
      stringsAsFactors=FALSE)
  }
  for(z in insights)for(v in z$insights)
    irows[[length(irows)+1L]]<-data.frame(
      record_id=sc(z$record_id),insight_type=sc(v$insight_type),
      summary=sc(v$summary),context=sc(v$context),
      evidence_basis=sc(v$evidence_basis),
      evidence_quote=sc(v$source_quote),
      source_anchor=sc(v$source_anchor),
      reason=sc(v$reason),human_decision="",reviewer="",
      review_complete=FALSE,stringsAsFactors=FALSE)
  empty<-function(columns) {
    x<-as.data.frame(setNames(rep(list(character()),length(columns)),
                            columns),stringsAsFactors=FALSE)
    x$review_complete<-logical()
    x
  }
  mechanisms<-if(length(mrows))do.call(rbind,mrows) else empty(c(
    "record_id","mechanism_id","status","evidence_quote","source_anchor",
    "model_reason","human_decision","reviewer"))
  novelty<-if(length(nrows))do.call(rbind,nrows) else empty(c(
    "record_id","proposed_label","working_definition","support_status",
    "closest_approved_mechanism_id","reason","evidence_quote",
    "source_anchor","human_decision","reviewer"))
  cases<-if(length(crows))do.call(rbind,crows) else empty(c(
    "record_id","fulltext_case_status","candidate_case_count",
    "candidate_labels","assessment_reason","human_decision","reviewer"))
  insights_table<-if(length(irows))do.call(rbind,irows) else empty(c(
    "record_id","insight_type","summary","context","evidence_basis",
    "evidence_quote","source_anchor","reason","human_decision","reviewer"))
  availability<-inames
  flagged_mechanisms<-vapply(m,function(x)
    isTRUE(x$evidence_review_required),logical(1))
  flagged_insights<-vapply(insights,function(x)
    isTRUE(x$evidence_review_required),logical(1))
  availability$mechanism_evidence_review_required<-
    availability$record_id%in%mids[flagged_mechanisms]
  availability$insights_evidence_review_required<-
    availability$record_id%in%ids[flagged_insights]
  availability$mechanism_coded<-availability$record_id%in%mids
  availability$insights_coded<-availability$record_id%in%ids
  availability$abstract_no_case_insights_eligible<-
    if(is.null(imeta))FALSE else
      availability$record_id%in%as.character(unlist(
        imeta$selected_no_abstract_case_ids,use.names=FALSE))
  checksum<-substr(digest::digest(c(sha(mfile),
    if(file.exists(ledger))sha(ledger) else "none",
    if(!is.null(ifile)&&file.exists(ifile))sha(ifile) else "none"),
    algo="sha256"),1L,12L)
  dest<-file.path(dirname(mechanism_dir),paste0("review-v1-",checksum))
  outfiles<-file.path(dest,c("mechanism_review.csv","novelty_review.csv",
    "case_presence_review.csv","insights_review.csv",
    "publication_inventory.csv","manifest.json"))
  if(file.exists(outfiles[[6L]])) {
    prior<-jsonlite::fromJSON(outfiles[[6L]],simplifyVector=FALSE)
    if(!identical(prior$mechanism_ledger_sha256,
        if(file.exists(ledger))sha(ledger) else "")||
       !identical(prior$insights_ledger_sha256,
        if(!is.null(ifile)&&file.exists(ifile))sha(ifile) else ""))
      stop("Review export exists with different source checksums")
    message("[OSTARA FT review] Existing review: ",dest)
    return(invisible(setNames(as.list(c(outfiles[1:5],dest)),
      c("mechanisms","novelty","case_presence","insights",
        "inventory","review_dir"))))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Review directory already contains files without a valid manifest")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  tables<-list(mechanisms,novelty,cases,insights_table,availability)
  for(j in seq_along(tables))
    utils::write.csv(tables[[j]],outfiles[[j]],row.names=FALSE,na="")
  jsonlite::write_json(list(schema="ostara-first10-fulltext-review-v1",
    mechanism_run=mechanism_dir,
    mechanism_ledger_sha256=if(file.exists(ledger))sha(ledger) else "",
    insights_run=if(is.null(insights_dir))"" else insights_dir,
    insights_ledger_sha256=if(!is.null(ifile)&&file.exists(ifile))
      sha(ifile) else "",
    output_sha256=as.list(vapply(outfiles[1:5],sha,character(1))),
    all_findings_require_human_review=TRUE),outfiles[[6L]],
    auto_unbox=TRUE,pretty=TRUE)
  message("[OSTARA FT review] ",length(mids)," mechanism-coded publications; ",
          length(ids)," insights-coded; ",sum(availability$availability=="available"),
          " documents present; ",nrow(availability)-sum(
             availability$availability=="available")," missing.")
  message("[OSTARA FT review] ",dest)
  invisible(setNames(as.list(c(outfiles[1:5],dest)),
    c("mechanisms","novelty","case_presence","insights",
      "inventory","review_dir")))
}
