#' Build W10 human-review worksheets from comprehensive FT mechanism extraction
#'
#' Retains prior W09/W10 review worksheets as a baseline, writing a NEW
#' versioned review directory. No extracted model decision is auto-approved.
#' @param review_dir Original W09/W10 mechanism review directory.
#' @param supplement_dir Completed supplementary extraction directory.
#' @return New worksheet paths.
#' @export
prepare_ostara_supplement_review <- function(review_dir,supplement_dir) {
  review_dir<-normalizePath(review_dir,mustWork=TRUE)
  supplement_dir<-normalizePath(supplement_dir,mustWork=TRUE)
  ledger<-file.path(supplement_dir,"mechanism_assessments.jsonl")
  man<-file.path(supplement_dir,"manifest.json")
  if(!all(file.exists(c(ledger,man))))stop("Supplemental extraction incomplete or missing")
  hash<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  original_m<-file.path(review_dir,"mechanism_review.csv")
  original_n<-file.path(review_dir,"novelty_review.csv")
  if(!all(file.exists(c(original_m,original_n))))stop("Original review worksheets missing")
  base<-utils::read.csv(original_m,stringsAsFactors=FALSE,check.names=FALSE)
  prior_n<-utils::read.csv(original_n,stringsAsFactors=FALSE,check.names=FALSE)
  extracted<-lapply(Filter(nzchar,readLines(ledger,warn=FALSE,encoding="UTF-8")),
                    jsonlite::fromJSON,simplifyVector=FALSE)
  meta<-jsonlite::fromJSON(man,simplifyVector=FALSE)
  expected<-unlist(meta$eligible_record_ids,use.names=FALSE)
  found<-vapply(extracted,function(x)x$record_id,character(1))
  if(anyDuplicated(found)||!setequal(found,expected))
    stop("Supplementary mechanism extraction is not complete for all selected records")
  old_ids<-unique(base$record_id)
  unknown<-setdiff(found,unique(c(old_ids,prior_n$record_id)))
  if(length(unknown))stop("Supplementary output includes publications absent from W10 review")
  out<-file.path(review_dir,paste0("supplement_review-",
                                   substr(hash(ledger),1,12)))
  if(dir.exists(out)&&length(list.files(out,all.files=TRUE,no..=TRUE)))
    stop("Supplement review already exists; refusing overwrite")
  mech<-list();novel<-list()
  get<-function(x,k) {
    y<-x[[k]]
    if(is.null(y)||!length(y))"" else
      paste(as.character(unlist(y,use.names=FALSE)),collapse="; ")
  }
  for(z in extracted) {
    id<-z$record_id
    # W10-eligible case references in original review are authoritative
    # for which prior cases may be considered at this stage.
    eligible<-unique(unlist(strsplit(
      paste(base$case_ids[base$record_id==id],
            prior_n$case_id[prior_n$record_id==id],collapse="; "),
      ";",fixed=TRUE),use.names=FALSE))
    eligible<-trimws(eligible)
    eligible<-eligible[nzchar(eligible)]
    abstract<-base$mechanism_id[base$record_id==id&
                                base$abstract_assigned%in%TRUE]
    for(v in z$mechanism_inventory) {
      code<-get(v,"mechanism_id")
      status<-get(v,"status")
      caseids<-unlist(v$case_ids,use.names=FALSE)
      caseids<-as.character(caseids)
      if(status=="implemented_case"&&
         (!length(caseids)||any(!caseids%in%eligible)))
        stop("Mechanism inventory references an ineligible or missing case: ",id)
      if(status!="implemented_case")caseids<-character()
      # Include all approved codes in the publication inventory;
      # only case-implemented codes are candidates for effective coding.
      mech[[length(mech)+1L]]<-data.frame(
        record_id=id,mechanism_id=code,w09_assessment=status,
        abstract_assigned=code%in%abstract,
        human_decision="",review_complete=FALSE,
        eligible_case_verified=FALSE,
        case_ids=paste(caseids,collapse="; "),
        evidence_quote=get(v,"source_quote"),
        source_anchor=get(v,"source_anchor"),
        reviewer="",review_timestamp="",stringsAsFactors=FALSE)
    }
    for(j in seq_along(z$abstract_novelty_validation)) {
      v<-z$abstract_novelty_validation[[j]]
      novel[[length(novel)+1L]]<-data.frame(
        record_id=id,
        candidate_id=paste0("FT-RECHECK-",id,"-",sprintf("%03d",j)),
        case_id=get(v,"case_ids"),
        w07_candidate_id=get(v,"abstract_candidate_id"),
        model_novelty_status=get(v,"status"),
        human_novelty_decision="uncertain",
        closest_approved_mechanism_id=get(v,"closest_approved_mechanism_id"),
        proposed_label=if(nzchar(get(v,"revised_label")))get(v,"revised_label") else get(v,"original_proposal"),
        working_definition=get(v,"revised_definition"),
        evidence_quote=get(v,"source_quote"),
        source_anchor=get(v,"source_anchor"),
        reviewer="",review_timestamp="",stringsAsFactors=FALSE)
    }
    for(j in seq_along(z$new_novelty)){
      v<-z$new_novelty[[j]]
      case_ids<-as.character(unlist(v$case_ids,use.names=FALSE))
      if(!length(case_ids)||any(!case_ids%in%eligible))
        stop("New FT candidate links to no eligible W10 case: ",id)
      novel[[length(novel)+1L]]<-data.frame(
        record_id=id,candidate_id=paste0("FT-NEW-",id,"-",sprintf("%03d",j)),
        case_id=paste(case_ids,collapse="; "),w07_candidate_id="",
        model_novelty_status="new_ft_proposal",
        human_novelty_decision="uncertain",
        closest_approved_mechanism_id=get(v,"closest_approved_mechanism_id"),
        proposed_label=get(v,"proposed_label"),
        working_definition=get(v,"working_definition"),
        evidence_quote=get(v,"source_quote"),source_anchor=get(v,"source_anchor"),
        reviewer="",review_timestamp="",stringsAsFactors=FALSE)
    }
  }
  if(!length(mech))stop("No full-text mechanism inventory rows")
  mechanisms<-do.call(rbind,mech)
  if(anyDuplicated(paste(mechanisms$record_id,mechanisms$mechanism_id)))
    stop("Duplicate publication-mechanism assessment")
  if(length(novel))novelty<-do.call(rbind,novel) else {
    novelty<-prior_n[FALSE,,drop=FALSE]
    novelty$model_novelty_status<-character()
  }
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  m<-file.path(out,"mechanism_review.csv")
  n<-file.path(out,"novelty_review.csv")
  utils::write.csv(mechanisms,m,row.names=FALSE,na="")
  utils::write.csv(novelty,n,row.names=FALSE,na="")
  jsonlite::write_json(list(schema="ostara-FT-supplement-review-v1",
    source_review_dir=review_dir,source_w07_run={
      source_manifest<-jsonlite::fromJSON(file.path(review_dir,"manifest.json"),
                                          simplifyVector=FALSE)
      source_manifest$w07_run},
    w07_run={
      source_manifest<-jsonlite::fromJSON(file.path(review_dir,"manifest.json"),
                                          simplifyVector=FALSE)
      source_manifest$w07_run},
    supplemental_ledger_sha256=hash(ledger),
    mechanism_review_sha256=hash(m),novelty_review_sha256=hash(n)),
    file.path(out,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(list(mechanisms=m,novelty=n,review_dir=out))
}
