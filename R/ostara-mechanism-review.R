#' Prepare an auditable human mechanism-review worksheet from W09 and W10
#'
#' Reads existing case extraction, first-pass case eligibility and optional
#' second-pass eligibility without modifying either. This worksheet is the
#' human verification interface for W07 mechanism overrides.
#' @param extraction_dir Completed W09 v2 extraction directory.
#' @param w10_run Completed first-pass W10 eligibility run directory.
#' @param second_pass_dir Optional W10 full-text second-pass directory.
#' @param output_dir New directory for review worksheets.
#' @return Paths of the mechanism and novelty review worksheets.
#' @export
prepare_ostara_mechanism_review <- function(extraction_dir,w10_run,
                                             second_pass_dir=NULL,output_dir) {
  reg<-file.path(extraction_dir,"fulltext_case_registry_normalised.jsonl")
  first<-file.path(w10_run,"w10_decisions.jsonl")
  if(!file.exists(reg)||!file.exists(first))
    stop("Completed W09 registry and W10 eligibility ledger are required.")
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Review output directory already exists and is not empty.")
  readjl<-function(p)lapply(Filter(nzchar,readLines(p,warn=FALSE,encoding="UTF-8")),
                             jsonlite::fromJSON,simplifyVector=FALSE)
  records<-readjl(reg)
  firstpass<-readjl(first)
  fids<-vapply(firstpass,function(x)x$observation_id,character(1))
  if(anyDuplicated(fids))stop("Duplicated W10 observation IDs")
  eligibility<-setNames(vapply(firstpass,function(x)x$overall,character(1)),fids)
  if(!is.null(second_pass_dir)){
    second<-file.path(second_pass_dir,"w10_fulltext_decisions.jsonl")
    if(!file.exists(second)){
      paths<-list.files(second_pass_dir,pattern="\\.jsonl$",full.names=TRUE)
      stop("Second-pass decision filename must be verified; available JSONL: ",
           paste(basename(paths),collapse=", "))
    }
    secondrows<-readjl(second)
    for(x in secondrows) {
      if(!x$observation_id%in%names(eligibility))
        stop("Unknown W10 second-pass observation")
      eligibility[[x$observation_id]]<-x$overall
    }
  }
  scalar<-function(x)if(is.null(x)||!length(x))"" else as.character(x[[1L]])
  mechanism<-list();novelty<-list()
  for(z in records) {
    id<-z$record_id
    case_ids<-vapply(seq_along(z$cases),function(j)
      paste0(id,"-case-",sprintf("%03d",j)),character(1))
    if(any(!case_ids%in%names(eligibility)))
      stop("W10 is missing case eligibility decisions for publication ",id)
    eligible_idx<-which(eligibility[case_ids]=="eligible")
    if(!length(eligible_idx))next
    eligible<-case_ids[eligible_idx]
    validation<-z$abstract_mechanism_validation
    for(v in validation){
      code<-scalar(v$mechanism_id)
      if(!nzchar(code))stop("Missing W09 validated mechanism code")
      mechanism[[length(mechanism)+1L]]<-data.frame(
        record_id=id,mechanism_id=code,
        w09_assessment=scalar(v$status),abstract_assigned=TRUE,
        human_decision="",review_complete=FALSE,eligible_case_verified=TRUE,
        case_ids=paste(eligible,collapse="; "),
        evidence_quote=scalar(v$source_quote),source_anchor=scalar(v$source_anchor),
        reviewer="",review_timestamp="",stringsAsFactors=FALSE)
    }
    for(j in eligible_idx){
      ca<-z$cases[[j]]
      for(v in ca$additional_mechanisms) {
        code<-scalar(v$mechanism_id)
        if(code%in%vapply(validation,function(y)scalar(y$mechanism_id),character(1)))
          next
        mechanism[[length(mechanism)+1L]]<-data.frame(
          record_id=id,mechanism_id=code,
          w09_assessment="additional",abstract_assigned=FALSE,
          human_decision="",review_complete=FALSE,eligible_case_verified=TRUE,
          case_ids=case_ids[[j]],
          evidence_quote=scalar(v$source_quote),source_anchor=scalar(v$source_anchor),
          reviewer="",review_timestamp="",stringsAsFactors=FALSE)
      }
    }
    for(j in seq_along(z$ontology_gaps)) {
      g<-z$ontology_gaps[[j]]
      novelty[[length(novelty)+1L]]<-data.frame(
        record_id=id,candidate_id=paste0("FT-",id,"-",sprintf("%03d",j)),
        case_id=paste(eligible,collapse="; "),
        w07_candidate_id="",human_novelty_decision="uncertain",
        closest_approved_mechanism_id="",
        proposed_label=scalar(g$description),working_definition="",
        evidence_quote=scalar(g$source_quote),source_anchor=scalar(g$source_anchor),
        reviewer="",review_timestamp="",stringsAsFactors=FALSE)
    }
  }
  if(length(mechanism)) {
    d<-do.call(rbind,mechanism)
    # Combine a mechanism mentioned in several eligible cases into one
    # publication-level assessment; preserve all case IDs.
    key<-paste(d$record_id,d$mechanism_id,sep="::")
    if(anyDuplicated(key)){
      groups<-split(seq_len(nrow(d)),factor(key,levels=unique(key)))
      d<-do.call(rbind,lapply(groups,function(k){
        v<-d[k[[1]],,drop=FALSE]
        v$case_ids<-paste(unique(unlist(strsplit(paste(d$case_ids[k],collapse="; "),"; ",fixed=TRUE))),collapse="; ")
        v
      }))
    }
  } else d<-data.frame(record_id=character(),mechanism_id=character(),
     w09_assessment=character(),abstract_assigned=logical(),
     human_decision=character(),review_complete=logical(),
     eligible_case_verified=logical(),case_ids=character(),
     evidence_quote=character(),source_anchor=character(),
     reviewer=character(),review_timestamp=character())
  n<-if(length(novelty))do.call(rbind,novelty) else data.frame(
    record_id=character(),candidate_id=character(),case_id=character(),
    w07_candidate_id=character(),human_novelty_decision=character(),
    closest_approved_mechanism_id=character(),proposed_label=character(),
    working_definition=character(),evidence_quote=character(),
    source_anchor=character(),reviewer=character(),review_timestamp=character())
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  a<-file.path(output_dir,"mechanism_review.csv")
  b<-file.path(output_dir,"novelty_review.csv")
  utils::write.csv(d,a,row.names=FALSE,na="")
  utils::write.csv(n,b,row.names=FALSE,na="")
  hash<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  jsonlite::write_json(list(
    schema="ostara-mechanism-review-v1",
    w09_registry_sha256=hash(reg),w10_eligibility_sha256=hash(first),
    second_pass_dir=if(is.null(second_pass_dir))NULL else normalizePath(second_pass_dir),
    mechanism_review_sha256=hash(a),novelty_review_sha256=hash(b)),
    file.path(output_dir,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(list(mechanisms=a,novelty=b))
}
