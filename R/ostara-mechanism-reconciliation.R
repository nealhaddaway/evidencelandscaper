#' Reconcile verified Ostara full-text mechanism decisions with W07
#'
#' This is a deterministic, read-only overlay. W07 coding and W09 model outputs
#' are never rewritten. Only W10 decisions explicitly marked complete for an
#' eligible case publication can supersede abstract assignments.
#' @param w07_coding_jsonl Immutable W07 coding JSONL path.
#' @param verification_csv Human-adjudicated publication-mechanism decisions CSV.
#' @param output_dir New, empty directory for reconciled outputs.
#' @return Paths of written CSV files.
#' @export
reconcile_ostara_mechanisms <- function(w07_coding_jsonl, verification_csv,
                                         output_dir) {
  if(!file.exists(w07_coding_jsonl)||!file.exists(verification_csv))
    stop("W07 coding and human verification input files must exist.")
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Refusing to overwrite nonempty reconciliation directory: ",output_dir)
  readjl<-function(p) {
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  w07<-readjl(w07_coding_jsonl)
  ids<-vapply(w07,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(ids)||any(!nzchar(ids)))stop("Invalid W07 record identifiers")
  original<-do.call(rbind,lapply(w07,function(z) {
    ms<-z$mechanisms
    if(is.null(ms)||!length(ms))return(NULL)
    codes<-unique(vapply(ms,function(m)as.character(m$mechanism_id),character(1)))
    data.frame(record_id=z$record_id,mechanism_id=codes,
               stringsAsFactors=FALSE)
  }))
  if(is.null(original))original<-data.frame(record_id=character(),
                                              mechanism_id=character())
  decisions<-utils::read.csv(verification_csv,stringsAsFactors=FALSE,
                              check.names=FALSE,fileEncoding="UTF-8-BOM")
  required<-c("record_id","mechanism_id","human_decision","review_complete",
              "eligible_case_verified","case_ids","evidence_quote",
              "source_anchor","reviewer","review_timestamp")
  if(!all(required%in%names(decisions)))
    stop("Missing verification fields: ",paste(setdiff(required,names(decisions)),collapse=", "))
  if(anyNA(decisions[,required])||
     any(!nzchar(trimws(as.matrix(decisions[,c("record_id","mechanism_id",
                                                "human_decision","reviewer")])))))
    stop("Missing identifiers, decision or reviewer")
  accepted<-c("confirmed_case","mentioned_only","rejected","uncertain")
  if(any(!decisions$human_decision%in%accepted))
    stop("Unknown human decision")
  flag<-function(x) {
    y<-tolower(trimws(as.character(x)))
    if(any(!y%in%c("true","false")))stop("Review flags must be explicit TRUE/FALSE")
    y=="true"
  }
  complete<-flag(decisions$review_complete)
  eligible<-flag(decisions$eligible_case_verified)
  if(any(complete & !eligible))
    stop("Cannot complete verification for a publication without an eligible verified case")
  if(any(!decisions$record_id%in%ids))
    stop("Verification contains records not in W07")
  keys<-paste(decisions$record_id,decisions$mechanism_id,sep="\u001f")
  if(anyDuplicated(keys))stop("Duplicate human decisions for a publication/mechanism")
  if(any(complete & decisions$human_decision=="confirmed_case" &
         (!nzchar(trimws(decisions$case_ids))|
          !nzchar(trimws(decisions$evidence_quote))|
          !nzchar(trimws(decisions$source_anchor)))))
    stop("Confirmed mechanisms require case IDs, supporting quotation and source anchor")
  # A complete review must be declared for the entire publication, not selectively
  # for one mechanism. It must explicitly cover every abstract-assigned code.
  by_record<-split(seq_len(nrow(decisions)),decisions$record_id)
  for(id in names(by_record)) {
    rows<-by_record[[id]]
    if(length(unique(complete[rows]))!=1L ||
       length(unique(eligible[rows]))!=1L)
      stop("Inconsistent publication-level verification flags for: ",id)
    if(complete[rows][[1L]]) {
      abs_codes<-original$mechanism_id[original$record_id==id]
      if(!all(abs_codes%in%decisions$mechanism_id[rows]))
        stop("Incomplete W07 mechanism coverage for publication: ",id)
    }
  }
  effective<-original
  effective$status<-rep("abstract_only",nrow(effective))
  for(i in seq_len(nrow(decisions))) {
    d<-decisions[i,,drop=FALSE]
    if(!complete[[i]]||!eligible[[i]]||d$human_decision=="uncertain")next
    existing<-which(effective$record_id==d$record_id&
                    effective$mechanism_id==d$mechanism_id)
    if(d$human_decision=="confirmed_case") {
      if(length(existing))effective$status[existing]<-"fulltext_confirmed"
      else effective<-rbind(effective,data.frame(
        record_id=d$record_id,mechanism_id=d$mechanism_id,
        status="fulltext_added"))
    } else if(length(existing)) {
      effective$status[existing]<-if(d$human_decision=="rejected")
        "fulltext_rejected" else "mentioned_only"
    }
  }
  effective$effective_case_assigned<-!effective$status%in%
    c("fulltext_rejected","mentioned_only")
  original$source<-"immutable_W07_abstract"
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  paths<-file.path(output_dir,c("original_w07_codes.csv",
                               "human_fulltext_decisions.csv",
                               "effective_mechanism_codes.csv",
                               "reconciliation_manifest.json"))
  utils::write.csv(original,paths[[1]],row.names=FALSE,na="")
  utils::write.csv(decisions,paths[[2]],row.names=FALSE,na="")
  utils::write.csv(effective,paths[[3]],row.names=FALSE,na="")
  hash<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  jsonlite::write_json(list(
    schema="ostara-mechanism-reconciliation-v1",
    w07_sha256=hash(w07_coding_jsonl),
    verification_sha256=hash(verification_csv),
    original_output_sha256=hash(paths[[1]]),
    human_output_sha256=hash(paths[[2]]),
    effective_output_sha256=hash(paths[[3]])),
    paths[[4]],auto_unbox=TRUE,pretty=TRUE)
  invisible(setNames(paths,c("w07","decisions","effective","manifest")))
}
