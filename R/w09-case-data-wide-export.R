#' Export W09 case data to a human-readable wide CSV
#'
#' One row per case. Every CSV question becomes an answer column followed
#' immediately by the supporting evidence quotations and source anchors.
#' This reads existing checkpoints only and makes no API requests.
#' @param w09 Existing W09 run directory, defaults to the Ostara 190-record run.
#' @param extraction_dir Exact case-data extraction directory. If NULL, prefer
#'   the known revised 41-field run; otherwise require an unambiguous run.
#' @param include_status Add a reporting status column after each evidence column.
#' @return Absolute path to exported CSV, invisibly.
#' @export
export_w09_cases_wide_evidence <- function(w09=NULL,extraction_dir=NULL,
                                            include_status=FALSE) {
  if(is.null(w09)) {
    root<-open_map("Ostara_collective_action_map")
    w09<-file.path(root,"runs","W09",
      "from-w08-cases-190-4459dd341126-with-unclear")
  }
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  if(is.null(extraction_dir)) {
    base<-file.path(w09,"case_data_extraction")
    dirs<-list.dirs(base,recursive=FALSE,full.names=TRUE)
    dirs<-dirs[file.exists(file.path(dirs,"case_data.jsonl"))]
    revised<-dirs[startsWith(basename(dirs),"csv-111e823925d2-prompt-")]
    if(length(revised)==1L) {
      extraction_dir<-revised[[1L]]
    } else if(length(dirs)==1L) {
      extraction_dir<-dirs[[1L]]
    } else stop("Multiple case extraction runs found; supply extraction_dir. Available: ",
                paste(basename(dirs),collapse=", "))
  }
  extraction_dir<-normalizePath(extraction_dir,winslash="/",mustWork=TRUE)
  if(!identical(dirname(dirname(extraction_dir)),w09))
    stop("Extraction directory does not belong to supplied W09 run.")
  ledger<-file.path(extraction_dir,"case_data.jsonl")
  mapping<-file.path(extraction_dir,"field_mapping.csv")
  if(!file.exists(ledger)||!file.exists(mapping))
    stop("Missing case_data.jsonl or field_mapping.csv.")
  spec<-utils::read.csv(mapping,stringsAsFactors=FALSE,check.names=FALSE,
                        fileEncoding="UTF-8-BOM")
  if(!all(c("field_id","Question")%in%names(spec)))
    stop("Field mapping missing field_id or Question.")
  if(anyDuplicated(spec$field_id))stop("Duplicate field IDs in mapping.")
  lines<-readLines(ledger,warn=FALSE,encoding="UTF-8")
  records<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
                  simplifyVector=FALSE)
  if(!length(records))stop("No checkpointed W09 records to export.")
  ids<-vapply(records,function(z)z$record_id,character(1))
  if(anyDuplicated(ids))stop("Duplicate checkpointed publications.")
  # Record-level audit flags are retained as metadata. This audit is known
  # to cover offline recovery; new API validation warnings are not guaranteed
  # to appear in this file.
  audit<-file.path(extraction_dir,"revalidation_audit.jsonl")
  audit_records<-if(file.exists(audit)) {
    a<-readLines(audit,warn=FALSE,encoding="UTF-8")
    lapply(a[nzchar(trimws(a))],jsonlite::fromJSON,simplifyVector=FALSE)
  } else list()
  scalar<-function(x) {
    if(is.null(x)||!length(x))return("")
    x<-unlist(x,use.names=FALSE)
    if(!length(x))return("")
    paste(as.character(x),collapse=" | ")
  }
  headings<-unlist(lapply(seq_len(nrow(spec)),function(i) {
    h<-paste0(sprintf("%02d",i),". ",trimws(spec$Question[[i]]))
    c(h,paste0(h," [evidence]"),
      if(include_status)paste0(h," [reporting status]"))
  }),use.names=FALSE)
  headings<-make.unique(headings,sep=" (duplicate ")
  rows<-list()
  for(rec in records) {
    for(case in rec$cases) {
      row<-c(record_id=scalar(rec$record_id),
             case_id=scalar(case$case_id),
             assessment_reason=scalar(case$assessment_reason),
             case_boundary_uncertainty=scalar(case$case_boundary_uncertainty))
      review<-Filter(function(z)identical(z$record_id,rec$record_id),
                     audit_records)
      if(length(review)) {
        review<-review[[length(review)]]
        flags<-vapply(review$evidence_flags,function(x)scalar(x),character(1))
        repairs<-if(length(review$deterministic_status_repairs))
          vapply(review$deterministic_status_repairs,function(x)
            paste0(scalar(x$case_id),": ",scalar(x$field_id)," -> ",
                   scalar(x$assigned_status)),character(1)) else character()
        row<-c(row,evidence_review_required=if(isTRUE(review$evidence_review_required))
          "TRUE" else "FALSE",
          review_notes=paste(c(flags,repairs),collapse="\n"))
      } else row<-c(row,evidence_review_required="not_audited",
                    review_notes="")
      for(i in seq_len(nrow(spec))) {
        f<-case$fields[[spec$field_id[[i]]]]
        vals<-if(is.null(f))character() else if(!is.null(f$values))
          unlist(f$values,use.names=FALSE) else f$value
        answer<-if(length(vals))paste(as.character(vals),collapse="\n") else ""
        ev<-if(is.list(f$evidence)&&length(f$evidence))
          vapply(f$evidence,function(e)
            paste0(scalar(e$anchor),": ",scalar(e$quote)),character(1))
          else character()
        row<-c(row,answer,paste(ev,collapse="\n"),
               if(include_status)scalar(f$reporting_status))
      }
      rows[[length(rows)+1L]]<-row
    }
  }
  if(!length(rows))stop("Checkpoint contains no identified cases.")
  columns<-c("record_id","case_id","assessment_reason",
             "case_boundary_uncertainty","evidence_review_required",
             "review_notes",headings)
  tab<-as.data.frame(do.call(rbind,rows),stringsAsFactors=FALSE,
                     check.names=FALSE)
  names(tab)<-columns
  output<-file.path(extraction_dir,"w09_cases_wide_with_evidence.csv")
  utils::write.csv(tab,output,row.names=FALSE,na="",fileEncoding="UTF-8")
  message("[W09 export] ",nrow(tab)," cases from ",length(records),
          " checkpointed publications; ",nrow(spec)," questions.")
  message("[W09 export] Saved: ",output)
  invisible(output)
}
