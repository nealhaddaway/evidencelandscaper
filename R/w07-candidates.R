#' Export W07 ontology-gap candidates without modifying approved mechanism codes
#' @param w07 Completed W07 trial or full run directory.
#' @return CSV file path, invisibly.
#' @export
export_w07_candidates <- function(w07) {
  w07<-normalizePath(w07,winslash="/",mustWork=TRUE)
  p<-file.path(w07,"coding.jsonl")
  m<-file.path(w07,"manifest.json")
  if(!file.exists(p)||!file.exists(m))stop("W07 run files missing.")
  readjl<-function(path)lapply(readLines(path,warn=FALSE),jsonlite::fromJSON,simplifyVector=FALSE)
  rows<-readjl(p)
  manifest<-jsonlite::fromJSON(m,simplifyVector=FALSE)
  ids<-vapply(rows,function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(ids))stop("Duplicate W07 record IDs.")
  selected<-unlist(manifest$selected_record_ids,use.names=FALSE)
  if(!setequal(ids,selected))stop("W07 incomplete: candidate export requires every selected record.")
  sha<-function(path)digest::digest(file=path,algo="sha256",serialize=FALSE)
  out<-file.path(w07,"candidate_mechanisms.csv")
  columns<-c("source_record_id","candidate_description","gap_reason",
             "initiative_stage","review_status","team_decision","team_notes",
             "source_coding_sha256","ontology_sha256","prompt_sha256")
  pending<-Filter(function(x)isTRUE(x$ontology_gap),rows)
  colval<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  df<-as.data.frame(setNames(replicate(length(columns),character(),simplify=FALSE),columns),
                    stringsAsFactors=FALSE)
  for(z in pending)df[nrow(df)+1L,]<-c(
      colval(z$record_id),colval(z$proposed_new_mechanism),
      colval(z$ontology_gap_reason),colval(z$initiative_stage),
      "pending_review","","",sha(p),
      colval(manifest$ontology_sha256),colval(manifest$prompt_sha256))
  if(file.exists(out)) {
    prior<-utils::read.csv(out,stringsAsFactors=FALSE,check.names=FALSE)
    if(!identical(prior,df))
      stop("Candidate review file already exists or contains reviewer edits; refusing overwrite.")
    return(invisible(out))
  }
  utils::write.csv(df,out,row.names=FALSE,na="")
  message("[W07] Exported ",nrow(df)," candidate records; approved ontology remains unchanged.")
  invisible(out)
}
