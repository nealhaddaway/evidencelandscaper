#' Re-extract an existing W09 case-data batch using revised inputs
#'
#' Uses the previously saved batch selection, never alters the earlier
#' extraction, and delegates all schema validation and checkpointing to
#' run_w09_case_data_extraction(). The revised CSV is authoritative,
#' including its 'Topics of interest' column (not the obsolete 'Examples').
#'
#' @param prompt_file Path to the revised prompt TXT.
#' @param schema_file Path to the revised case-data CSV.
#' @param w09 Existing W09 run folder; defaults to the Ostara 190-record run.
#' @param selection_file Existing batch_10_selected.csv; discovered if unique.
#' @param max_records Number of remaining publications to run (default 10).
#' @param revalidate_only Revalidate archived responses without API usage.
#' @return Extraction directory, invisibly.
#' @export
rerun_w09_case_data_batch <- function(prompt_file, schema_file, w09=NULL,
                                      selection_file=NULL, max_records=10L,
                                      revalidate_only=FALSE) {
  if(is.null(w09)) {
    root<-open_map("Ostara_collective_action_map")
    w09<-file.path(root,"runs","W09",
      "from-w08-cases-190-4459dd341126-with-unclear")
  }
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  if(is.null(selection_file)) {
    base<-file.path(w09,"case_data_extraction")
    if(!dir.exists(base))stop("W09 case-data extraction directory not found.")
    found<-list.files(base,pattern="^batch_10_selected[.]csv$",
                      recursive=TRUE,full.names=TRUE)
    if(length(found)!=1L)
      stop("Expected exactly one saved batch_10_selected.csv, found ",
           length(found),". Supply selection_file explicitly.")
    selection_file<-found[[1L]]
  }
  selection_file<-normalizePath(selection_file,winslash="/",mustWork=TRUE)
  chosen<-utils::read.csv(selection_file,stringsAsFactors=FALSE)
  if(!"record_id"%in%names(chosen)||nrow(chosen)!=10L||
     anyNA(chosen$record_id)||anyDuplicated(chosen$record_id))
    stop("Saved W09 batch selection must contain 10 distinct record IDs.")
  ids<-as.character(chosen$record_id)
  if(length(max_records)!=1L||is.na(max_records)||max_records<1L||
     max_records!=as.integer(max_records))
    stop("max_records must be a positive integer.")
  message("[W09 case data] Reusing the saved batch: ",
          paste(ids,collapse=", "))
  run_w09_case_data_extraction(
    w09=w09,prompt_file=prompt_file,schema_file=schema_file,
    record_ids=ids,max_records=max_records,model="gpt-5.6-luna",
    max_pages=0L,revalidate_only=revalidate_only)
}
