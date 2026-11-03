#' Re-extract an existing W09 case-data batch using revised inputs
#'
#' Uses the previously saved batch selection, never alters the earlier
#' extraction, and delegates all schema validation and checkpointing to
#' run_w09_case_data_extraction(). The revised CSV is authoritative,
#' including its 'Topics of interest' column (not the obsolete 'Examples').
#'
#' @param prompt_file Path to the revised prompt TXT, or NULL to choose interactively.
#' @param schema_file Path to the revised case-data CSV, or NULL to choose interactively.
#' @param w09 Existing W09 run folder; defaults to the Ostara 190-record run.
#' @param selection_file Optional explicit record-ID selection CSV; otherwise all W08-confirmed case-positive full texts.
#' @param max_records Number of pending records to process; 0 means all.
#' @param include_unclear Include W08 unclear records with case candidates (default FALSE).
#' @param revalidate_only Revalidate archived responses without API usage.
#' @return Extraction directory, invisibly.
#' @export
rerun_w09_case_data_batch <- function(prompt_file=NULL, schema_file=NULL, w09=NULL,
                                      selection_file=NULL, max_records=0L,
                                      revalidate_only=FALSE,
                                      include_unclear=FALSE) {
  if(is.null(prompt_file)) {
    message("[W09 case data] Select your REVISED PROMPT (.txt) in the file chooser.")
    if(!interactive())stop("Supply prompt_file in non-interactive sessions.")
    prompt_file<-file.choose()
  }
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  message("[W09 case data] Prompt selected: ",basename(prompt_file))
  if(is.null(schema_file)) {
    message("[W09 case data] Select your REVISED CASE SCHEMA (.csv) in the file chooser.")
    if(!interactive())stop("Supply schema_file in non-interactive sessions.")
    schema_file<-file.choose()
  }
  schema_file<-normalizePath(schema_file,winslash="/",mustWork=TRUE)
  message("[W09 case data] Schema selected: ",basename(schema_file))
  if(is.null(w09)) {
    root<-open_map("Ostara_collective_action_map")
    w09<-file.path(root,"runs","W09",
      "from-w08-cases-190-4459dd341126-with-unclear")
  }
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  if(!is.logical(include_unclear)||length(include_unclear)!=1L||
     is.na(include_unclear))
    stop("include_unclear must be TRUE or FALSE.")
  if(length(max_records)!=1L||is.na(max_records)||max_records<0L||
     max_records!=as.integer(max_records))
    stop("max_records must be a nonnegative integer; 0 means all.")
  if(is.null(selection_file)) {
    selection_file<-file.path(w09,"w08_case_candidates.csv")
    if(!file.exists(selection_file))
      stop("W08 case-candidate selection missing: ",selection_file,
           ". Supply selection_file explicitly if using a different queue.")
    chosen<-utils::read.csv(selection_file,stringsAsFactors=FALSE)
    required<-c("record_id","case_evidence_status","candidate_count")
    if(!all(required%in%names(chosen)))
      stop("W08 case-candidate selection lacks required columns: ",
           paste(setdiff(required,names(chosen)),collapse=", "))
    allowed<-c("specific_case","multiple_cases")
    if(include_unclear)allowed<-c(allowed,"unclear")
    eligible<-chosen$case_evidence_status%in%allowed&
      !is.na(chosen$candidate_count)&chosen$candidate_count>0L
    message("[W09 case data] W08 candidates: ",nrow(chosen),
            "; case-positive: ",sum(eligible),
            "; W08 unclear held separately: ",
            sum(chosen$case_evidence_status=="unclear" & !eligible),
            ".")
    ids<-as.character(chosen$record_id[eligible])
  } else {
    selection_file<-normalizePath(selection_file,winslash="/",mustWork=TRUE)
    chosen<-utils::read.csv(selection_file,stringsAsFactors=FALSE)
    if(!"record_id"%in%names(chosen))
      stop("Selection CSV must contain record_id.")
    ids<-as.character(chosen$record_id)
    message("[W09 case data] Explicit selection: ",selection_file)
  }
  if(!length(ids)||anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))
    stop("No unique eligible case-positive publication IDs in selection.")
  queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                         stringsAsFactors=FALSE)
  if(!"record_id"%in%names(queue)||anyDuplicated(queue$record_id)||
     any(!ids%in%queue$record_id))
    stop("Selected case-positive IDs missing or duplicated in W09 queue.")
  has_fulltext<-vapply(ids,function(id) {
    any(file.exists(c(file.path(w09,"structured",paste0(id,".xml")),
                      file.path(w09,"structured",paste0(id,".json")),
                      file.path(w09,"pdfs",paste0(id,".pdf")))))
  },logical(1))
  message("[W09 case data] ",length(ids),
          " case-positive publications; ",sum(has_fulltext),
          " have saved full texts; ",sum(!has_fulltext),
          " missing and skipped.")
  ids<-ids[has_fulltext]
  if(!length(ids))stop("No selected case-positive publication has a full-text file.")
  message("[W09 case data] Processing all ",length(ids),
          " available publications (max_records=",
          max_records, "; 0 means all), preserving existing checkpoints.")
  run_w09_case_data_extraction(
    w09=w09,prompt_file=prompt_file,schema_file=schema_file,
    record_ids=ids,max_records=max_records,model="gpt-5.6-luna",
    max_pages=0L,revalidate_only=revalidate_only)
}
