#' Resume case-data extraction using authoritative W08 classifications
#'
#' Defaults to the checksum-verified colleague-adjudicated W08 cohort,
#' never the stale provisional W09 candidate list. Refuses incomplete
#' selection coverage rather than silently omitting confirmed cases.
#' Delegates extraction and checkpointing to run_w09_case_data_extraction().
#' The revised CSV is authoritative,
#' including its 'Topics of interest' column (not the obsolete 'Examples').
#'
#' @param prompt_file Path to the revised prompt TXT, or NULL to choose interactively.
#' @param schema_file Path to the revised case-data CSV, or NULL to choose interactively.
#' @param w09 All-included first-10% W09 run, discovered if NULL.
#' @param selection_file Deprecated and intentionally refused; use authoritative W08 decisions.
#' @param max_records Number of pending records to process; 0 means all.
#' @param include_unclear Include W08 cases left unclear after human adjudication (default FALSE).
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
  if(!is.null(selection_file))
    stop("Manual selection_file overrides are disabled for this workflow; ",
         "the human-adjudicated W08 snapshot is authoritative.")
  # The authoritative first-10% queue has every W04-retained publication.
  # Never route extraction from a provisional W08-to-W09 subset.
  if(is.null(w09)) {
    w09<-prepare_w09_first10_all_included()
  }
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  wm<-jsonlite::fromJSON(file.path(w09,"manifest.json"),
                          simplifyVector=FALSE)
  if(!identical(wm$schema,"w09-first10-all-included-v1"))
    stop("Case-data extraction requires the first10-all-included W09 queue, ",
         "not the old provisional 190-publication queue.")
  if(!is.logical(include_unclear)||length(include_unclear)!=1L||
     is.na(include_unclear))
    stop("include_unclear must be TRUE or FALSE.")
  if(length(max_records)!=1L||is.na(max_records)||max_records<0L||
     max_records!=as.integer(max_records))
    stop("max_records must be a nonnegative integer; 0 means all.")
  decisions<-.w09_first10_abstract_case_decisions(w09)
  if(!nzchar(decisions$human_snapshot))
    stop("No checksum-verified W08 human adjudication snapshot matches ",
         "this first-10% cohort. Cannot use model-only classifications.")
  human<-decisions$decisions
  good_status<-c("specific_case","multiple_cases")
  if(include_unclear)good_status<-c(good_status,"unclear")
  selected<-human$final_abstract_case_status%in%good_status
  ids<-sort(as.character(human$record_id[selected]))
  if(!length(ids))stop("No case-positive publications after adjudication.")
  queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                         stringsAsFactors=FALSE)
  if(anyDuplicated(queue$record_id)||any(!ids%in%queue$record_id))
    stop("Human-confirmed positive records missing from all-included W09 queue.")
  message("[W09 case data] Verified human adjudication: ",
          decisions$human_snapshot)
  message("[W09 case data] Human-reviewed: ",
          sum(human$decision_origin=="human_adjudicated"),
          "; final case-positive: ",
          sum(human$final_abstract_case_status%in%
              c("specific_case","multiple_cases")),
          "; still unclear: ",
          sum(human$final_abstract_case_status=="unclear"),".")
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
