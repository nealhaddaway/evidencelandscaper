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
#' @param w09 Existing W09 run folder; defaults to the Ostara 190-record run.
#' @param selection_file Optional explicit record-ID selection CSV; otherwise authoritative W08 classifications.
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
    sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
    manifest_path<-file.path(w09,"manifest.json")
    if(!file.exists(manifest_path))stop("W09 source manifest missing.")
    wm<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(wm$schema,"w09-from-provisional-w08-cases-v1")||
       is.null(wm$source_w08)||is.null(wm$w08_assessments_sha256))
      stop("W09 is not a traceable W08 case-candidate retrieval queue.")
    w08<-normalizePath(wm$source_w08,winslash="/",mustWork=TRUE)
    model_file<-file.path(w08,"case_assessments.jsonl")
    if(!file.exists(model_file)||
       !identical(sha(model_file),wm$w08_assessments_sha256))
      stop("W08 model source does not match the frozen W09 cohort.")
    source_lines<-readLines(model_file,warn=FALSE,encoding="UTF-8")
    model<-lapply(source_lines[nzchar(trimws(source_lines))],
                  jsonlite::fromJSON,simplifyVector=FALSE)
    model_ids<-vapply(model,function(z)as.character(z$record_id),
                      character(1))
    model_status<-vapply(model,function(z)
      as.character(z$case_evidence_status),character(1))
    if(anyDuplicated(model_ids)||any(!model_status%in%
       c("specific_case","multiple_cases","no_identifiable_case","unclear")))
      stop("Invalid W08 model classifications.")
    hr<-file.path(w08,"human_adjudication")
    snapshots<-if(dir.exists(hr))
      list.dirs(hr,recursive=FALSE,full.names=TRUE) else character()
    snapshots<-snapshots[file.exists(file.path(snapshots,"manifest.json"))]
    if(length(snapshots)!=1L)
      stop("Expected exactly one imported W08 human-adjudication snapshot; found ",
           length(snapshots),". Import/reconcile the colleague decisions first.")
    human_dir<-snapshots[[1L]]
    hm<-jsonlite::fromJSON(file.path(human_dir,"manifest.json"),
                           simplifyVector=FALSE)
    effective_file<-file.path(human_dir,
                              "effective_case_classifications.csv")
    if(!file.exists(effective_file)||
       !identical(hm$source_w08_sha256,sha(model_file))||
       !identical(hm$effective_case_classifications_sha256,
                  sha(effective_file)))
      stop("Human W08 adjudication does not match its source or checksum.")
    effective<-utils::read.csv(effective_file,stringsAsFactors=FALSE)
    required<-c("record_id","model_case_status","final_case_status",
                "decision_origin")
    if(!all(required%in%names(effective))||
       anyNA(effective$record_id)||anyDuplicated(effective$record_id)||
       !setequal(effective$record_id,model_ids))
      stop("Invalid human W08 effective-classification table.")
    effective<-effective[match(model_ids,effective$record_id),,drop=FALSE]
    if(!identical(as.character(effective$model_case_status),
                  as.character(model_status))||
       any(!effective$final_case_status%in%c("specific_case",
           "multiple_cases","no_identifiable_case","unclear")))
      stop("Human classifications differ from original W08 model source.")
    if(!any(effective$decision_origin=="human_adjudicated"))
      stop("Imported W08 snapshot has no human-adjudicated decisions.")
    eligible<-effective$final_case_status%in%
      c("specific_case","multiple_cases",
        if(include_unclear)"unclear")
    ids<-as.character(effective$record_id[eligible])
    queue_check<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                                stringsAsFactors=FALSE)
    out_of_queue<-setdiff(ids,as.character(queue_check$record_id))
    audit<-effective
    audit$selected_for_case_extraction<-eligible
    audit$in_current_w09_queue<-audit$record_id%in%queue_check$record_id
    audit$human_snapshot_sha256<-sha(effective_file)
    audit_path<-file.path(w09,"case_data_human_selection_audit.csv")
    if(file.exists(audit_path)) {
      prior<-utils::read.csv(audit_path,stringsAsFactors=FALSE)
      if(!identical(prior,audit))
        stop("Existing human-selection audit differs. ",
             "Preserve the earlier audit and reconcile inputs before retrying.")
    } else utils::write.csv(audit,audit_path,row.names=FALSE,na="")
    message("[W09 case data] Using checksum-verified W08 human adjudication: ",
            human_dir)
    message("[W09 case data] Human adjudications: ",
            sum(effective$decision_origin=="human_adjudicated"),
            "; final case-positive: ",sum(eligible),
            "; remaining unclear: ",
            sum(effective$final_case_status=="unclear"),".")
    message("[W09 case data] Human-selection audit: ",audit_path)
    if(length(out_of_queue))
      stop(length(out_of_queue)," human-confirmed case-positive records are ",
           "absent from this old 190-publication W09 retrieval queue. ",
           "Do NOT treat this queue as complete; use the all-included ",
           "first-10% retrieval queue before continuing. First missing ID: ",
           out_of_queue[[1L]])
  } else {
    selection_file<-normalizePath(selection_file,winslash="/",mustWork=TRUE)
    chosen<-utils::read.csv(selection_file,stringsAsFactors=FALSE)
    if(!"record_id"%in%names(chosen))
      stop("Selection CSV must contain record_id.")
    ids<-as.character(chosen$record_id)
    message("[W09 case data] Explicit selection override: ",selection_file)
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
