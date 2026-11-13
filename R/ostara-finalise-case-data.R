#' Finalise and register the locked OSTARA full-text case extraction
#'
#' Consolidates the two cohort-wide exports (each already contains its relevant
#' pilot checkpoints) without concatenating the 20-paper pilot a second time.
#' Requires 100% of the saved accessible-fulltext inventories to have validated
#' W09 checkpoint records. Writes immutable, checksum-verifiable final outputs
#' and a versioned final manifest. Does not call the model or modify checkpoints.
#'
#' @param map_path OSTARA project root.
#' @param pilot_id Name of the locked pilot archive.
#' @param overwrite_final Allow replacement ONLY if existing files have exactly
#'   identical bytes; changed final outputs require an explicitly new version.
#' @return Invisible list of final output paths and counts.
#' @export
finalise_ostara_locked_case_data <- function(map_path,
     pilot_id="case_pilot_20_20261113",overwrite_final=FALSE) {
  norm<-function(x)normalizePath(x,winslash="/",mustWork=TRUE)
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  read_csv<-function(path)utils::read.csv(path,
    stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8-BOM")
  readjl<-function(path) {
    if(!file.exists(path))return(list())
    lines<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
           simplifyVector=FALSE)
  }
  id<-function(x)as.character(x$record_id)
  if(!is.logical(overwrite_final)||length(overwrite_final)!=1L||
     is.na(overwrite_final))stop("overwrite_final must be TRUE or FALSE.")
  map_path<-norm(map_path)
  pilot<-norm(file.path(map_path,"reports","ostara_case_pilots",pilot_id))
  pilot_manifest_file<-norm(file.path(pilot,"batch_manifest.json"))
  pilot_manifest<-jsonlite::fromJSON(pilot_manifest_file,simplifyVector=FALSE)
  prompt<-norm(file.path(pilot,"prompt.txt"))
  schema<-norm(file.path(pilot,"case_schema.csv"))
  if(!identical(sha(prompt),pilot_manifest$staged_prompt_sha256)||
     !identical(sha(schema),pilot_manifest$schema_sha256))
    stop("Archived pilot prompt/schema differ from approved manifest.")
  pilot_ids_file<-norm(file.path(pilot,"sampled_publications.csv"))
  if(!identical(sha(pilot_ids_file),pilot_manifest$sample_sha256))
    stop("Approved pilot sample register was changed.")
  pilot_ids<-read_csv(pilot_ids_file)$record_id
  if(length(pilot_ids)!=20L||anyDuplicated(pilot_ids))
    stop("Expected 20 unique pilot publication IDs.")
  root<-file.path(map_path,"reports","ostara_case_fulltext_locked")
  cohorts<-c("remaining90","first10")
  case_sets<-list();status_sets<-list();provenance<-list()
  for(cohort in cohorts){
    d<-norm(file.path(root,cohort))
    selection_file<-norm(file.path(d,"selection_manifest.json"))
    selection<-jsonlite::fromJSON(selection_file,simplifyVector=FALSE)
    inventory_file<-norm(file.path(d,"selected_accessible_fulltexts.csv"))
    case_file<-norm(file.path(d,"all_cases_wide.csv"))
    status_file<-norm(file.path(d,"all_publications_status.csv"))
    if(!identical(selection$cohort,cohort)||
       !identical(selection$prompt_sha256,sha(prompt))||
       !identical(selection$schema_sha256,sha(schema))||
       !identical(selection$pilot_manifest_sha256,sha(pilot_manifest_file))||
       !identical(selection$inventory_sha256,sha(inventory_file)))
      stop("Selection provenance mismatch for ",cohort)
    w09<-norm(selection$w09)
    w09_manifest<-file.path(w09,"case_data_extraction",paste0(
      "csv-",substr(sha(schema),1,12),
      "-prompt-",substr(sha(prompt),1,12),
      "-model-gpt-5.6-luna"))
    extraction_manifest<-norm(file.path(w09_manifest,"manifest.json"))
    stored<-jsonlite::fromJSON(extraction_manifest,simplifyVector=FALSE)
    if(!identical(stored$prompt_sha256,sha(prompt))||
       !identical(stored$csv_sha256,sha(schema))||
       !identical(stored$queue_sha256,
                  sha(file.path(w09,"retrieval_queue.csv")))||
       !identical(stored$model,"gpt-5.6-luna"))
      stop("W09 checkpoint has different extraction inputs: ",cohort)
    ledger_file<-norm(file.path(w09_manifest,"case_data.jsonl"))
    records<-readjl(ledger_file)
    checkpoint_ids<-vapply(records,id,character(1))
    if(anyDuplicated(checkpoint_ids))
      stop("Duplicate W09 checkpoint record IDs: ",cohort)
    inventory<-read_csv(inventory_file)
    statuses<-read_csv(status_file)
    cases<-read_csv(case_file)
    if(!all(c("record_id","source_sha256")%in%names(inventory))||
       !all(c("record_id","extraction_status","case_count")%in%names(statuses))||
       !all(c("record_id","case_id")%in%names(cases)))
      stop("Required inventory/status/case columns missing: ",cohort)
    ids<-inventory$record_id
    if(anyDuplicated(ids)||anyDuplicated(statuses$record_id)||
       !identical(ids,statuses$record_id))
      stop("Inventory/status IDs disagree: ",cohort)
    missing<-setdiff(ids,checkpoint_ids)
    if(length(missing))
      stop(cohort,": ",length(missing),
           " accessible full texts still have no validated checkpoint. ",
           "Examples: ",paste(head(missing,10L),collapse=", "),
           ". Rerun the extraction before finalising.")
    if(any(statuses$extraction_status!="validated"))
      stop(cohort,": status CSV still lists unresolved publications.")
    actual_counts<-vapply(records[match(ids,checkpoint_ids)],
                          function(z)length(z$cases),integer(1))
    if(anyNA(statuses$case_count)||
       !identical(as.integer(statuses$case_count),actual_counts))
      stop("Publication case counts disagree with JSONL checkpoint: ",cohort)
    expected_cases<-sum(actual_counts)
    if(nrow(cases)!=expected_cases)
      stop("Case CSV row count differs from validated JSONL: ",cohort)
    if(anyDuplicated(cases$case_id)||
       any(!cases$record_id%in%ids))
      stop("Duplicate/out-of-scope case IDs in ",cohort)
    expected_case_ids<-unlist(lapply(
      records[match(ids,checkpoint_ids)],
      function(z)if(length(z$cases))vapply(z$cases,
               function(c)as.character(c$case_id),character(1))
               else character()),use.names=FALSE)
    if(!setequal(cases$case_id,expected_case_ids))
      stop("Exported case IDs differ from validated checkpoint: ",cohort)
    cases$cohort<-cohort
    statuses$cohort<-cohort
    case_sets[[cohort]]<-cases
    status_sets[[cohort]]<-statuses
    provenance[[cohort]]<-list(
      w09=w09,selection_manifest=selection_file,
      selection_manifest_sha256=sha(selection_file),
      inventory=inventory_file,inventory_sha256=sha(inventory_file),
      extraction_manifest=extraction_manifest,
      extraction_manifest_sha256=sha(extraction_manifest),
      checkpoint=ledger_file,checkpoint_sha256=sha(ledger_file),
      case_export=case_file,case_export_sha256=sha(case_file),
      status_export=status_file,status_export_sha256=sha(status_file),
      accessible_publications=length(ids),
      validated_publications=length(ids),
      extracted_cases=nrow(cases))
  }
  if(!identical(names(case_sets[[1L]]),names(case_sets[[2L]]))||
     !identical(names(status_sets[[1L]]),names(status_sets[[2L]])))
    stop("First10 and remaining90 exports have incompatible columns.")
  all_cases<-do.call(rbind,case_sets)
  all_status<-do.call(rbind,status_sets)
  rownames(all_cases)<-NULL;rownames(all_status)<-NULL
  if(anyDuplicated(all_status$record_id)||
     anyDuplicated(all_cases$case_id))
    stop("Cohorts contain duplicate publication or case IDs.")
  if(!all(pilot_ids%in%all_status$record_id))
    stop("Not all approved pilot publications are present in final cohorts.")
  if(!all(all_status$extraction_status=="validated"))
    stop("Some publications remain unresolved.")
  if(nrow(all_cases)!=sum(all_status$case_count))
    stop("Combined case count is inconsistent.")
  # Write only after all checks pass. Existing final snapshots are immutable;
  # a changed run must be finalised to a new revision rather than overwritten.
  final_dir<-file.path(root,"final")
  dir.create(final_dir,recursive=TRUE,showWarnings=FALSE)
  cases_out<-file.path(final_dir,"OSTARA_all_case_data.csv")
  status_out<-file.path(final_dir,"OSTARA_all_publication_status.csv")
  summary_out<-file.path(final_dir,"OSTARA_extraction_summary.csv")
  manifest_out<-file.path(final_dir,"final_manifest.json")
  outfiles<-c(cases_out,status_out,summary_out,manifest_out)
  if(any(file.exists(outfiles)))
    stop("A final archive already exists. It is immutable; inspect ",
         manifest_out," rather than overwrite it.")
  write_excel_csv<-function(x,path) {
    con<-file(path,open="wb")
    on.exit(close(con),add=TRUE)
    writeBin(as.raw(c(0xEF,0xBB,0xBF)),con)
    utils::write.table(x,file=con,sep=",",row.names=FALSE,
                       col.names=TRUE,quote=TRUE,na="",qmethod="double")
  }
  summary<-data.frame(cohort=cohorts,
    accessible_publications=vapply(provenance,
      function(p)p$accessible_publications,integer(1)),
    validated_publications=vapply(provenance,
      function(p)p$validated_publications,integer(1)),
    extracted_cases=vapply(provenance,
      function(p)p$extracted_cases,integer(1)))
  summary<-rbind(summary,data.frame(cohort="TOTAL",
    accessible_publications=sum(summary$accessible_publications),
    validated_publications=sum(summary$validated_publications),
    extracted_cases=sum(summary$extracted_cases)))
  write_excel_csv(all_cases,cases_out)
  write_excel_csv(all_status,status_out)
  write_excel_csv(summary,summary_out)
  manifest<-list(
    schema="ostara-locked-case-data-final-v1",
    status="FINAL_ACCESSIBLE_FULLTEXT_EXTRACTION",
    finalised_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    project="Ostara_collective_action_map",
    model="gpt-5.6-luna",
    eligibility="W08 human-adjudicated specific_case or multiple_cases with readable W09 full text",
    scope="Accessible full texts only; inaccessible papers excluded and documented per cohort",
    approved_pilot=pilot_id,
    approved_pilot_manifest_sha256=sha(pilot_manifest_file),
    approved_pilot_sample_sha256=sha(pilot_ids_file),
    prompt_sha256=sha(prompt),
    schema_sha256=sha(schema),
    prompt_path=prompt,schema_path=schema,
    cohorts=provenance,
    accessible_publications=nrow(all_status),
    validated_publications=nrow(all_status),
    extracted_case_rows=nrow(all_cases),
    final_case_csv=cases_out,final_case_csv_sha256=sha(cases_out),
    final_status_csv=status_out,final_status_csv_sha256=sha(status_out),
    final_summary_csv=summary_out,final_summary_csv_sha256=sha(summary_out))
  jsonlite::write_json(manifest,manifest_out,pretty=TRUE,auto_unbox=TRUE)
  message("[OSTARA final] ",nrow(all_status)," validated publications; ",
          nrow(all_cases)," cases; pilot included exactly once.")
  message("[OSTARA final] Final manifest: ",manifest_out)
  invisible(list(cases=cases_out,status=status_out,summary=summary_out,
                 manifest=manifest_out,counts=summary))
}
