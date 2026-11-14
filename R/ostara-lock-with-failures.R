#' Finalise OSTARA case data with three documented extraction failures
#'
#' Creates a NEW immutable final snapshot from cohort exports, which already
#' contain the approved 20-paper pilot. Requires precisely the three
#' authorised remaining90 failures, and no other unresolved accessible texts.
#' Existing combined CSVs and all JSONL checkpoints are never changed.
#' No OpenAI requests are made.
#' @param map_path OSTARA project root.
#' @return Invisible list of snapshot paths and counts.
#' @export
lock_ostara_case_data_with_failures <- function(map_path) {
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  csv<-function(p)utils::read.csv(p,stringsAsFactors=FALSE,
                   check.names=FALSE,fileEncoding="UTF-8-BOM")
  jl<-function(p){
    if(!file.exists(p))return(list())
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  ident<-function(x)as.character(x$record_id)
  map_path<-norm(map_path)
  root<-file.path(map_path,"reports","ostara_case_fulltext_locked")
  pilot<-norm(file.path(map_path,"reports","ostara_case_pilots",
                         "case_pilot_20_20261113"))
  pilot_mf<-norm(file.path(pilot,"batch_manifest.json"))
  pmeta<-jsonlite::fromJSON(pilot_mf,simplifyVector=FALSE)
  prompt<-norm(file.path(pilot,"prompt.txt"))
  schema<-norm(file.path(pilot,"case_schema.csv"))
  sample_file<-norm(file.path(pilot,"sampled_publications.csv"))
  if(!identical(sha(prompt),pmeta$staged_prompt_sha256)||
     !identical(sha(schema),pmeta$schema_sha256)||
     !identical(sha(sample_file),pmeta$sample_sha256))
    stop("Approved pilot inputs or sample have changed.")
  pilot_ids<-csv(sample_file)$record_id
  if(length(pilot_ids)!=20L||anyDuplicated(pilot_ids))
    stop("Pilot sample must have 20 unique record IDs.")
  expected_failures<-c("work-4cfa0cacd9a36144",
     "work-6a77cf0d62cba451","work-9c31363ab815eff1")
  # The nine validated publications from the final retry, recorded explicitly
  # to document the change from 483 to 492 validated remaining90 records.
  latest_retry_ids<-c(
    "work-2bf8eac51f20836e","work-472b06139c558eb7",
    "work-4c9a43b0d69013c0","work-5a9d05d79986ed5c",
    "work-714ec230989ef6a1","work-81e7bf81768a5cd2",
    "work-834184f3cb207052","work-aafd19f3fb6d25bd",
    "work-b99a82efc107b5db")
  cohorts<-c("remaining90","first10")
  data_cases<-list();data_status<-list();prov<-list()
  for(cohort in cohorts) {
    d<-norm(file.path(root,cohort))
    mf_path<-norm(file.path(d,"selection_manifest.json"))
    meta<-jsonlite::fromJSON(mf_path,simplifyVector=FALSE)
    inv_file<-norm(file.path(d,"selected_accessible_fulltexts.csv"))
    cases_file<-norm(file.path(d,"all_cases_wide.csv"))
    status_file<-norm(file.path(d,"all_publications_status.csv"))
    if(!identical(meta$cohort,cohort)||
       !identical(meta$prompt_sha256,sha(prompt))||
       !identical(meta$schema_sha256,sha(schema))||
       !identical(meta$pilot_manifest_sha256,sha(pilot_mf))||
       !identical(meta$inventory_sha256,sha(inv_file)))
      stop("Cohort selection provenance mismatch: ",cohort)
    w09<-norm(meta$w09)
    xdir<-norm(file.path(w09,"case_data_extraction",paste0(
      "csv-",substr(sha(schema),1L,12L),
      "-prompt-",substr(sha(prompt),1L,12L),
      "-model-gpt-5.6-luna")))
    xf<-norm(file.path(xdir,"manifest.json"))
    xm<-jsonlite::fromJSON(xf,simplifyVector=FALSE)
    if(!identical(xm$prompt_sha256,sha(prompt))||
       !identical(xm$csv_sha256,sha(schema))||
       !identical(xm$model,"gpt-5.6-luna")||
       !identical(xm$queue_sha256,
         sha(file.path(w09,"retrieval_queue.csv"))))
      stop("W09 extraction provenance mismatch: ",cohort)
    ledger_path<-norm(file.path(xdir,"case_data.jsonl"))
    recs<-jl(ledger_path)
    rid<-vapply(recs,ident,character(1))
    if(anyDuplicated(rid))stop("Duplicate record IDs in W09 checkpoint: ",cohort)
    inv<-csv(inv_file);st<-csv(status_file);cs<-csv(cases_file)
    if(!all(c("record_id","source_sha256")%in%names(inv))||
       !all(c("record_id","extraction_status","case_count")%in%names(st))||
       !all(c("record_id","case_id")%in%names(cs)))
      stop("Missing required inventory, status or cases columns: ",cohort)
    ids<-inv$record_id
    if(anyDuplicated(ids)||anyDuplicated(st$record_id)||
       !identical(ids,st$record_id))
      stop("Inventory/status mismatch: ",cohort)
    unresolved<-setdiff(ids,rid)
    desired<-if(identical(cohort,"remaining90"))
      expected_failures else character()
    if(!setequal(unresolved,desired))
      stop(cohort,": unresolved set differs from the THREE agreed failures. ",
           "Found: ",paste(unresolved,collapse=", "))
    if(!identical(st$extraction_status,
       ifelse(ids%in%unresolved,"unresolved","validated")))
      stop("Stale or inconsistent cohort publication statuses: ",cohort)
    valid_ids<-setdiff(ids,unresolved)
    if(identical(cohort,"remaining90")&&
       !all(latest_retry_ids%in%valid_ids))
      stop("One or more of the nine newly validated retry IDs is missing.")
    counts<-vapply(recs[match(valid_ids,rid)],
                    function(z)length(z$cases),integer(1))
    observed<-st$case_count[match(valid_ids,ids)]
    if(anyNA(observed)||!identical(as.integer(observed),counts)||
       any(!is.na(st$case_count[match(unresolved,ids)])))
      stop("Cohort publication case counts differ from checkpoint: ",cohort)
    expected_cases<-unlist(lapply(recs[match(valid_ids,rid)],
       function(z)if(length(z$cases))
         vapply(z$cases,function(c)as.character(c$case_id),character(1))
         else character()),use.names=FALSE)
    if(nrow(cs)!=length(expected_cases)||anyDuplicated(cs$case_id)||
       !setequal(cs$case_id,expected_cases)||
       any(!cs$record_id%in%valid_ids))
      stop("Cohort case CSV differs from validated JSONL records: ",cohort)
    # Require a recorded three-attempt failure rather than merely 'unresolved'.
    failures_file<-file.path(xdir,"batch_failures.jsonl")
    failures<-jl(failures_file)
    logged<-vapply(failures,ident,character(1))
    if(length(unresolved)&&!all(unresolved%in%logged))
      stop("One or more agreed failures lack a W09 batch failure log.")
    st$extraction_status[st$record_id%in%unresolved]<-"failed_extraction"
    cs$cohort<-cohort;st$cohort<-cohort
    data_cases[[cohort]]<-cs
    data_status[[cohort]]<-st
    inaccessible_file<-file.path(d,"inaccessible_fulltexts.csv")
    prov[[cohort]]<-list(
       w09=w09,selection_manifest=mf_path,
       selection_manifest_sha256=sha(mf_path),
       inventory=inv_file,inventory_sha256=sha(inv_file),
       inaccessible_fulltexts=inaccessible_file,
       inaccessible_fulltexts_sha256=if(file.exists(inaccessible_file))
         sha(inaccessible_file) else NULL,
       extraction_manifest=xf,extraction_manifest_sha256=sha(xf),
       extraction_checkpoint=ledger_path,
       extraction_checkpoint_sha256=sha(ledger_path),
       batch_failures_file=failures_file,
       batch_failures_sha256=if(file.exists(failures_file))
         sha(failures_file) else NULL,
       case_export=cases_file,case_export_sha256=sha(cases_file),
       status_export=status_file,status_export_sha256=sha(status_file),
       accessible_fulltexts=length(ids),
       validated_publications=length(valid_ids),
       failed_extractions=as.list(unresolved),
       extracted_cases=nrow(cs))
  }
  if(!identical(names(data_cases[[1L]]),names(data_cases[[2L]]))||
     !identical(names(data_status[[1L]]),names(data_status[[2L]])))
    stop("Cohort-wide exports have incompatible schemas.")
  combined<-do.call(rbind,data_cases)
  statuses<-do.call(rbind,data_status)
  rownames(combined)<-NULL;rownames(statuses)<-NULL
  if(anyDuplicated(combined$case_id)||
     anyDuplicated(statuses$record_id)||
     !all(pilot_ids%in%statuses$record_id)||
     !all(statuses$extraction_status%in%c("validated","failed_extraction"))||
     nrow(combined)!=sum(statuses$case_count,na.rm=TRUE))
    stop("Combined case IDs, pilot inclusion or counts are invalid.")
  summary<-data.frame(
    cohort=cohorts,
    accessible_fulltexts=vapply(prov,function(x)x$accessible_fulltexts,integer(1)),
    validated_publications=vapply(prov,function(x)x$validated_publications,integer(1)),
    failed_extractions=vapply(prov,function(x)length(x$failed_extractions),integer(1)),
    extracted_cases=vapply(prov,function(x)x$extracted_cases,integer(1)))
  summary<-rbind(summary,data.frame(
    cohort="TOTAL",accessible_fulltexts=sum(summary$accessible_fulltexts),
    validated_publications=sum(summary$validated_publications),
    failed_extractions=sum(summary$failed_extractions),
    extracted_cases=sum(summary$extracted_cases)))
  failed<-data.frame(record_id=expected_failures,cohort="remaining90",
    status="failed_extraction",
    reason="Model output failed validation after repeated attempts; no validated case extraction",
    stringsAsFactors=FALSE)
  # An immutable final snapshot is created in a dedicated directory, leaving
  # previously published combined CSVs untouched for audit comparison.
  outdir<-file.path(root,"final_with_3_failures")
  files<-file.path(outdir,c("OSTARA_all_case_data.csv",
     "OSTARA_all_publication_status.csv","OSTARA_extraction_summary.csv",
     "OSTARA_failed_extractions.csv","final_manifest.json"))
  if(any(file.exists(files))||dir.exists(outdir))
    stop("Final snapshot directory already exists; refusing to overwrite: ",outdir)
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  write_excel_csv<-function(x,path){
    con<-file(path,open="wb")
    on.exit(close(con),add=TRUE)
    writeBin(as.raw(c(0xEF,0xBB,0xBF)),con)
    utils::write.table(x,file=con,sep=",",row.names=FALSE,
      col.names=TRUE,quote=TRUE,na="",qmethod="double")
  }
  write_excel_csv(combined,files[[1L]])
  write_excel_csv(statuses,files[[2L]])
  write_excel_csv(summary,files[[3L]])
  write_excel_csv(failed,files[[4L]])
  manifest<-list(schema="ostara-final-case-data-with-documented-failures-v1",
    status="FINAL_WITH_3_DOCUMENTED_EXTRACTION_FAILURES",
    finalised_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
    scope="Human-classified case-positive publications with accessible W09 full text",
    model="gpt-5.6-luna",approved_pilot="case_pilot_20_20261113",
    approved_pilot_manifest=pilot_mf,
    approved_pilot_manifest_sha256=sha(pilot_mf),
    approved_pilot_sample_sha256=sha(sample_file),
    locked_prompt=prompt,locked_prompt_sha256=sha(prompt),
    locked_schema=schema,locked_schema_sha256=sha(schema),
    cohorts=prov,
    total_accessible_fulltexts=nrow(statuses),
    total_validated_publications=sum(statuses$extraction_status=="validated"),
    total_failed_extractions=sum(statuses$extraction_status=="failed_extraction"),
    failed_record_ids=as.list(expected_failures),
    total_extracted_cases=nrow(combined),
    last_retry_new_validations=9L,
    last_retry_validated_record_ids=as.list(latest_retry_ids),
    remaining90_validated_before_last_retry=483L,
    remaining90_validated_after_last_retry=492L,
    outputs=list(
      cases=list(path=files[[1L]],sha256=sha(files[[1L]])),
      publication_status=list(path=files[[2L]],sha256=sha(files[[2L]])),
      summary=list(path=files[[3L]],sha256=sha(files[[3L]])),
      failed_extractions=list(path=files[[4L]],sha256=sha(files[[4L]]))),
    reporting_note="Failed extractions remain missing; no fabricated no-case classifications. Validated checkpoint records include prior 20-paper pilot exactly once. Evidence-review flags require separate QA.")
  jsonlite::write_json(manifest,files[[5L]],auto_unbox=TRUE,pretty=TRUE)
  message("[OSTARA final] ",nrow(statuses)," accessible full texts; ",
     sum(statuses$extraction_status=="validated")," validated; ",
     length(expected_failures)," documented failures; ",
     nrow(combined)," cases.")
  message("[OSTARA final] Snapshot: ",outdir)
  invisible(list(cases=files[[1L]],status=files[[2L]],summary=files[[3L]],
    failures=files[[4L]],manifest=files[[5L]],counts=summary))
}
