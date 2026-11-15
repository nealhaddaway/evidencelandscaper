#' Stage and optionally extract a ten-source OSTARA grey-literature pilot
#'
#' A separate W09-compatible namespace is created for identity-verified
#' grey-literature PDF/XML files. The locked pilot prompt/schema are reused,
#' without changing the published peer-reviewed case-data snapshot.
#' Metadata file must have file_name,title,source_url,access_date and
#' human_verified_identity columns. Never infers case-positive status from
#' filename or metadata: submitted full texts may yield zero cases.
#'
#' @param map_path OSTARA project root.
#' @param input_dir Directory with 10 source PDF/XML files and sources.csv.
#' @param batch_id Stable pilot identifier, reused to resume.
#' @param run_extraction FALSE stages/validates only; TRUE incurs API costs.
#' @return Paths and source/publication statuses, invisibly.
#' @export
run_ostara_grey_literature_pilot <- function(map_path,input_dir,
   batch_id="grey_pilot_10",run_extraction=FALSE) {
  if(!is.logical(run_extraction)||length(run_extraction)!=1L||
     is.na(run_extraction))stop("run_extraction must be TRUE or FALSE.")
  if(!is.character(batch_id)||length(batch_id)!=1L||
     is.na(batch_id)||!grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$",batch_id))
    stop("batch_id must contain only letters, digits, underscores and hyphens.")
  norm<-function(x)normalizePath(x,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  jl<-function(p){
    if(!file.exists(p))return(list())
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x){
    y<-as.character(unlist(x,use.names=FALSE))
    if(!length(y))"" else paste(y,collapse=" | ")
  }
  csv<-function(p)utils::read.csv(p,check.names=FALSE,
             stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
  excel_csv<-function(x,p){
    con<-file(p,open="wb");on.exit(close(con),add=TRUE)
    writeBin(as.raw(c(239,187,191)),con)
    utils::write.table(x,con,sep=",",row.names=FALSE,col.names=TRUE,
                       quote=TRUE,na="",qmethod="double")
  }
  map_path<-norm(map_path);input_dir<-norm(input_dir)
  metadata_path<-norm(file.path(input_dir,"sources.csv"))
  m<-csv(metadata_path)
  required<-c("file_name","title","source_url","access_date",
              "human_verified_identity")
  if(!all(required%in%names(m)))
    stop("sources.csv needs columns: ",paste(required,collapse=", "))
  if(nrow(m)!=10L)stop("Ten-source pilot requires exactly 10 metadata rows.")
  for(k in required)if(anyNA(m[[k]])||
     any(!nzchar(trimws(as.character(m[[k]])))))
       stop("Missing required metadata in column ",k)
  if(anyDuplicated(m$file_name))
    stop("Duplicate source filename in sources.csv.")
  if(any(!m$human_verified_identity%in%c("TRUE","true",TRUE)))
    stop("Every source requires human_verified_identity=TRUE; ",
         "check document identity before a paid extraction.")
  if(any(!grepl("^\\d{4}-\\d{2}-\\d{2}$",m$access_date)))
    stop("access_date must use ISO YYYY-MM-DD.")
  if(any(!grepl("^(https?://|doi:)",m$source_url,ignore.case=TRUE)))
    stop("Each source_url must be a URL or doi: identifier.")
  if(any(grepl("[/\\\\]",m$file_name))||
     any(!grepl("\\.(pdf|xml)$",m$file_name,ignore.case=TRUE)))
    stop("Provide plain .pdf/.xml filenames, no subfolders.")
  input_paths<-file.path(input_dir,m$file_name)
  if(any(!file.exists(input_paths)))
    stop("Missing input files: ",
         paste(m$file_name[!file.exists(input_paths)],collapse=", "))
  ext<-tolower(tools::file_ext(input_paths))
  hashes<-vapply(input_paths,sha,character(1))
  if(anyDuplicated(hashes))
    stop("Two input files have identical SHA256; resolve duplicate sources.")
  # IDs remain reproducible across reruns and distinct from journal IDs.
  ids<-paste0("grey-",substr(hashes,1L,20L))
  if(anyDuplicated(ids))stop("Source ID hash collision.")
  pilot<-norm(file.path(map_path,"reports","ostara_case_pilots",
                         "case_pilot_20_20261113"))
  prompt<-norm(file.path(pilot,"prompt.txt"))
  schema<-norm(file.path(pilot,"case_schema.csv"))
  pfile<-norm(file.path(pilot,"batch_manifest.json"))
  pmeta<-jsonlite::fromJSON(pfile,simplifyVector=FALSE)
  if(!identical(sha(prompt),pmeta$staged_prompt_sha256)||
     !identical(sha(schema),pmeta$schema_sha256))
    stop("Locked OSTARA pilot prompt/schema hashes do not match.")
  root<-file.path(map_path,"runs","W09","grey_literature",batch_id)
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  staging_manifest<-file.path(root,"grey_sources_manifest.json")
  qfile<-file.path(root,"retrieval_queue.csv")
  inventory<-data.frame(
    record_id=ids,file_name=m$file_name,title=m$title,
    source_url=m$source_url,access_date=m$access_date,
    human_verified_identity=rep(TRUE,10L),source_format=ext,
    source_sha256=hashes,original_file_bytes=as.numeric(file.info(input_paths)$size),
    stringsAsFactors=FALSE)
  manifest<-list(schema="ostara-grey-literature-batch-v1",
    batch_id=batch_id,source_class="grey_literature",
    case_positive_prior_classification="not_assumed",
    full_texts_provided=10L,
    locked_prompt_sha256=sha(prompt),locked_schema_sha256=sha(schema),
    approved_pilot_manifest_sha256=sha(pfile),
    source_records=lapply(seq_len(nrow(inventory)),function(i)
      as.list(inventory[i,,drop=FALSE])))
  if(file.exists(staging_manifest)){
    old<-jsonlite::fromJSON(staging_manifest,simplifyVector=FALSE)
    # If the first run established a source identity register, metadata
    # changes or replacement files must use a fresh batch ID.
    if(!identical(old,manifest))
      stop("Grey batch source register changed. Use a NEW batch_id ",
           "for changed metadata or documents.")
  } else {
    if(file.exists(qfile)||dir.exists(file.path(root,"case_data_extraction")))
      stop("Staging files exist without the source manifest; refusing overwrite.")
    jsonlite::write_json(manifest,staging_manifest,
                         auto_unbox=TRUE,pretty=TRUE)
  }
  dir.create(file.path(root,"pdfs"),showWarnings=FALSE)
  dir.create(file.path(root,"structured"),showWarnings=FALSE)
  for(i in seq_along(ids)){
    p<-file.path(root,if(ext[[i]]=="pdf")"pdfs" else "structured",
                 paste0(ids[[i]],".",ext[[i]]))
    if(file.exists(p)){
      if(!identical(sha(p),hashes[[i]]))
        stop("Staged file changed: ",p)
    } else {
      if(!file.copy(input_paths[[i]],p))
        stop("Cannot stage: ",input_paths[[i]])
    }
  }
  queue<-data.frame(record_id=ids,title=m$title,
                    doi=rep("",length(ids)),stringsAsFactors=FALSE)
  if(file.exists(qfile)){
    old<-csv(qfile)
    if(!identical(old,queue))
      stop("Staged W09 retrieval queue has changed.")
  } else utils::write.csv(queue,qfile,row.names=FALSE,na="")
  # Stage all documents first; unprocessable PDF or XML is logged and
  # never classified as 'no case'. Extraction requires 10 readable sources.
  readable<-logical(length(ids));errors<-character(length(ids))
  for(i in seq_along(ids)){
    outcome<-tryCatch(w09_text_payload(root,ids[[i]],max_pages=0L),
       error=function(e)e)
    if(inherits(outcome,"error"))errors[[i]]<-conditionMessage(outcome)
    else if(!identical(outcome$sha256,hashes[[i]]))
      errors[[i]]<-"Staged source SHA256 does not match original"
    else readable[[i]]<-TRUE
  }
  status_file<-file.path(root,"grey_source_readability.csv")
  source_status<-data.frame(inventory,
     readable_full_text=readable,
     readability_issue=errors,stringsAsFactors=FALSE)
  excel_csv(source_status,status_file)
  if(run_extraction) {
    if(!all(readable))
      stop("Not all ten sources are readable; fix sources and create ",
           "a new batch if files change. See ",status_file)
    if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
      stop("OPENAI_API_KEY missing. No paid requests were made.")
    run_w09_case_data_extraction(
      w09=root,prompt_file=prompt,schema_file=schema,
      record_ids=ids,max_records=0L,
      model="gpt-5.6-luna",max_pages=0L,revalidate_only=FALSE)
  }
  # Derive publication-level status directly from checkpoint, not guesses.
  xdir<-file.path(root,"case_data_extraction",paste0(
      "csv-",substr(sha(schema),1L,12L),"-prompt-",
      substr(sha(prompt),1L,12L),"-model-gpt-5.6-luna"))
  ledger_path<-file.path(xdir,"case_data.jsonl")
  records<-jl(ledger_path)
  rid<-if(length(records))
    vapply(records,function(z)scalar(z$record_id),character(1))
    else character()
  if(anyDuplicated(rid)||any(!rid%in%ids))
    stop("Grey extraction ledger contains invalid or duplicate IDs.")
  cases_n<-rep(NA_integer_,length(ids))
  for(i in which(ids%in%rid))
    cases_n[[i]]<-length(records[[match(ids[[i]],rid)]]$cases)
  status<-data.frame(record_id=ids,title=m$title,
     extraction_status=ifelse(!readable,"unreadable_full_text",
       ifelse(ids%in%rid,"validated","not_yet_validated")),
     case_count=cases_n,stringsAsFactors=FALSE)
  status_out<-file.path(root,"grey_publication_status.csv")
  excel_csv(status,status_out)
  cases_csv<-NULL
  if(length(records)){
    cases_csv<-export_w09_cases_wide_evidence(
      w09=root,extraction_dir=xdir,include_status=TRUE)
  }
  # Explicit outcome manifest may be updated as a batch resumes;
  # source identity manifest is immutable.
  outcome<-list(schema="ostara-grey-literature-outcome-v1",
    batch_id=batch_id,source_manifest=staging_manifest,
    source_manifest_sha256=sha(staging_manifest),
    source_count=10L,
    readable_fulltexts=sum(readable),
    unreadable_fulltexts=sum(!readable),
    validated_publications=sum(ids%in%rid),
    not_yet_validated=sum(readable&!ids%in%rid),
    extracted_cases=sum(cases_n,na.rm=TRUE),
    status_csv=status_out,status_csv_sha256=sha(status_out),
    ledger=if(file.exists(ledger_path))ledger_path else NULL,
    ledger_sha256=if(file.exists(ledger_path))sha(ledger_path) else NULL,
    cases_csv=cases_csv,
    cases_csv_sha256=if(!is.null(cases_csv))sha(cases_csv) else NULL,
    included_in_locked_journal_dataset=FALSE,
    updated_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  outcome_path<-file.path(root,"grey_extraction_outcome.json")
  jsonlite::write_json(outcome,outcome_path,auto_unbox=TRUE,pretty=TRUE)
  message("[OSTARA grey] ",sum(readable),"/10 readable; ",
    sum(ids%in%rid),"/10 validated; ",
    sum(cases_n,na.rm=TRUE)," cases. ",
    if(!run_extraction)"Dry-run only; zero API calls." else
    "Existing validated records checkpointed.")
  invisible(list(batch_dir=root,source_manifest=staging_manifest,
     source_inventory=status_file,status=status_out,
     outcome_manifest=outcome_path,cases_csv=cases_csv,
     readable=sum(readable),validated=sum(ids%in%rid),
     case_count=sum(cases_n,na.rm=TRUE)))
}
