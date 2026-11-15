#' Stage and optionally extract a ten-source OSTARA grey-literature pilot
#'
#' A separate W09-compatible namespace is created from exactly ten PDF/XML
#' files. The filename is used as the source label, with file hashes and
#' extraction provenance recorded automatically. No sources.csv is needed.
#' No case-positive classification is assumed; a document may yield zero cases.
#'
#' @param map_path OSTARA project root.
#' @param input_dir Directory containing exactly 10 PDF/XML files.
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
  files<-sort(list.files(input_dir,pattern="\\.(pdf|xml)$",
                         ignore.case=TRUE,full.names=FALSE))
  if(length(files)!=10L)
    stop("Expected exactly 10 PDF/XML documents in input folder; found ",
         length(files),".")
  if(anyDuplicated(files))stop("Duplicate source filenames.")
  input_paths<-file.path(input_dir,files)
  if(any(dir.exists(input_paths))||any(!file.exists(input_paths)))
    stop("All ten PDF/XML inputs must be files in the input directory.")
  # Filenames are the ONLY source labels supplied by the operator. Bibliographic
  # metadata is deliberately not inferred or fabricated.
  labels<-tools::file_path_sans_ext(files)
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
    record_id=ids,file_name=files,title=labels,source_format=ext,
    source_sha256=hashes,original_file_bytes=as.numeric(file.info(input_paths)$size),
    stringsAsFactors=FALSE)
  manifest<-list(schema="ostara-grey-literature-filename-only-batch-v2",
    batch_id=batch_id,source_class="grey_literature",
    case_positive_prior_classification="not_assumed",
    metadata_policy="Filename label only; bibliographic metadata unverified",
    full_texts_provided=10L,
    locked_prompt_sha256=sha(prompt),locked_schema_sha256=sha(schema),
    approved_pilot_manifest_sha256=sha(pfile),
    source_records=lapply(seq_len(nrow(inventory)),function(i)
      as.list(inventory[i,,drop=FALSE])))
  if(file.exists(staging_manifest)){
    old<-jsonlite::fromJSON(staging_manifest,simplifyVector=FALSE)
    # If the first run established a source identity register, metadata
    # changes or replacement files must use a fresh batch ID.
    as_json<-function(z)as.character(jsonlite::toJSON(z,
                              auto_unbox=TRUE,null="null",digits=NA))
    if(!identical(as_json(old),as_json(manifest)))
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
  queue<-data.frame(record_id=ids,title=labels,
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
  status<-data.frame(record_id=ids,file_name=files,title=labels,
     extraction_status=ifelse(!readable,"unreadable_full_text",
       ifelse(ids%in%rid,"validated","not_yet_validated")),
     case_count=cases_n,stringsAsFactors=FALSE)
  status_out<-file.path(root,"grey_publication_status.csv")
  excel_csv(status,status_out)
  cases_csv<-NULL
  if(length(records)){
    spec<-csv(schema)
    spec<-spec[!grepl("mechanisms of the collective action",
              spec$Question,ignore.case=TRUE),,drop=FALSE]
    names_out<-unlist(lapply(seq_len(nrow(spec)),function(i){
      nm<-paste0(sprintf("%02d",i),". ",spec$Question[[i]])
      c(nm,paste0(nm," [evidence]"),
        paste0(nm," [reporting status]"))
    }),use.names=FALSE)
    names_out<-make.unique(names_out)
    base<-c("record_id","case_id","assessment_reason",
            "case_boundary_uncertainty")
    rows<-list()
    for(rec in records)for(onecase in rec$cases){
      fixed<-c(record_id=scalar(rec$record_id),
        case_id=scalar(onecase$case_id),
        assessment_reason=scalar(onecase$assessment_reason),
        case_boundary_uncertainty=scalar(onecase$case_boundary_uncertainty))
      values<-character()
      for(i in seq_len(nrow(spec))) {
        f<-onecase$fields[[sprintf("field_%03d",i)]]
        if(is.null(f))stop("Validated grey case missing field ",i)
        v<-if(!is.null(f$values))unlist(f$values,use.names=FALSE)
           else unlist(f$value,use.names=FALSE)
        is_factors<-grepl("enabler|barrier",spec$Question[[i]],
                           ignore.case=TRUE)
        answer<-if(!length(v))"" else if(is_factors)
          paste0("• ",paste(v,collapse="\n• ")) else paste(v,collapse="\n")
        ev<-if(length(f$evidence))
          vapply(f$evidence,function(e)
            paste0(scalar(e$anchor),": ",scalar(e$quote)),character(1))
          else character()
        values<-c(values,answer,paste(ev,collapse="\n"),
                  scalar(f$reporting_status))
      }
      names(values)<-names_out
      rows[[length(rows)+1L]]<-c(fixed,values)
    }
    all_names<-c(base,names_out)
    if(length(rows)){
      wide<-as.data.frame(do.call(rbind,rows),check.names=FALSE,
                          stringsAsFactors=FALSE)
      names(wide)<-all_names
    }else {
      wide<-as.data.frame(matrix(character(),nrow=0L,
                         ncol=length(all_names)),check.names=FALSE)
      names(wide)<-all_names
    }
    cases_csv<-file.path(root,"grey_case_data_wide.csv")
    excel_csv(wide,cases_csv)
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
