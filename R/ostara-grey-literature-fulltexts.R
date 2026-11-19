#' Stage and optionally extract a folder of grey-lit PDF and HTML sources
#'
#' A separate W09-compatible namespace is created from PDF, XML, HTML, HTM and ASPX
#' files. The filename is used as the source label, with file hashes and
#' extraction provenance recorded automatically. No sources.csv is needed.
#' No case-positive classification is assumed; a document may yield zero cases.
#'
#' @param map_path OSTARA project root.
#' @param input_dir User-selected directory of PDF, XML, HTML, HTM or ASPX files.
#' @param batch_id Stable pilot label. Revised prompt/schema files automatically
#'   produce a separate hash-versioned extraction subdirectory.
#' @param run_extraction FALSE stages/validates only; TRUE incurs API costs.
#' @param prompt_file Required revised grey-literature case extraction TXT.
#' @param schema_file Required authoritative revised 41-field grey case-analysis CSV.
#' @return Paths and source/publication statuses, invisibly.
#' @export
run_ostara_grey_literature_fulltexts <- function(map_path,input_dir,
   batch_id="grey_fulltexts",run_extraction=FALSE,
   prompt_file=NULL,schema_file=NULL) {
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
  files<-sort(list.files(input_dir,pattern="\\.(pdf|xml|html|htm|aspx)$",
                         ignore.case=TRUE,full.names=FALSE))
  if(!length(files))stop("No supported files found in chosen folder.")
  if(anyDuplicated(files))stop("Duplicate source filenames.")
  input_paths<-file.path(input_dir,files)
  if(any(dir.exists(input_paths))||any(!file.exists(input_paths)))
    stop("Invalid input files in selected directory.")
  # Filenames are the ONLY source labels supplied by the operator. Bibliographic
  # metadata is deliberately not inferred or fabricated.
  labels<-tools::file_path_sans_ext(files)
  ext<-tolower(tools::file_ext(input_paths))
  hashes<-unname(vapply(input_paths,sha,character(1)))
  # Treat byte-identical files as ONE source, while retaining a traceable
  # record of every supplied filename. This happens before any paid work.
  all_files<-files
  all_hashes<-hashes
  first_indices<-match(all_hashes,all_hashes)
  keep<-!duplicated(all_hashes)
  duplicates<-data.frame(
    supplied_filename=all_files,
    source_sha256=all_hashes,
    selected_filename=all_files[first_indices],
    selected_for_extraction=keep,
    stringsAsFactors=FALSE)
  files<-files[keep]
  input_paths<-input_paths[keep]
  labels<-labels[keep]
  ext<-ext[keep]
  hashes<-hashes[keep]
  # IDs remain reproducible across reruns and distinct from journal IDs.
  ids<-paste0("grey-",substr(hashes,1L,20L))
  if(anyDuplicated(ids))stop("Source ID hash collision.")
  pilot<-norm(file.path(map_path,"reports","ostara_case_pilots",
                         "case_pilot_20_20261113"))
  pfile<-norm(file.path(pilot,"batch_manifest.json"))
  pmeta<-jsonlite::fromJSON(pfile,simplifyVector=FALSE)
  original_prompt<-norm(file.path(pilot,"prompt.txt"))
  original_schema<-norm(file.path(pilot,"case_schema.csv"))
  if(!identical(sha(original_prompt),pmeta$staged_prompt_sha256)||
     !identical(sha(original_schema),pmeta$schema_sha256))
    stop("Approved OSTARA pilot prompt/schema hashes do not match.")
  if(is.null(prompt_file)||is.null(schema_file))
    stop("Supply the exact revised prompt TXT and authoritative 41-field case-analysis CSV.")
  prompt<-norm(prompt_file)
  schema<-norm(schema_file)
  if(!is.null(schema_file)){
    grey_spec<-csv(schema)
    if(nrow(grey_spec)!=41L||
       !identical(trimws(grey_spec$Question[[1L]]),"Type of source")||
       !grepl("^Was this source/publication written, funded or commissioned",
              trimws(grey_spec$Question[[41L]])))
      stop("Expected the client's exact revised 41-field grey-literature schema.")
    if(!identical(basename(batch_id),"grey_pilot_10_revised"))
      message("[OSTARA grey] Using custom prompt/schema for batch ",batch_id,
              "; this MUST be a distinct batch ID from previous inputs.")
  }
  # Every exact prompt/schema version receives an independent W09
  # namespace. Same ten source files + changed client inputs are always
  # re-extracted, while rerunning identical inputs resumes their checkpoint.
  # The operator keeps using the same batch_id in their R script.
  requested_batch_id<-batch_id
  if(!is.null(prompt_file)) {
    version_id<-paste0("prompt-",substr(sha(prompt),1L,12L),
                       "-schema-",substr(sha(schema),1L,12L))
    batch_id<-paste0(batch_id,"__",version_id)
  }
  root<-file.path(map_path,"runs","W09","grey_literature",batch_id)
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  staging_manifest<-file.path(root,"grey_sources_manifest.json")
  qfile<-file.path(root,"retrieval_queue.csv")
  # Record omitted byte-identical copies for reproducibility.
  duplicate_csv<-file.path(root,"grey_duplicate_files.csv")
  excel_csv(duplicates,duplicate_csv)
  if(any(!keep))message("[OSTARA grey] ",sum(!keep),
    " byte-identical file(s) omitted; see ",duplicate_csv)

  inventory<-data.frame(
    record_id=ids,file_name=files,title=labels,source_format=ext,
    source_sha256=hashes,original_file_bytes=as.numeric(file.info(input_paths)$size),
    stringsAsFactors=FALSE)
  manifest<-list(schema="ostara-grey-literature-filename-only-batch-v2",
    batch_id=batch_id,requested_batch_id=requested_batch_id,
    source_class="grey_literature",
    case_positive_prior_classification="not_assumed",
    metadata_policy="Filename label only; bibliographic metadata unverified",
    input_contract=if(is.null(schema_file))"original_pilot" else "client_revised_grey_41_fields",
    full_texts_provided=length(ids),
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
  # Preserve raw files and convert rendered HTML/ASPX into XML text blocks.
  dir.create(file.path(root,"originals"),showWarnings=FALSE)
  preparation_issues<-rep("",length(ids))
  for(i in seq_along(ids)){
    original<-file.path(root,"originals",paste0(ids[[i]],".",ext[[i]]))
    if(file.exists(original)) {
      if(!identical(sha(original),hashes[[i]]))
        stop("Original archived source changed: ",original)
    } else if(!file.copy(input_paths[[i]],original,overwrite=FALSE))
      stop("Cannot archive source: ",input_paths[[i]])
    is_html<-ext[[i]]%in%c("html","htm","aspx")
    staged_ext<-if(is_html)"xml" else ext[[i]]
    staged<-file.path(root,if(staged_ext=="pdf")"pdfs" else "structured",
                      paste0(ids[[i]],".",staged_ext))
    if(!is_html) {
      if(file.exists(staged)) {
        if(!identical(sha(staged),hashes[[i]]))
          stop("Staged source changed: ",staged)
      } else if(!file.copy(original,staged,overwrite=FALSE))
        stop("Failed staging: ",original)
      next
    }
    if(file.exists(staged))next
    issue<-tryCatch({
      if(!requireNamespace("xml2",quietly=TRUE))
        stop("Install package xml2 for HTML and ASPX.")
      page<-paste(readLines(original,warn=FALSE,encoding="UTF-8"),
                  collapse="\n")
      if(grepl("<%@|<asp:|runat[[:space:]]*=",page,ignore.case=TRUE))
        stop("Unrendered server-side ASPX source; not usable full text")
      doc<-xml2::read_html(original)
      xml2::xml_remove(xml2::xml_find_all(doc,"//script|//style|//nav|//footer"))
      paras<-xml2::xml_find_all(doc,
        "//body//*[self::p or self::h1 or self::h2 or self::h3 or self::h4 or self::li or self::td or self::blockquote]")
      words<-trimws(xml2::xml_text(paras))
      words<-words[nzchar(words)]
      if(!length(words)||sum(nchar(words))<500L)
        stop("No sufficient rendered HTML text in file")
      new<-xml2::xml_new_root("document")
      for(word in words)xml2::xml_add_child(new,"para",word)
      xml2::write_xml(new,staged)
      ""
    },error=function(e)conditionMessage(e))
    if(nzchar(issue)) {
      preparation_issues[[i]]<-issue
      if(file.exists(staged))unlink(staged)
    }
  }
  queue<-data.frame(record_id=ids,title=labels,
                    doi=rep("",length(ids)),stringsAsFactors=FALSE)
  if(file.exists(qfile)){
    # A CSV round-trip can convert an all-empty DOI column to logical NA
    # or otherwise infer column types. Compare the recorded string values,
    # not R's inferred data-frame column classes, and never rewrite a queue.
    old<-utils::read.csv(qfile,check.names=FALSE,stringsAsFactors=FALSE,
        colClasses="character",na.strings="NA",fileEncoding="UTF-8-BOM")
    if(!identical(names(old),names(queue))||
       nrow(old)!=nrow(queue))
      stop("Staged W09 retrieval queue columns/row count changed.")
    # Empty DOI is the only allowed missing value: no DOI was supplied for
    # filename-only source records. Preserve every record_id and filename.
    if(anyNA(old$record_id)||anyNA(old$title)||
       !identical(old$record_id,as.character(queue$record_id))||
       !identical(old$title,as.character(queue$title))||
       !identical(replace(old$doi,is.na(old$doi),""),queue$doi))
      stop("Staged W09 retrieval queue content changed. ",
           "No staged inputs were overwritten.")
  } else utils::write.csv(queue,qfile,row.names=FALSE,na="")
  # Stage documents; unreadable files are logged, never classified negative.
  readable<-logical(length(ids));errors<-preparation_issues
  for(i in seq_along(ids)){
    if(nzchar(errors[[i]]))next
    outcome<-tryCatch(w09_text_payload(root,ids[[i]],max_pages=0L),
       error=function(e)e)
    if(inherits(outcome,"error"))errors[[i]]<-conditionMessage(outcome)
    else if(!ext[[i]]%in%c("html","htm","aspx")&&
            !identical(outcome$sha256,hashes[[i]]))
      errors[[i]]<-"Staged source SHA256 does not match original"
    else readable[[i]]<-TRUE
  }
  status_file<-file.path(root,"grey_source_readability.csv")
  source_status<-data.frame(inventory,
     readable_full_text=readable,
     readability_issue=errors,stringsAsFactors=FALSE)
  excel_csv(source_status,status_file)
  if(run_extraction) {
    if(!any(readable))stop("No readable sources: ",status_file)
    if(any(!readable))message("Skipping ",sum(!readable)," unreadable files: ",status_file)
    if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
      stop("OPENAI_API_KEY missing. No paid requests were made.")
    run_w09_case_data_extraction(
      w09=root,prompt_file=prompt,schema_file=schema,
      record_ids=ids[readable],max_records=0L,
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
    source_count=length(ids),
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
  message("[OSTARA grey] ",sum(readable),"/",length(ids)," readable; ",
    sum(ids%in%rid),"/",length(ids)," validated; ",
    sum(cases_n,na.rm=TRUE)," cases. ",
    if(!run_extraction)"Dry-run only; zero API calls." else
    "Existing validated records checkpointed.")
  invisible(list(batch_dir=root,source_manifest=staging_manifest,
     source_inventory=status_file,status=status_out,
     outcome_manifest=outcome_path,cases_csv=cases_csv,
     duplicate_report=duplicate_csv,duplicates_omitted=sum(!keep),
     readable=sum(readable),validated=sum(ids%in%rid),
     case_count=sum(cases_n,na.rm=TRUE)))
}
