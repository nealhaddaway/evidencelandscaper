#' Run a new, resume-safe OSTARA case-positive full-text pilot
#'
#' Selects only human-classified specific_case/multiple_cases records from an
#' explicit W08 classification CSV that also occur in the supplied W09 queue.
#' A permanent batch register is written before paid extraction. All previously
#' attempted/extracted W09 IDs and saved pilot samples are excluded at first
#' selection. Existing global extraction checkpoints never count toward a new
#' batch; this batch's own checkpoints are reused on resume.
#'
#' @param w09 Exact W09 full-text retrieval directory.
#' @param classifications_csv Authoritative W08 CSV with record_id and
#'   final_case_status columns.
#' @param prompt_file TXT extraction prompt.
#' @param schema_file CSV case-analysis schema.
#' @param batch_id Unique batch identifier, reused to resume same 20 records.
#' @param n Number of new publications, default 20.
#' @param seed Random sampling seed.
#' @param map_path OSTARA project root; defaults to the parent of runs/W09.
#' @return Invisibly, a list of output paths and completion counts.
#' @export
run_ostara_case_pilot_batch <- function(w09,classifications_csv,prompt_file,
                                        schema_file,batch_id,n=20L,seed=20261113L,
                                        map_path=NULL) {
  norm<-function(x)normalizePath(x,winslash="/",mustWork=TRUE)
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    if(!file.exists(p))return(list())
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  chr<-function(x,sep=" | ") {
    if(is.null(x)||!length(x))return("")
    y<-as.character(unlist(x,use.names=FALSE))
    y<-y[!is.na(y)]
    if(!length(y))"" else paste(y,collapse=sep)
  }
  ids_from<-function(path) {
    z<-readjl(path)
    if(!length(z))return(character())
    vapply(z,function(x)chr(x$record_id),character(1))
  }
  if(length(batch_id)!=1L||is.na(batch_id)||
     !grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$",batch_id))
    stop("batch_id must contain only letters, digits, underscores and hyphens.")
  if(length(n)!=1L||is.na(n)||n!=as.integer(n)||n<1L)
    stop("n must be a positive integer.")
  w09<-norm(w09)
  classifications_csv<-norm(classifications_csv)
  prompt_file<-norm(prompt_file);schema_file<-norm(schema_file)
  if(is.null(map_path))map_path<-dirname(dirname(dirname(w09)))
  map_path<-norm(map_path)
  qfile<-file.path(w09,"retrieval_queue.csv")
  q<-utils::read.csv(qfile,stringsAsFactors=FALSE,check.names=FALSE,
                     fileEncoding="UTF-8-BOM")
  if(!all(c("record_id","title","doi")%in%names(q))||
     anyNA(q$record_id)||anyDuplicated(q$record_id))
    stop("Invalid W09 retrieval queue.")
  class<-utils::read.csv(classifications_csv,stringsAsFactors=FALSE,
                         check.names=FALSE,fileEncoding="UTF-8-BOM")
  if(!all(c("record_id","final_case_status")%in%names(class))||
     anyNA(class$record_id)||anyDuplicated(class$record_id))
    stop("Classification CSV must have unique record_id and final_case_status.")
  if(anyNA(class$final_case_status)||
     any(!class$final_case_status%in%c(
       "specific_case","multiple_cases","no_identifiable_case",
       "unclear","unvalidated_W08")))
    stop("Unexpected W08 final_case_status; use the authoritative classification CSV.")
  eligible<-intersect(
    class$record_id[class$final_case_status%in%c("specific_case","multiple_cases")],
    q$record_id
  )
  if(!length(eligible))stop("No confirmed case-positive IDs overlap the W09 queue.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY required for extraction.")
  spec<-utils::read.csv(schema_file,stringsAsFactors=FALSE,check.names=FALSE,
                         fileEncoding="UTF-8-BOM")
  required<-c("Question","Definition","Topics of interest",
              "Answer_form","Response_compulsory")
  if(!all(required%in%names(spec)))
    stop("Case schema missing: ",paste(setdiff(required,names(spec)),collapse=", "))
  spec<-spec[!grepl("mechanisms of the collective action",spec$Question,
                     ignore.case=TRUE),,drop=FALSE]
  if(!nrow(spec)||anyDuplicated(spec$Question))stop("Invalid case schema questions.")
  dir<-file.path(map_path,"reports","ostara_case_pilots",batch_id)
  sample_file<-file.path(dir,"sampled_publications.csv")
  manifest_file<-file.path(dir,"batch_manifest.json")
  dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  # Stage the exact selected files. Correct the outdated fixed-choice example
  # only in an auditable archived prompt; the original is not changed.
  staged_prompt<-file.path(dir,"prompt.txt")
  staged_schema<-file.path(dir,"case_schema.csv")
  source_prompt_sha<-sha(prompt_file);source_schema_sha<-sha(schema_file)
  prompt_lines<-readLines(prompt_file,warn=FALSE,encoding="UTF-8")
  prompt_lines<-gsub("use systemic or other, not systemic",
                     "use systemic, not systemic or other",
                     prompt_lines,fixed=TRUE)
  if(!any(nzchar(trimws(prompt_lines))))stop("Empty extraction prompt.")
  if(file.exists(staged_prompt)) {
    saved<-readLines(staged_prompt,warn=FALSE,encoding="UTF-8")
    if(!identical(saved,prompt_lines))stop("Archived prompt differs for this batch.")
  } else writeLines(prompt_lines,staged_prompt,useBytes=TRUE)
  if(file.exists(staged_schema)) {
    if(!identical(sha(staged_schema),source_schema_sha))
      stop("Archived schema differs for this batch.")
  } else if(!file.copy(schema_file,staged_schema))
    stop("Cannot archive case schema.")
  if(!file.exists(sample_file)) {
    # Exclude all IDs touched in ANY W09 extraction in this project.
    roots<-list.dirs(file.path(map_path,"runs","W09"),
                     recursive=FALSE,full.names=TRUE)
    previous<-character()
    for(root in roots) {
      extraction_base<-file.path(root,"case_data_extraction")
      if(!dir.exists(extraction_base))next
      runs<-list.dirs(extraction_base,recursive=FALSE,full.names=TRUE)
      for(run in runs)
        for(file in c("case_data.jsonl","invalid_responses.jsonl",
                      "source_documents.jsonl","batch_failures.jsonl",
                      "unprocessed_fulltexts.jsonl"))
          previous<-c(previous,ids_from(file.path(run,file)))
    }
    # Also exclude explicitly saved samples from earlier pilots, regardless
    # of whether those IDs reached the API extraction stage.
    samples<-list.files(file.path(map_path,"reports"),
                        pattern="^sampled_publications\\.csv$",
                        recursive=TRUE,full.names=TRUE)
    samples<-setdiff(normalizePath(samples,winslash="/",mustWork=FALSE),
                     normalizePath(sample_file,winslash="/",mustWork=FALSE))
    for(file in samples) {
      x<-tryCatch(utils::read.csv(file,stringsAsFactors=FALSE,
                       fileEncoding="UTF-8-BOM"),error=function(e)NULL)
      if(!is.null(x)&&"record_id"%in%names(x))
        previous<-c(previous,as.character(x$record_id))
    }
    candidates<-setdiff(sort(unique(eligible)),unique(previous))
    if(length(candidates)<n)
      stop("Only ",length(candidates)," unused confirmed case-positive IDs remain; need ",n,
           ". No model requests were made.")
    set.seed(seed)
    candidates<-sample(candidates,length(candidates))
    chosen<-list()
    for(id in candidates) {
      if(length(chosen)>=n)break
      payload<-tryCatch(w09_text_payload(w09,id,max_pages=0L),error=function(e)e)
      if(inherits(payload,"error"))next
      i<-match(id,q$record_id)
      chosen[[length(chosen)+1L]]<-data.frame(
        record_id=id,title=as.character(q$title[[i]]),
        doi=as.character(q$doi[[i]]),
        source_sha256=payload$sha256,
        stringsAsFactors=FALSE)
    }
    if(length(chosen)!=n)
      stop("Only ",length(chosen)," unused readable confirmed-case full texts found; need ",n)
    selected<-do.call(rbind,chosen)
    utils::write.csv(selected,sample_file,row.names=FALSE,na="",fileEncoding="UTF-8")
    jsonlite::write_json(list(batch_id=batch_id,n=n,seed=seed,w09=w09,
       classification_sha256=sha(classifications_csv),queue_sha256=sha(qfile),
       source_prompt_sha256=source_prompt_sha,
       staged_prompt_sha256=sha(staged_prompt),
       schema_sha256=source_schema_sha,sample_sha256=sha(sample_file)),
       manifest_file,auto_unbox=TRUE,pretty=TRUE)
    message("[OSTARA pilot] New selection saved: ",sample_file)
  }
  if(!file.exists(manifest_file))stop("Sample exists without batch manifest.")
  manifest<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
  checks<-list(batch_id=batch_id,n=n,w09=w09,
     classification_sha256=sha(classifications_csv),
     queue_sha256=sha(qfile),
     source_prompt_sha256=source_prompt_sha,
     staged_prompt_sha256=sha(staged_prompt),
     schema_sha256=source_schema_sha,sample_sha256=sha(sample_file))
  for(key in names(checks))
    if(!identical(manifest[[key]],checks[[key]]))
      stop("Batch provenance mismatch: ",key)
  selected<-utils::read.csv(sample_file,stringsAsFactors=FALSE,
                            check.names=FALSE,fileEncoding="UTF-8-BOM")
  if(nrow(selected)!=n||anyDuplicated(selected$record_id)||
     !all(selected$record_id%in%eligible))
    stop("Saved sample does not contain exactly the specified confirmed-case IDs.")
  # Ensure that the source files remain identical across resumes.
  for(i in seq_len(nrow(selected))) {
    payload<-w09_text_payload(w09,selected$record_id[[i]],max_pages=0L)
    if(!identical(payload$sha256,selected$source_sha256[[i]]))
      stop("Full-text source hash changed for ",selected$record_id[[i]])
  }
  ids<-selected$record_id
  extraction_dir<-file.path(w09,"case_data_extraction",paste0(
     "csv-",substr(sha(staged_schema),1,12),
     "-prompt-",substr(sha(staged_prompt),1,12),"-model-gpt-5.6-luna"))
  # A shared extraction directory can contain other batches; count only
  # the saved IDs from THIS batch, never all global checkpoints.
  done_before<-ids_from(file.path(extraction_dir,"case_data.jsonl"))
  new_this_call<-setdiff(ids,done_before)
  if(!file.exists(file.path(dir,"batch_started.json"))&&
     length(intersect(ids,done_before)))
    stop("First run overlaps existing extraction checkpoints; no API call made.")
  if(!file.exists(file.path(dir,"batch_started.json")))
    jsonlite::write_json(list(sample_sha256=sha(sample_file),
      prompt_sha256=sha(staged_prompt),schema_sha256=sha(staged_schema)),
      file.path(dir,"batch_started.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[OSTARA pilot] Batch IDs: ",n,
          "; completed in THIS batch: ",n-length(new_this_call),
          "; remaining: ",length(new_this_call),
          "; older global checkpoints not counted.")
  error<-NULL
  if(length(new_this_call))
    tryCatch(run_w09_case_data_extraction(
      w09=w09,prompt_file=staged_prompt,schema_file=staged_schema,
      record_ids=ids,max_records=0L,model="gpt-5.6-luna",
      max_pages=0L,revalidate_only=FALSE),
      error=function(e){error<<-conditionMessage(e);NULL})
  # Read and export only this batch's validated records.
  ledger<-readjl(file.path(extraction_dir,"case_data.jsonl"))
  lid<-vapply(ledger,function(z)chr(z$record_id),character(1))
  if(anyDuplicated(lid))stop("Duplicate IDs in extraction checkpoint.")
  mapping_file<-file.path(extraction_dir,"field_mapping.csv")
  mapping<-if(file.exists(mapping_file))utils::read.csv(mapping_file,
    stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8-BOM") else
    transform(spec,field_id=sprintf("field_%03d",seq_len(nrow(spec))))
  if(nrow(mapping)!=nrow(spec)||
     !identical(as.character(mapping$Question),as.character(spec$Question)))
    stop("Checkpoint mapping does not match case schema.")
  columns<-unlist(lapply(seq_len(nrow(spec)),function(i){
    h<-paste0(sprintf("%02d",i),". ",trimws(spec$Question[[i]]))
    c(h,paste0(h," [evidence]"),paste0(h," [reporting status]"))
  }),use.names=FALSE)
  columns<-make.unique(columns)
  base<-c("record_id","title","doi","publication_extraction_status",
          "case_id","assessment_reason","case_boundary_uncertainty")
  output_cols<-c(base,columns)
  rows<-list();statuses<-list()
  for(i in seq_len(nrow(selected))) {
    id<-selected$record_id[[i]]
    j<-match(id,lid)
    if(is.na(j)) {
      statuses[[i]]<-data.frame(record_id=id,status="unresolved",cases=NA_integer_)
      next
    }
    rec<-ledger[[j]]
    statuses[[i]]<-data.frame(record_id=id,status="validated",
                              cases=length(rec$cases))
    makebase<-function()c(record_id=id,title=chr(selected$title[[i]]),
      doi=chr(selected$doi[[i]]),publication_extraction_status=
      chr(rec$publication_extraction_status),case_id="",
      assessment_reason="",case_boundary_uncertainty="")
    if(!length(rec$cases)) {
      row<-c(makebase(),setNames(rep("",length(columns)),columns))
      row["case_id"]<-"NO QUALIFYING CASE"
      rows[[length(rows)+1L]]<-row
      next
    }
    for(case in rec$cases) {
      row<-makebase()
      row["case_id"]<-chr(case$case_id)
      row["assessment_reason"]<-chr(case$assessment_reason)
      row["case_boundary_uncertainty"]<-chr(case$case_boundary_uncertainty)
      values<-character()
      for(k in seq_len(nrow(mapping))) {
        f<-case$fields[[mapping$field_id[[k]]]]
        if(is.null(f))stop("Missing checkpoint field ",mapping$field_id[[k]],
                           " for ",id)
        v<-if(!is.null(f$values))f$values else f$value
        v<-as.character(unlist(v,use.names=FALSE))
        factor_field<-grepl("enabler|barrier",mapping$Question[[k]],ignore.case=TRUE)
        answer<-if(!length(v))"" else if(factor_field)
          paste0("\u2022 ",paste(v,collapse="\n\u2022 ")) else paste(v,collapse=" | ")
        evidence<-if(length(f$evidence))vapply(f$evidence,
          function(z)paste0(chr(z$anchor),": ",chr(z$quote)),character(1))
          else character()
        values<-c(values,answer,paste(evidence,collapse="\n\n"),
                  chr(f$reporting_status))
      }
      names(values)<-columns
      rows[[length(rows)+1L]]<-c(row,values)
    }
  }
  if(length(rows)) {
    tab<-as.data.frame(do.call(rbind,rows),stringsAsFactors=FALSE,
                        check.names=FALSE)
    names(tab)<-output_cols
  } else {
    tab<-as.data.frame(matrix(character(),nrow=0L,ncol=length(output_cols)),
                       stringsAsFactors=FALSE,check.names=FALSE)
    names(tab)<-output_cols
  }
  status<-do.call(rbind,statuses)
  invalid<-readjl(file.path(extraction_dir,"invalid_responses.jsonl"))
  unresolved<-status$record_id[status$status=="unresolved"]
  bad<-Filter(function(z)chr(z$record_id)%in%unresolved,invalid)
  failures<-if(length(bad))do.call(rbind,lapply(bad,function(z)
    data.frame(record_id=chr(z$record_id),attempt=chr(z$attempt),
               issue=chr(z$issue),stringsAsFactors=FALSE))) else
    data.frame(record_id=character(),attempt=character(),issue=character())
  write_excel_csv<-function(x,path) {
    # BOM helps Excel recognise UTF-8; write.csv quotes multiline cells.
    con<-file(path,open="wb")
    on.exit(close(con),add=TRUE)
    writeBin(as.raw(c(0xEF,0xBB,0xBF)),con)
    utils::write.table(x,file=con,sep=",",row.names=FALSE,col.names=TRUE,
                       quote=TRUE,na="",qmethod="double",fileEncoding="")
  }
  out<-file.path(dir,"case_data_wide.csv")
  status_file<-file.path(dir,"publication_status.csv")
  errors_file<-file.path(dir,"validation_errors.csv")
  write_excel_csv(tab,out)
  write_excel_csv(status,status_file)
  write_excel_csv(failures,errors_file)
  message("[OSTARA pilot] Validated THIS batch: ",sum(status$status=="validated"),
          "/",n,"; cases: ",sum(status$cases,na.rm=TRUE),
          "; unresolved: ",length(unresolved))
  message("[OSTARA pilot] CSV: ",out)
  if(!is.null(error))warning("Extraction interrupted: ",error,call.=FALSE)
  invisible(list(csv=out,status=status_file,errors=errors_file,
     sample=sample_file,selected=n,validated=sum(status$status=="validated"),
     unresolved=unresolved,extraction_dir=extraction_dir))
}
