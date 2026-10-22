#' Export records missing abstracts and selectively rerun W04 with supplied abstracts
#'
#' The rescreen is ISOLATED: it does not alter W02, W03, W04 consensus or
#' adjudication files. It uses the packaged original W04 two-pass/third-pass
#' engine with a subset of canonical and W03 status records.
#' @param w04 Existing completed W04 local run directory.
#' @param abstracts_csv Optional CSV with record_id and abstract columns.
#'   When omitted, creates an editable template for records lacking abstracts.
#' @param model Model for selective consensus rerun.
#' @return Path to template CSV or isolated rescreen run.
#' @export
rescreen_w04_abstracts <- function(w04, abstracts_csv=NULL, model="gpt-5.6") {
  w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
  parent<-dirname(dirname(w04))
  canonical<-file.path(parent,"W02_main","finalised","canonical.jsonl")
  w03<-file.path(parent,"W03_publication_status","full","publication_status.jsonl")
  layer<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  manifest_path<-file.path(w04,"local_manifest.json")
  config_path<-file.path(w04,"screening_config.json")
  paths<-c(canonical,w03,layer,manifest_path,config_path)
  if(any(!file.exists(paths)))stop("W04 completed model results and W02/W03 inputs required.")
  sha<-function(path)digest::digest(file=path,algo="sha256",serialize=FALSE)
  meta<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
  if(!identical(meta$status,"screened")||
     !identical(meta$consensus_sha256,sha(layer))||
     !identical(meta$input_w02_sha256,sha(canonical))||
     !identical(meta$input_w03_sha256,sha(w03)))
    stop("W04 provenance mismatch.")
  readjl<-function(path) {
    z<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  rows<-readjl(canonical); statuses<-readjl(w03); screen<-readjl(layer)
  ids<-vapply(rows,function(x)as.character(x$identity$record_id),character(1))
  screened_ids<-vapply(screen,function(x)as.character(x$record_id),character(1))
  status_ids<-vapply(statuses,function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(ids)||anyDuplicated(status_ids)||!setequal(ids,status_ids))
    stop("W02/W03 record identity mismatch.")
  # Only records screened by THIS W04 run are eligible for targeted rescoring.
  available<-rows[ids%in%screened_ids]
  available_ids<-vapply(available,function(x)as.character(x$identity$record_id),character(1))
  scalar<-function(x)if(is.null(x)||!length(x))"" else paste(unlist(x,use.names=FALSE),collapse="; ")
  missing<-available[!vapply(available,function(x)nzchar(trimws(scalar(x$canonical$abstract))),logical(1))]
  template<-file.path(w04,"missing_abstracts_template.csv")
  if(is.null(abstracts_csv)) {
    if(!length(missing)) {
      message("[W04] No empty abstracts in the W04 screened records.")
      return(invisible(NULL))
    }
    out<-data.frame(
      record_id=vapply(missing,function(x)as.character(x$identity$record_id),character(1)),
      title=vapply(missing,function(x)scalar(x$canonical$title),character(1)),
      abstract=rep("",length(missing)),stringsAsFactors=FALSE)
    if(file.exists(template)) {
      prior<-utils::read.csv(template,stringsAsFactors=FALSE,check.names=FALSE)
      if(!identical(prior,out))stop("Existing template has changes; refusing overwrite.")
    } else utils::write.csv(out,template,row.names=FALSE,na="")
    message("[W04] Fill the abstract column and save this template: ",template)
    return(invisible(template))
  }
  abstracts_csv<-normalizePath(abstracts_csv,winslash="/",mustWork=TRUE)
  amended<-utils::read.csv(abstracts_csv,stringsAsFactors=FALSE,check.names=FALSE,
                           fileEncoding="UTF-8-BOM",na.strings=character())
  if(!all(c("record_id","abstract")%in%names(amended)))
    stop("CSV must contain record_id and abstract columns.")
  amended$record_id<-trimws(as.character(amended$record_id))
  amended$abstract<-trimws(as.character(amended$abstract))
  amended<-amended[nzchar(amended$abstract),,drop=FALSE]
  if(!nrow(amended))stop("No filled abstracts supplied.")
  if(anyDuplicated(amended$record_id)||
     any(!amended$record_id %in% vapply(missing,function(x)as.character(x$identity$record_id),character(1))))
    stop("Only records with missing abstracts from this W04 run may be rescreened.")
  selected_ids<-amended$record_id
  selected<-available[match(selected_ids,available_ids)]
  for(i in seq_along(selected)) selected[[i]]$canonical$abstract<-amended$abstract[[i]]
  status_map<-setNames(statuses,status_ids)
  selected_status<-unname(status_map[selected_ids])
  for(x in selected_status)
    if(isTRUE(x$publication_status$exclude_from_workflow04))
      stop("W03-excluded record cannot be rescreened.")
  source_hash<-sha(abstracts_csv)
  dest<-file.path(w04,"abstract_rescreens",paste0("subset-",nrow(amended),"-",
      substr(source_hash,1,12),"-",substr(sha(layer),1,10)))
  inputs<-file.path(dest,"inputs"); results<-file.path(dest,"consensus")
  if(dir.exists(dest))stop("Existing rescreen folder preserved; do not overwrite: ",dest)
  dir.create(inputs,recursive=TRUE,showWarnings=FALSE)
  writejl<-function(data,path)
    writeLines(vapply(data,function(x)jsonlite::toJSON(x,auto_unbox=TRUE,
               null="null",na="null",digits=NA),character(1)),path,useBytes=TRUE)
  writejl(selected,file.path(inputs,"canonical.jsonl"))
  writejl(selected_status,file.path(inputs,"publication_status.jsonl"))
  file.copy(abstracts_csv,file.path(inputs,"submitted_abstracts.csv"),overwrite=FALSE)
  oldconfig<-jsonlite::fromJSON(config_path,simplifyVector=FALSE)
  oldprompt<-oldconfig$prompt_path
  if(!file.exists(oldprompt))stop("W04 original prompt missing: ",oldprompt)
  # The original W04 engine expects hash of normalised prompt lines.
  config<-list(prompt_path=oldprompt,prompt_version=oldconfig$prompt_version,
       response_schema_name=oldconfig$response_schema_name,
       expected_prompt_sha256=oldconfig$expected_prompt_sha256)
  jsonlite::write_json(config,file.path(inputs,"screening_config.json"),
                       auto_unbox=TRUE,pretty=TRUE)
  jsonlite::write_json(list(schema="local-w04-abstract-rescreen-v1",
    status="started",original_w04=w04,source_w04_sha256=sha(layer),
    source_w02_sha256=sha(canonical),source_w03_sha256=sha(w03),
    submitted_csv_sha256=source_hash,record_ids=as.list(selected_ids),
    model=model,merge_policy="DO_NOT_MERGE_AUTOMATICALLY"),
    file.path(dest,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  script<-system.file("w04","workflow_04_luna_consensus.R",
                       package="evidencelandscaper")
  if(!nzchar(script))stop("Original W04 engine not installed.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))stop("OPENAI_API_KEY required.")
  args<-c(script,"--canonical",file.path(inputs,"canonical.jsonl"),
     "--workflow03-status",file.path(inputs,"publication_status.jsonl"),
     "--screening-config",file.path(inputs,"screening_config.json"),
     "--output-dir",results,"--model",model,"--max-records","0",
     "--checkpoint-every","10")
  message("[W04] Rescreening ",nrow(amended)," records with newly supplied abstracts.")
  log<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
       args=vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
  code<-attr(log,"status")
  if(!is.null(code)&&code!=0L)stop("Selective rescreen failed:\n",
                                    paste(tail(log,30L),collapse="\n"))
  if(!file.exists(file.path(results,"summary.json")))stop("No rescreen summary.")
  sm<-jsonlite::fromJSON(file.path(results,"summary.json"))
  if(!sm$status%in%c("PASS","HUMAN_REVIEW_REQUIRED")||
     as.integer(sm$records_screened)!=nrow(amended))
    stop("Selective rescreen not complete.")
  meta<-jsonlite::fromJSON(file.path(dest,"manifest.json"),simplifyVector=FALSE)
  meta$status<-"completed"
  meta$consensus_sha256<-sha(file.path(results,"workflow04_consensus_layer.jsonl"))
  jsonlite::write_json(meta,file.path(dest,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[W04] Rescreen output isolated: ",dest,
          "\n[W04] No previous decisions were changed; results require review before merging.")
  invisible(dest)
}
