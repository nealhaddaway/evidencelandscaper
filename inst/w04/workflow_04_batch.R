# Asynchronous Batch API transport for the unchanged W04 Responses request.
# Loaded by workflow_04_luna_consensus.R only when --execution batch is selected.
# Every model input comes from screen_request_body(), and all accepted outputs
# pass through screen_response_row(). This helper never edits existing rows.

batch_api <- function(method,endpoint,body=NULL,file=NULL) {
  req<-httr2::request(paste0("https://api.openai.com/v1/",endpoint)) |>
    httr2::req_method(method) |>
    httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
    httr2::req_timeout(240) |>
    httr2::req_retry(max_tries=4,backoff=~min(30,2^.x))
  if(!is.null(file)) {
    req<-httr2::req_body_multipart(req,purpose="batch",
                                  file=httr2::upload_file(file,type="application/jsonl"))
  } else if(!is.null(body)) {
    req<-httr2::req_body_json(req,body,auto_unbox=TRUE)
  }
  response<-httr2::req_perform(req)
  if(identical(method,"GET") && grepl("/content$",endpoint))
    return(httr2::resp_body_string(response))
  httr2::resp_body_json(response,simplifyVector=FALSE)
}

batch_atomic_json <- function(value,path) {
  tmp<-paste0(path,".tmp")
  jsonlite::write_json(value,tmp,auto_unbox=TRUE,pretty=TRUE,null="null",na="null")
  if(!file.rename(tmp,path))stop("Cannot atomically write batch state: ",path,call.=FALSE)
}

batch_atomic_text <- function(value,path) {
  tmp<-paste0(path,".tmp")
  writeLines(value,tmp,useBytes=TRUE)
  if(!file.rename(tmp,path))stop("Cannot atomically write batch output: ",path,call.=FALSE)
}

batch_state <- function(file)jsonlite::fromJSON(file,simplifyVector=FALSE)
batch_scalar <- function(x)if(is.null(x)||!length(x))"" else as.character(x[[1L]])

batch_download <- function(file_id,target) {
  if(!nzchar(batch_scalar(file_id)))return(FALSE)
  if(!file.exists(target))
    batch_atomic_text(batch_api("GET",paste0("files/",batch_scalar(file_id),"/content")),target)
  TRUE
}

batch_create <- function(records,pass,job_dir,ordinal,pilot=FALSE) {
  dir.create(job_dir,recursive=TRUE,showWarnings=FALSE)
  stem<-sprintf("pass%d-%04d",pass,ordinal)
  input<-file.path(job_dir,paste0(stem,"-input.jsonl"))
  state_file<-file.path(job_dir,paste0(stem,"-state.json"))
  if(file.exists(state_file)||file.exists(input))
    stop("Existing batch submission files: ",stem,"; refusing overwrite.",call.=FALSE)
  keys<-sprintf("w04-p%d-%s-%06d",pass,substr(PROMPT_SHA256,1L,12L),
                seq_along(records))
  entries<-lapply(seq_along(records),function(i)list(
    custom_id=keys[[i]],
    method="POST",
    url="/v1/responses",
    body=screen_request_body(records[[i]])
  ))
  input_lines<-vapply(entries,function(x)
    jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),character(1))
  # Custom IDs must be unique within each batch; job name distinguishes batches.
  keys<-paste0(stem,"-",seq_along(records))
  for(i in seq_along(entries))entries[[i]]$custom_id<-keys[[i]]
  input_lines<-vapply(entries,function(x)
    jsonlite::toJSON(x,auto_unbox=TRUE,null="null",na="null",digits=NA),character(1))
  batch_atomic_text(input_lines,input)
  ids<-vapply(records,record_id,character(1))
  state<-list(
    schema="ostara-w04-batch-v1",pass=pass,pilot=isTRUE(pilot),
    source_prompt_sha256=PROMPT_SHA256,model=model,
    input_sha256=digest::digest(file=input,algo="sha256",serialize=FALSE),
    record_ids=as.list(ids),custom_ids=as.list(keys),
    input_file_id=NULL,batch_id=NULL,
    stage="prepared",created_at=now_utc()
  )
  batch_atomic_json(state,state_file)
  message("[W04 Batch] Uploading ",stem," (",length(records)," requests).")
  uploaded<-batch_api("POST","files",file=input)
  if(!nzchar(batch_scalar(uploaded$id)))stop("Batch file upload returned no file ID.",call.=FALSE)
  state$input_file_id<-batch_scalar(uploaded$id)
  state$stage<-"uploaded"
  batch_atomic_json(state,state_file)
  # Write an explicit barrier before batch creation. If the network fails
  # after remote creation but before local receipt, do NOT blindly resubmit.
  state$stage<-"creating"
  batch_atomic_json(state,state_file)
  created<-batch_api("POST","batches",body=list(
    input_file_id=state$input_file_id,
    endpoint="/v1/responses",
    completion_window="24h",
    metadata=list(project="ostara-w04",pass=as.character(pass),job=stem)
  ))
  if(!nzchar(batch_scalar(created$id)))stop("Batch created without batch ID.",call.=FALSE)
  state$batch_id<-batch_scalar(created$id)
  state$stage<-"submitted"
  batch_atomic_json(state,state_file)
  message("[W04 Batch] Submitted ",stem,": ",state$batch_id)
  invisible(state)
}

batch_collect <- function(state_file,record_lookup,pass,path,existing) {
  state<-batch_state(state_file)
  stem<-sub("-state[.]json$","",basename(state_file))
  if(!identical(state$schema,"ostara-w04-batch-v1") ||
     !identical(as.integer(state$pass),pass) ||
     !identical(batch_scalar(state$model),model) ||
     !identical(batch_scalar(state$source_prompt_sha256),PROMPT_SHA256))
    stop("W04 batch provenance mismatch: ",state_file,call.=FALSE)
  input<-file.path(dirname(state_file),paste0(stem,"-input.jsonl"))
  if(!file.exists(input)||!identical(digest::digest(file=input,algo="sha256",
        serialize=FALSE),batch_scalar(state$input_sha256)))
    stop("Batch input bytes changed: ",input,call.=FALSE)
  if(!nzchar(batch_scalar(state$batch_id))) {
    if(identical(batch_scalar(state$stage),"creating"))
      stop("Batch submission outcome is uncertain for ",stem,
           ". Inspect the OpenAI batches dashboard for uploaded file ",
           batch_scalar(state$input_file_id),
           "; do not submit a duplicate.",call.=FALSE)
    stop("Unsubmitted batch state at ",state_file,
         ". Inspect before resuming to avoid duplicate charges.",call.=FALSE)
  }
  if(identical(batch_scalar(state$stage),"collected"))return(TRUE)
  remote<-batch_api("GET",paste0("batches/",batch_scalar(state$batch_id)))
  status<-batch_scalar(remote$status)
  message("[W04 Batch] ",stem," ",batch_scalar(state$batch_id),
          ": ",status," (",batch_scalar((remote$request_counts %||% list())$completed),
          "/",length(state$record_ids)," completed)")
  if(status%in%c("validating","in_progress","finalizing","cancelling"))
    return(FALSE)
  if(identical(status,"failed")) {
    batch_atomic_json(remote,file.path(dirname(state_file),paste0(stem,"-remote-failure.json")))
    stop("Remote Batch API validation failed for ",stem,
         ". See remote-failure.json; source checkpoints are unchanged.",call.=FALSE)
  }
  if(!status%in%c("completed","expired","cancelled"))
    stop("Unknown Batch API status: ",status,call.=FALSE)
  results_file<-file.path(dirname(state_file),paste0(stem,"-output.jsonl"))
  errors_file<-file.path(dirname(state_file),paste0(stem,"-errors.jsonl"))
  batch_download(remote$output_file_id,results_file)
  batch_download(remote$error_file_id,errors_file)
  result_rows<-if(file.exists(results_file))read_jsonl(results_file) else list()
  error_rows<-if(file.exists(errors_file))read_jsonl(errors_file) else list()
  all_rows<-c(result_rows,error_rows)
  result_keys<-vapply(all_rows,function(x)batch_scalar(x$custom_id),character(1))
  keys<-unlist(state$custom_ids,use.names=FALSE)
  ids<-unlist(state$record_ids,use.names=FALSE)
  if(anyDuplicated(result_keys)||any(!result_keys%in%keys)||
     any(!nzchar(result_keys)))
    stop("Unexpected or duplicate custom_id in remote Batch output: ",stem,call.=FALSE)
  if(identical(status,"completed")&&!setequal(result_keys,keys))
    stop("Completed batch has missing result IDs; refusing silent omissions: ",stem,call.=FALSE)
  if(isTRUE(state$pilot)) {
    if(!identical(status,"completed")||length(all_rows)!=length(keys)||
       length(error_rows)>0L)
      stop("W04 Batch pilot did not complete cleanly: ",stem,
           ". Inspect saved output/error files before submitting the full corpus.",
           call.=FALSE)
    for(i in seq_along(keys)) {
      result<-all_rows[[match(keys[[i]],result_keys)]]
      if(!identical(as.integer(result$response$status_code %||% 0L),200L) ||
         is.null(result$response$body))
        stop("W04 Batch pilot rejected a model request: ",stem,
             ". Raw output saved at ",results_file,call.=FALSE)
      tryCatch(screen_response_row(result$response$body,
                record_lookup[[ids[[i]]]],pass),error=function(e)
        stop("W04 Batch pilot output validation failed: ",
             conditionMessage(e),". Raw output: ",results_file,call.=FALSE))
    }
    message("[W04 Batch] Model/endpoint pilot passed: ",
            length(keys)," valid gpt-5.6-luna votes.")
  }
  complete_ids<-if(length(existing))
    vapply(existing,function(x)batch_scalar(x$record_id),character(1)) else character()
  for(i in seq_along(ids)){
    if(ids[[i]]%in%complete_ids)next
    match_at<-match(keys[[i]],result_keys)
    if(is.na(match_at))next  # expired/cancelled: retry only missing requests later
    result<-all_rows[[match_at]]
    rec<-record_lookup[[ids[[i]]]]
    row<-tryCatch({
      status_code<-as.integer(result$response$status_code %||% 0L)
      if(!identical(status_code,200L)||is.null(result$response$body))
        stop("Batch item API failure: ",jsonlite::toJSON(
          result$error %||% result$response,auto_unbox=TRUE))
      screen_response_row(result$response$body,rec,pass)
    },error=function(e)screen_failure_row(rec,pass,conditionMessage(e)))
    if(!identical(batch_scalar(row$record_id),ids[[i]]) ||
       !identical(as.integer(row$pass),pass))
      stop("Batch output record identity mismatch.",call.=FALSE)
    append_jsonl(row,path)
    complete_ids<-c(complete_ids,ids[[i]])
    if(length(complete_ids)%%checkpoint_every==0L)
      cat(sprintf("PASS %d checkpoint: %d/%d\n",
                  pass,length(complete_ids),length(record_lookup)))
  }
  state$stage<-"collected"
  state$remote_status<-status
  state$output_file_id<-remote$output_file_id
  state$error_file_id<-remote$error_file_id
  state$collected_at<-now_utc()
  batch_atomic_json(state,state_file)
  TRUE
}

batch_run_pass <- function(records,pass,path,existing) {
  ids<-vapply(records,record_id,character(1))
  lookup<-setNames(records,ids)
  job_dir<-file.path(output_dir,"batch_api",paste0("pass",pass))
  dir.create(job_dir,recursive=TRUE,showWarnings=FALSE)
  states<-sort(list.files(job_dir,pattern="-state[.]json$",full.names=TRUE))
  active<-FALSE
  if(length(states))for(state_file in states) {
    ready<-batch_collect(state_file,lookup,pass,path,existing)
    if(!ready)active<-TRUE
    existing<-if(file.exists(path))read_jsonl(path) else list()
  }
  done<-if(length(existing))vapply(existing,
    function(x)batch_scalar(x$record_id),character(1)) else character()
  pending<-which(!ids%in%done)
  if(active) {
    message("[W04 Batch] Pass ",pass,": ",length(pending),
            " votes outstanding. R can be closed; rerun to collect.")
    return(NULL)
  }
  if(length(pending)) {
    # Initial three-record pilot verifies custom model/endpoint support before
    # committing the remainder of the screening corpus to asynchronous billing.
    pilot<-identical(pass,1L)&&!length(states)&&length(pending)>3L
    if(pilot) {
      chunks<-list(head(pending,3L))
      message("[W04 Batch] Submitting a 3-record compatibility pilot first.")
    } else {
      chunks<-split(pending,ceiling(seq_along(pending)/batch_chunk_size))
    }
    next_number<-length(states)+1L
    for(i in seq_along(chunks)) {
      batch_create(records[chunks[[i]]],pass,job_dir,next_number+i-1L,
                   pilot=pilot)
    }
    message("[W04 Batch] Submitted ",length(chunks)," new batch(es) for pass ",
            pass,". R can be closed. Rerun the same command to collect results.")
    return(NULL)
  }
  message("[W04 Batch] Pass ",pass," complete: ",length(done),"/",length(ids))
  existing
}
