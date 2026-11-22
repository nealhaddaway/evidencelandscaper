#' Code case mechanism summaries against the exact OSTARA v5 ontology
#'
#' One resumable Luna request per chunk of case rows. The 26-code ontology
#' and the original 559-row source CSV are SHA256 locked. The model receives
#' both mechanism summaries and their extracted source-evidence passages.
#' Multiple codes or no supported code are allowed. Every returned code and
#' supporting quote is validated before a checkpoint is committed.
#' Input files, W07 and W09 results are never modified.
#'
#' @param cases_csv Case mechanism spreadsheet (record_id, case_id, mechanism
#'   summary, evidence); the two question columns are matched by name.
#' @param ontology_csv Exact v5 mechanism ontology (mechanism_id, category,
#'   function, mechanism, definition).
#' @param output_dir New output directory, or existing matching run to resume.
#' @param batch_size Number of cases per Luna call (default 15).
#' @param max_batches Maximum NEW requests this call; 0 means all.
#' @param model Luna model.
#' @param run_extraction FALSE creates manifest and exports existing results
#'   without making API calls; TRUE uses Luna to process pending batches.
#' @return Paths to per-case and long-format classification CSVs and status.
#' @export
code_ostara_case_mechanisms_v5 <- function(
    cases_csv,ontology_csv,output_dir,batch_size=15L,max_batches=0L,
    model="gpt-5.6-luna",run_extraction=TRUE) {
  if(!is.character(model)||length(model)!=1L||
     is.na(model)||!grepl("-luna$",model))
    stop("A Luna model is required.")
  if(!is.logical(run_extraction)||length(run_extraction)!=1L||
     is.na(run_extraction))stop("run_extraction must be TRUE/FALSE.")
  if(length(batch_size)!=1L||is.na(batch_size)||
     batch_size!=as.integer(batch_size)||batch_size<1L||batch_size>40L)
    stop("batch_size must be between 1 and 40.")
  if(length(max_batches)!=1L||is.na(max_batches)||
     max_batches!=as.integer(max_batches)||max_batches<0L)
    stop("max_batches must be zero or a positive integer.")
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  cases_csv<-norm(cases_csv);ontology_csv<-norm(ontology_csv)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  csv<-function(p)utils::read.csv(p,check.names=FALSE,
    stringsAsFactors=FALSE,colClasses="character",
    fileEncoding="UTF-8-BOM",na.strings=character())
  cases<-csv(cases_csv);ont<-csv(ontology_csv)
  fields<-c("mechanism_id","category","function","mechanism","definition")
  if(!identical(names(ont),fields)||nrow(ont)!=26L||
     anyNA(ont)||any(!nzchar(trimws(as.matrix(ont))))||
     anyDuplicated(ont$mechanism_id))
    stop("Expected the complete unique 26-code v5 ontology.")
  key<-c("record_id","case_id")
  if(!all(key%in%names(cases))||ncol(cases)!=4L)
    stop("Expected four-column case mechanism source with record_id/case_id.")
  cols<-setdiff(names(cases),key)
  summary_col<-cols[!grepl("\\[evidence\\]$",cols)]
  evidence_col<-cols[grepl("\\[evidence\\]$",cols)]
  if(length(summary_col)!=1L||length(evidence_col)!=1L||
     !startsWith(summary_col,"13.")||
     !identical(evidence_col,paste0(summary_col," [evidence]")))
    stop("Expected original field 13 mechanism summary and [evidence].")
  if(!nrow(cases)||any(!nzchar(cases$record_id))||
     any(!nzchar(cases$case_id))||anyDuplicated(cases$case_id)||
     any(!nzchar(trimws(cases[[summary_col]])))||
     any(!nzchar(trimws(cases[[evidence_col]]))))
    stop("Missing/duplicate case IDs or empty mechanism summaries/evidence.")
  input_sha<-sha(cases_csv);ontology_sha<-sha(ontology_csv)
  batch_size<-as.integer(batch_size)
  total<-nrow(cases);batch_ids<-ceiling(seq_len(total)/batch_size)
  n_batches<-max(batch_ids)
  if(!dir.exists(output_dir))dir.create(output_dir,recursive=TRUE)
  output_dir<-norm(output_dir)
  manifest_path<-file.path(output_dir,"manifest.json")
  manifest<-list(schema="ostara-v5-case-mechanism-mapping-v1",
    input_sha256=input_sha,ontology_sha256=ontology_sha,
    rows=total,codes=nrow(ont),batch_size=batch_size,
    model=model,summary_column=summary_col,evidence_column=evidence_col,
    prompt_version="ostara_v5_mechanism_row_coder_v1")
  if(file.exists(manifest_path)) {
    prior<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    for(k in names(manifest))if(!identical(prior[[k]],manifest[[k]]))
      stop("Existing mechanism run provenance changed for ",k,
           ". Choose a different output_dir for new inputs/settings.")
  } else {
    if(length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
      stop("Output directory has files but no manifest; refusing overwrite.")
    jsonlite::write_json(manifest,manifest_path,
                         auto_unbox=TRUE,pretty=TRUE)
  }
  jl<-function(p) {
    if(!file.exists(p))return(list())
    x<-readLines(p,encoding="UTF-8",warn=FALSE)
    x<-x[nzchar(trimws(x))]
    lapply(seq_along(x),function(i)tryCatch(
      jsonlite::fromJSON(x[[i]],simplifyVector=FALSE),
      error=function(e)stop("Malformed checkpoint line ",i,": ",p)))
  }
  ledger<-file.path(output_dir,"coded_batches.jsonl")
  rawlog<-file.path(output_dir,"model_responses.jsonl")
  errors<-file.path(output_dir,"failed_batches.jsonl")
  done<-jl(ledger)
  done_ids<-if(length(done))
    vapply(done,function(z)as.integer(z$batch_id),integer(1))
    else integer()
  if(anyDuplicated(done_ids)||anyNA(done_ids)||
     any(!done_ids%in%seq_len(n_batches)))
    stop("Checkpoint has invalid or duplicate batch identifiers.")
  allowed<-ont$mechanism_id
  # A supporting quotation must be included in the input summary or
  # extracted evidence, not merely in the ontology definition.
  check_batch<-function(z,b) {
    ix<-which(batch_ids==b)
    requested<-as.character(cases$case_id[ix])
    if(!is.list(z)||!is.list(z$rows)||length(z$rows)!=length(ix))
      stop("Model did not return exactly the expected number of rows.")
    result_ids<-vapply(z$rows,function(a) {
      if(!is.list(a)||!is.character(a$case_id)||
         length(a$case_id)!=1L)return("")
      a$case_id
    },character(1))
    if(anyDuplicated(result_ids)||!setequal(result_ids,requested))
      stop("Model case IDs do not match the exact requested batch.")
    sorted<-z$rows[match(requested,result_ids)]
    for(i in seq_along(sorted)) {
      a<-sorted[[i]]
      if(!is.list(a$assignments))stop("Missing assignments array.")
      if(length(a$assignments)) {
        codes<-vapply(a$assignments,function(m) {
          if(!is.list(m)||!is.character(m$mechanism_id)||
             length(m$mechanism_id)!=1L)return("")
          m$mechanism_id
        },character(1))
        if(any(!codes%in%allowed)||anyDuplicated(codes))
          stop("Unknown/duplicate mechanism codes: ",requested[[i]])
        target<-paste(cases[[summary_col]][ix[[i]]],
                      cases[[evidence_col]][ix[[i]]],sep="\n")
        for(m in a$assignments) {
          if(!is.character(m$supporting_quote)||
             length(m$supporting_quote)!=1L||
             !nzchar(trimws(m$supporting_quote))||
             !grepl(trimws(m$supporting_quote),target,fixed=TRUE))
            stop("Mechanism quote is not verbatim from input: ",
                 requested[[i]]," / ",m$mechanism_id)
          if(!is.character(m$rationale)||length(m$rationale)!=1L||
             !nzchar(trimws(m$rationale)))
            stop("Mechanism assignment missing rationale: ",
                 requested[[i]])
        }
      }
      if(!is.character(a$status)||length(a$status)!=1L||
         !a$status%in%c("coded","no_supported_match","uncertain")||
         (a$status=="coded"&&!length(a$assignments))||
         (a$status=="no_supported_match"&&length(a$assignments)))
        stop("Invalid case classification status: ",requested[[i]])
    }
    sorted
  }
  # Revalidate checkpoints before using, to catch manual edits or corruption.
  for(d in done)check_batch(list(rows=d$rows),as.integer(d$batch_id))
  instructions<-paste(
    "You code documented mechanisms of collective action against a closed",
    "26-code OSTARA v5 ontology. Use the provided mechanism summaries AND",
    "supporting evidence. Assign ALL supported mechanism codes per case;",
    "multiple codes are allowed, but do not add speculative codes.",
    "Mechanism IDs must be exactly from the supplied ontology. Match on the",
    "definition, NOT superficial keyword similarity. Only classify activities",
    "the case actually operationalises, not proposed or unrelated approaches.",
    "For each assignment supply a short rationale and a SHORT EXACT VERBATIM",
    "supporting_quote from that case's summary or evidence. No invented",
    "quotes. Do not use the ontology definition as source evidence.",
    "If no code has support, return no_supported_match and [] assignments.",
    "If evidence is materially ambiguous, use status uncertain (retain only",
    "defensible supported assignments) and explain uncertainty in note.",
    "Return ONLY JSON object {rows:[{case_id, status, note,",
    "assignments:[{mechanism_id,supporting_quote,rationale}]}]}.",
    "Provide exactly one row for every supplied case_id, in input order."
  )
  batches_to_run<-setdiff(seq_len(n_batches),done_ids)
  if(max_batches>0L)batches_to_run<-head(batches_to_run,max_batches)
  if(run_extraction&&length(batches_to_run)&&
     !nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY not set. No calls made.")
  if(run_extraction)for(b in batches_to_run) {
    ix<-which(batch_ids==b)
    payload<-list(ontology=lapply(seq_len(nrow(ont)),function(i)
      as.list(ont[i,,drop=FALSE])),
      cases=lapply(ix,function(i)list(
        case_id=cases$case_id[[i]],
        record_id=cases$record_id[[i]],
        mechanism_summary=cases[[summary_col]][[i]],
        extracted_evidence=cases[[evidence_col]][[i]])))
    body<-list(model=model,store=FALSE,
      reasoning=list(effort="low"),
      input=list(
        list(role="system",content=list(list(
          type="input_text",text=instructions))),
        list(role="user",content=list(list(type="input_text",
          text=as.character(jsonlite::toJSON(payload,
                 auto_unbox=TRUE,null="null")))))),
      text=list(format=list(type="json_object")))
    response<-NULL;err<-NULL
    for(attempt in seq_len(3L)) {
      call<-tryCatch(
        httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>
          httr2::req_perform(),
        error=function(e)e)
      if(inherits(call,"error")) {
        err<-conditionMessage(call)
        if(attempt<3L)Sys.sleep(5*attempt)
        next
      }
      response<-httr2::resp_body_json(call,simplifyVector=FALSE)
      parts<-unlist(lapply(response$output,function(z)
        unlist(lapply(z$content,function(c)
          if(identical(c$type,"output_text"))c$text else NULL),
          use.names=FALSE)),use.names=FALSE)
      if(!length(parts)) {
        err<-"Model returned no JSON text."
        if(attempt<3L)Sys.sleep(5*attempt)
        next
      }
      raw<-as.character(parts[[1L]])
      # Raw response is retained even when schema validation fails.
      cat(as.character(jsonlite::toJSON(list(batch_id=b,raw=raw),
          auto_unbox=TRUE,null="null")),"\n",file=rawlog,
          append=TRUE,sep="")
      validated<-tryCatch({
        parsed<-jsonlite::fromJSON(raw,simplifyVector=FALSE)
        check_batch(parsed,b)
      },error=function(e)e)
      if(inherits(validated,"error")) {
        err<-conditionMessage(validated)
        if(attempt<3L)Sys.sleep(5*attempt)
        next
      }
      checkpoint<-list(batch_id=b,rows=validated)
      cat(as.character(jsonlite::toJSON(checkpoint,
          auto_unbox=TRUE,null="null")),"\n",file=ledger,
          append=TRUE,sep="")
      done[[length(done)+1L]]<-checkpoint
      message("[OSTARA mechanisms] checkpointed batch ",b,"/",n_batches,
              " (",length(ix)," cases)")
      err<-NULL
      break
    }
    if(!is.null(err)) {
      cat(as.character(jsonlite::toJSON(list(batch_id=b,error=err),
        auto_unbox=TRUE,null="null")),"\n",file=errors,
        append=TRUE,sep="")
      warning("Batch ",b," failed after retries: ",err,
              "; checkpointed batches remain safe.",call.=FALSE)
    }
  }
  # Convert completed checkpoints to both case-wide and assignment-long CSV.
  completed<-jl(ledger)
  rows<-unlist(lapply(completed,function(d)d$rows),recursive=FALSE)
  mapped<-setNames(rows,vapply(rows,function(z)z$case_id,character(1)))
  if(anyDuplicated(names(mapped)))
    stop("Duplicate case IDs in extracted checkpoints.")
  wide<-data.frame(record_id=cases$record_id,case_id=cases$case_id,
    mechanism_summary=cases[[summary_col]],
    mechanism_evidence=cases[[evidence_col]],
    coding_status=rep("not_yet_processed",total),
    mechanism_ids=rep("",total),mechanism_labels=rep("",total),
    model_note=rep("",total),stringsAsFactors=FALSE)
  assigned<-list()
  for(i in seq_len(total)) {
    row<-mapped[[cases$case_id[[i]]]]
    if(is.null(row))next
    wide$coding_status[[i]]<-row$status
    wide$model_note[[i]]<-if(is.null(row$note))"" else
      as.character(row$note)
    if(!length(row$assignments))next
    codes<-vapply(row$assignments,function(m)m$mechanism_id,character(1))
    wide$mechanism_ids[[i]]<-paste(codes,collapse="; ")
    wide$mechanism_labels[[i]]<-paste(ont$mechanism[
      match(codes,ont$mechanism_id)],collapse="; ")
    for(m in row$assignments) {
      j<-match(m$mechanism_id,ont$mechanism_id)
      assigned[[length(assigned)+1L]]<-data.frame(
        record_id=cases$record_id[[i]],case_id=cases$case_id[[i]],
        mechanism_id=m$mechanism_id,
        category=ont$category[[j]],function_label=ont[["function"]][[j]],
        mechanism=ont$mechanism[[j]],definition=ont$definition[[j]],
        supporting_quote=m$supporting_quote,rationale=m$rationale,
        coding_status=row$status,stringsAsFactors=FALSE)
    }
  }
  long_names<-c("record_id","case_id","mechanism_id","category",
    "function_label","mechanism","definition","supporting_quote",
    "rationale","coding_status")
  long<-if(length(assigned))do.call(rbind,assigned) else
    as.data.frame(setNames(replicate(length(long_names),character(),
      simplify=FALSE),long_names),stringsAsFactors=FALSE)
  save_csv<-function(d,p) {
    tmp<-tempfile(tmpdir=output_dir,fileext=".csv")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    con<-file(tmp,open="wb")
    writeBin(as.raw(c(239L,187L,191L)),con)
    utils::write.table(d,con,sep=",",row.names=FALSE,
      col.names=TRUE,quote=TRUE,qmethod="double",na="")
    close(con)
    if(!file.copy(tmp,p,overwrite=TRUE))stop("Cannot write ",p)
  }
  casefile<-file.path(output_dir,"case_mechanism_codes_wide.csv")
  assignmentsfile<-file.path(output_dir,"case_mechanism_assignments_long.csv")
  save_csv(wide,casefile);save_csv(long,assignmentsfile)
  message("[OSTARA mechanisms] ",sum(wide$coding_status!="not_yet_processed"),
    "/",total," case rows checkpointed; ",
    sum(wide$coding_status=="not_yet_processed")," outstanding; ",
    nrow(long)," mechanism assignments.")
  invisible(list(wide_csv=casefile,long_csv=assignmentsfile,
    manifest=manifest_path,ledger=ledger,failed_batches=errors,
    completed=sum(wide$coding_status!="not_yet_processed"),
    total=total,assignments=nrow(long),batch_count=n_batches))
}
