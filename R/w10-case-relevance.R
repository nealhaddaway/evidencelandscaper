#' Audit W09 case eligibility against the four archived W04 criteria
#'
#' Append-only W10 responses; never edits W09. Uncertain and failed cases
#' remain in the audit, and only eligible observations enter publication.
#' @param extraction_dir Completed W09 extraction directory
#' @param w04_prompt Archived W04 prompt used for this named run
#' @param model Responses API model
#' @return Directory with W10 decisions, manifest and CSV audit.
#' @export
run_w10_case_relevance <- function(extraction_dir,w04_prompt,model="gpt-6-luna") {
  extraction_dir<-normalizePath(extraction_dir,mustWork=TRUE)
  w04_prompt<-normalizePath(w04_prompt,mustWork=TRUE)
  src<-file.path(extraction_dir,"fulltext_case_registry_normalised.jsonl")
  if(!file.exists(src))stop("W09 normalised case registry is missing.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  prompt<-paste(readLines(w04_prompt,warn=FALSE,encoding="UTF-8"),collapse="\n")
  marker<-sha(w04_prompt);source_hash<-sha(src)
  rules_path_pre<-system.file("w10","case_relevance_v2.txt",package="evidencelandscaper")
  if(!nzchar(rules_path_pre))stop("Installed W10 v2 prompt missing.")
  run<-file.path(dirname(extraction_dir),"W10",paste0("relevance-v2-",substr(sha(rules_path_pre),1,10),"-",substr(source_hash,1,12),
      "-",substr(marker,1,10),"-",gsub("[^a-zA-Z0-9_-]","_",model)))
  dir.create(run,recursive=TRUE,showWarnings=FALSE)
  ledger<-file.path(run,"w10_decisions.jsonl")
  records<-lapply(Filter(nzchar,readLines(src,warn=FALSE,encoding="UTF-8")),
                  jsonlite::fromJSON,simplifyVector=FALSE)
  cases<-unlist(lapply(records,function(z)lapply(seq_along(z$cases),function(j)
    list(observation_id=paste0(z$record_id,"-case-",sprintf("%03d",j)),
         record_id=z$record_id,case=z$cases[[j]]))),recursive=FALSE)
  ids<-vapply(cases,function(x)x$observation_id,character(1))
  if(anyDuplicated(ids))stop("W09 contains duplicate case observation IDs.")
  completed<-if(file.exists(ledger))lapply(Filter(nzchar,readLines(ledger,warn=FALSE)),
               jsonlite::fromJSON,simplifyVector=FALSE) else list()
  done<-vapply(completed,function(x)x$observation_id,character(1))
  if(anyDuplicated(done)||any(!done%in%ids))stop("W10 checkpoint contains invalid IDs.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")) && length(done)<length(ids))
    stop("OPENAI_API_KEY required for unfinished W10 cases.")
  rules_path<-system.file("w10","case_relevance_v2.txt",package="evidencelandscaper")
  if(!nzchar(rules_path))stop("Installed W10 v2 eligibility prompt missing.")
  rules_sha<-sha(rules_path)
  instructions<-paste(readLines(rules_path,warn=FALSE,encoding="UTF-8"),collapse="\n")
  instructions<-paste0(instructions,
    "\n\nARCHIVED W04 SCREENING PROMPT (historical reference; ",
    "apply the clarified W10 B boundary above):\n",prompt,
    "\n\nFIRST-PASS OUTPUT: JSON observation_id, criteria A/B/C/D ",
    "each with status, rationale, quote, and overall. ",
    "Quotations must be exact substrings of the supplied extracted case.")
  validate<-function(z,item,case_text) {
    if(!identical(z$observation_id,item$observation_id)||!is.list(z$criteria)||
       !setequal(names(z$criteria),c("A","B","C","D")))
      stop("W10 response identity or criteria mismatch.")
    sts<-vapply(c("A","B","C","D"),function(k) {
      a<-z$criteria[[k]]
      if(!is.list(a)||!a$status%in%c("pass","fail","uncertain")||
         !is.character(a$rationale)||length(a$rationale)!=1L||
         !nzchar(trimws(a$rationale))||!is.character(a$quote)||
         length(a$quote)!=1L)stop("Invalid W10 criterion response.")
      if(nzchar(a$quote)&&!grepl(a$quote,case_text,fixed=TRUE))
        stop("W10 response quote absent from source case.")
      a$status
    },character(1))
    expected<-if(any(sts=="fail"))"ineligible" else if(all(sts=="pass"))"eligible" else "uncertain"
    if(!identical(z$overall,expected))stop("W10 overall decision inconsistent with criteria.")
    TRUE
  }
  for(item in cases[!ids%in%done]) {
    case_text<-jsonlite::toJSON(item$case,auto_unbox=TRUE,null="null")
    input<-jsonlite::toJSON(list(observation_id=item$observation_id,case=item$case),
                            auto_unbox=TRUE,null="null")
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",text=instructions))),
                 list(role="user",content=list(list(type="input_text",text=input)))),
      text=list(format=list(type="json_object")))
    success<-FALSE
    output_contract<-paste0(
      "Return exactly one JSON object with observation_id equal to ",
      jsonlite::toJSON(item$observation_id,auto_unbox=TRUE),
      ", criteria an OBJECT with exactly A, B, C, D as keys; ",
      "each criterion an object with status (pass/fail/uncertain), ",
      "rationale (string), quote (string); and overall ",
      "(eligible/ineligible/uncertain). No arrays or renamed fields.")
    body$input[[1L]]$content[[1L]]$text<-paste(instructions,
      output_contract,sep="\n\n")
    for(attempt in seq_len(3L)) {
      chunks<-character()
      result<-tryCatch({
        response<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        chunks<-unlist(lapply(response$output,function(x)
          unlist(lapply(x$content,function(y)
            if(identical(y$type,"output_text"))y$text else NULL),
            use.names=FALSE)),use.names=FALSE)
        if(!length(chunks))stop("Empty W10 model response.")
        z<-jsonlite::fromJSON(chunks[[1L]],simplifyVector=FALSE)
        validate(z,item,case_text)
        z
      },error=function(e)e)
      if(!inherits(result,"error")){success<-TRUE;break}
      message("[W10] ",item$observation_id," attempt ",attempt,
              "/3 failed: ",conditionMessage(result))
      if(exists("chunks",inherits=FALSE) && length(chunks)) {
        cat(as.character(jsonlite::toJSON(list(
          observation_id=item$observation_id,attempt=attempt,
          error=conditionMessage(result),raw_model_output=chunks[[1L]]),
          auto_unbox=TRUE,null="null")),"\n",
          file=file.path(run,"invalid_w10_responses.jsonl"),
          append=TRUE,sep="")
      }
      body$input[[1L]]$content[[1L]]$text<-paste(
        instructions,output_contract,
        paste0("Your preceding response was rejected: ",
               conditionMessage(result),
               ". Correct the OUTPUT STRUCTURE and identity only; ",
               "use the same substantive eligibility criteria. ",
               "Return a complete valid JSON object."),
        sep="\n\n")
      if(attempt<3L)Sys.sleep(2^attempt)
    }
    if(!success)stop("W10 stopped; completed responses preserved. Re-run to resume.")
    result$record_id<-item$record_id
    result$w09_source_sha256<-source_hash
    result$w04_prompt_sha256<-marker
    result$model<-model
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=ledger,append=TRUE,sep="")
    message("[W10] ",length(done)+1L,"/",length(ids)," ",item$observation_id,
            ": ",result$overall)
    done<-c(done,item$observation_id)
  }
  all<-lapply(Filter(nzchar,readLines(ledger,warn=FALSE)),
              jsonlite::fromJSON,simplifyVector=FALSE)
  df<-data.frame(observation_id=vapply(all,function(z)z$observation_id,character(1)),
                 record_id=vapply(all,function(z)z$record_id,character(1)),
                 decision=vapply(all,function(z)z$overall,character(1)),
                 stringsAsFactors=FALSE)
  for(k in c("A","B","C","D"))df[[paste0("criterion_",k)]]<-
    vapply(all,function(z)z$criteria[[k]]$status,character(1))
  report<-file.path(run,"w10_case_relevance_audit.csv")
  utils::write.csv(df,report,row.names=FALSE)
  manifest<-list(w09_registry_sha256=source_hash,w04_prompt_sha256=marker,w10_rules_sha256=rules_sha,
                 model=model,observations=length(ids),
                 decisions_sha256=sha(ledger),
                 counts=as.list(table(factor(df$decision,
                   levels=c("eligible","ineligible","uncertain")))))
  jsonlite::write_json(manifest,file.path(run,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[W10] Complete: ",report)
  invisible(run)
}
