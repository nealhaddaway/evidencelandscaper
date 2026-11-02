#' Extract full-text case data using a prompt and authoritative CSV
#'
#' Separate from W07 mechanism coding and the historical W09 v2 extraction.
#' Saves immutable input archives and resumes a JSONL case ledger. Only records
#' with manually verified source identity should be passed as record_ids.
#' @param w09 Prepared W09 case-positive retrieval folder.
#' @param prompt_file Short case extraction TXT prompt.
#' @param schema_file Case-analysis instructions CSV (without mechanism coding).
#' @param record_ids Source identity-verified IDs selected for extraction.
#' @param max_records Maximum unprocessed IDs to submit on this call, default 1.
#' @param model Luna model only.
#' @param max_pages Max PDF pages; 0 means all.
#' @param revalidate_only Recheck archived invalid responses without API calls.
#' @return Output directory, invisibly.
#' @export
run_w09_case_data_extraction <- function(w09,prompt_file,schema_file,record_ids,
  max_records=1L,model="gpt-5.6-luna",max_pages=0L,
  revalidate_only=FALSE) {
  if(!is.logical(revalidate_only)||length(revalidate_only)!=1L||is.na(revalidate_only))
    stop("revalidate_only must be TRUE or FALSE.")
  if(!identical(model,"gpt-5.6-luna"))
    stop("W09 case data extraction permits gpt-5.6-luna only.")
  if(!revalidate_only&&!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY is required.")
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  schema_file<-normalizePath(schema_file,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  txt<-paste(readLines(prompt_file,warn=FALSE,encoding="UTF-8"),collapse="\n")
  if(!nzchar(trimws(txt)))stop("Extraction prompt is empty.")
  if(grepl("independently handled field at CSV data row 11",txt,fixed=TRUE))
    stop("Remove the outdated CSV row-11 reference from your prompt.")
  spec<-utils::read.csv(schema_file,stringsAsFactors=FALSE,check.names=FALSE,
                         fileEncoding="UTF-8-BOM")
  required<-c("Question","Definition","Topics of interest","Answer_form","Response_compulsory")
  if(!all(required%in%names(spec)))stop("Case-analysis CSV missing required columns: ",
       paste(setdiff(required,names(spec)),collapse=", "))
  # Older CSVs may still contain the retired mechanism ontology row. Never
  # submit it to this separate case extraction workflow.
  obsolete<-grepl("mechanisms of the collective action",spec$Question,
                  ignore.case=TRUE)
  if(any(obsolete)) {
    message("[W09] Omitting ",sum(obsolete),
            " mechanism ontology row(s) from case-data extraction.")
    spec<-spec[!obsolete,,drop=FALSE]
  }
  if(!nrow(spec)||any(!spec$Response_compulsory%in%c("TRUE","FALSE"))||
     any(!spec$Answer_form%in%c("Fixed, one option",
       "Fixed, multiple options possible","Free text",
       "Free text, multiple options possible")))
    stop("Invalid CSV fields, compulsory flags, or answer forms.")
  if(any(!nzchar(trimws(spec$Definition))))
    stop("CSV contains empty field definitions.")
  spec$field_id<-sprintf("field_%03d",seq_len(nrow(spec)))
  schema_sha<-sha(schema_file)
  if(!length(record_ids)||anyNA(record_ids)||anyDuplicated(record_ids))
    stop("Supply unique manually identity-verified record_ids.")
  queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                          stringsAsFactors=FALSE)
  if(anyDuplicated(queue$record_id)||any(!record_ids%in%queue$record_id))
    stop("record_ids must be uniquely present in W09 retrieval queue.")
  if(length(max_records)!=1L||is.na(max_records)||max_records<1L||
     max_records!=as.integer(max_records)||length(max_pages)!=1L||
     is.na(max_pages)||max_pages<0L||max_pages!=as.integer(max_pages))
    stop("max_records must be positive; max_pages must be nonnegative.")
  dest<-file.path(w09,"case_data_extraction",paste0(
    "csv-",substr(schema_sha,1,12),"-prompt-",substr(sha(prompt_file),1,12),
    "-model-",model))
  manpath<-file.path(dest,"manifest.json")
  ledger<-file.path(dest,"case_data.jsonl")
  source_selection<-file.path(dest,"source_documents.jsonl")
  manifest<-list(schema="ostara-w09-csv-case-data-extraction-v1",
    w09=w09,prompt_sha256=sha(prompt_file),csv_sha256=schema_sha,
    queue_sha256=sha(file.path(w09,"retrieval_queue.csv")),
    model=model,field_ids=as.list(spec$field_id),
    csv_questions=as.list(spec$Question))
  if(file.exists(manpath)) {
    old<-jsonlite::fromJSON(manpath,simplifyVector=FALSE)
    for(k in names(manifest))if(!identical(old[[k]],manifest[[k]]))
      stop("Existing W09 case-extraction provenance mismatch: ",k)
    if(!identical(sha(file.path(dest,"prompt.txt")),manifest$prompt_sha256)||
       !identical(sha(file.path(dest,"case_schema.csv")),manifest$csv_sha256))
      stop("Archived W09 case extraction inputs changed.")
  } else {
    if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Existing W09 extraction output directory has no manifest.")
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(prompt_file,file.path(dest,"prompt.txt"))||
       !file.copy(schema_file,file.path(dest,"case_schema.csv")))
      stop("Cannot archive extraction prompt and CSV.")
    jsonlite::write_json(manifest,manpath,auto_unbox=TRUE,pretty=TRUE)
    utils::write.csv(spec,file.path(dest,"field_mapping.csv"),row.names=FALSE)
  }
  readjl<-function(p) {
    if(!file.exists(p))return(list())
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  done<-readjl(ledger)
  done_ids<-vapply(done,function(x)x$record_id,character(1))
  if(anyDuplicated(done_ids)||any(!done_ids%in%queue$record_id))
    stop("Existing case-data checkpoint contains invalid or duplicated IDs.")
  # Validate a fixed JSON envelope, with CSV-defined field identifiers.
  # Each field has the same record-independent evidence object shape.
  check_result<-function(x,id,source_text){
    if(!identical(x$record_id,id)||!is.list(x$cases)||
       !is.character(x$publication_extraction_status)||
       length(x$publication_extraction_status)!=1L||
       is.na(x$publication_extraction_status)||
       !x$publication_extraction_status%in%c("cases_identified","no_qualifying_case"))
      stop("Invalid publication-level W09 case extraction result.")
    if((length(x$cases)>0L)!=identical(x$publication_extraction_status,"cases_identified"))
      stop("Inconsistent publication case count/status.")
    case_ids<-vapply(x$cases,function(z)z$case_id,character(1))
    if(anyDuplicated(case_ids))stop("Duplicate case IDs.")
    fields_expected<-as.character(spec$field_id)
    for(j in seq_along(x$cases)) {
      case<-x$cases[[j]]
      if(!identical(case$case_id,paste0(id,"::case_",sprintf("%03d",j))))
        stop("Case IDs must be sequential and stable.")
      if(is.null(case$assessment_reason)||!is.character(case$assessment_reason)||
         !nzchar(trimws(case$assessment_reason)))
        stop("Missing case eligibility reason.")
      if(!is.list(case$fields)||!setequal(names(case$fields),fields_expected))
        stop("Case response has missing or unrecognised CSV fields.")
      for(i in seq_len(nrow(spec))) {
        key<-spec$field_id[[i]]
        f<-case$fields[[key]]
        form<-spec$Answer_form[[i]]
        if(!is.list(f)||
           !is.character(f$reporting_status)||
           length(f$reporting_status)!=1L||
           is.na(f$reporting_status)||
           !f$reporting_status%in%c("reported","not_reported","explicitly_none"))
          stop("Case ",j," ",key,": missing or invalid reporting_status.")
        multi<-grepl("multiple",form,fixed=TRUE)
        if(multi) {
          if(!is.list(f$values)&&!is.character(f$values))
            stop("Multi-response field must have a values array: ",key)
          vals<-unlist(f$values,use.names=FALSE)
        } else {
          if(is.null(f$value))vals<-character() else {
            if(!is.character(f$value)||length(f$value)!=1L)
              stop("Single response value must be string or null: ",key)
            vals<-f$value
          }
        }
        # Validate finite fixed vocabularies from the CSV, without inventing
        # a closed vocabulary for countries or commodities.
        q<-tolower(trimws(spec$Question[[i]]))
        allowed<-NULL
        if(grepl("^what is the status",q))
          allowed<-c("active","paused","completed","abandoned")
        if(grepl("^what is the level at which",q))
          allowed<-c("private sector","place- or landscape-based","national",
            "inter-governmental or multinational","systemic or other")
        if(startsWith(q,"what is/are the shared interest"))
          allowed<-c("shared risks","shared costs","shared benefits",
                     "shared value creation")
        if(identical(q,"supply chain stages"))
          allowed<-c("production","aggregation","processing","transport",
                     "wholesale","retail")
        if(!is.null(allowed)&&any(!vals%in%allowed))
          stop("Unrecognised fixed-choice value for ",key,": ",
               paste(setdiff(vals,allowed),collapse=", "))
        if(grepl("^where did the case take place \\(country\\)",q)&&
           any(!grepl("^[A-Z]{3}$",vals)))
          stop("Country identifiers must be ISO3 upper-case codes: ",key)
        if(identical(f$reporting_status,"not_reported")&&length(vals))
          stop("Missing response must not contain invented values: ",key)
        if(identical(f$reporting_status,"reported")&&!length(vals))
          stop("Reported response requires substantive values: ",key)
        if(!is.list(f$evidence))stop("Case ",j," ",key,
                                      ": evidence must be an array.")
        if(identical(f$reporting_status,"reported")&&!length(f$evidence))
          stop("Reported field lacks supporting evidence: ",key)
        for(e in f$evidence){
          if(!is.character(e$quote)||length(e$quote)!=1L||
             !nzchar(trimws(e$quote))||
             !is.character(e$anchor)||length(e$anchor)!=1L||
             !grepl("^\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]$",e$anchor)||
             !grepl(e$anchor,source_text,fixed=TRUE))
            stop("Evidence quote/anchor missing from supplied text: ",key)
          # Verify the quotation within the cited PAGE/SECTION/TEXT BLOCK,
          # tolerating layout-related whitespace and capitalisation changes only.
          at<-regexpr(e$anchor,source_text,fixed=TRUE)[[1L]]
          after<-substring(source_text,at+nchar(e$anchor))
          next_anchor<-regexpr("\\[(?:PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",
                               after,perl=TRUE)[[1L]]
          segment<-if(next_anchor>0L)substr(after,1L,next_anchor-1L) else after
          # Ignore differences in punctuation, whitespace and capitalisation.
          # Untraceable quotations are flagged for human review, not silently
          # treated as verified. Non-evidence schema rules remain strict.
          compact<-function(z) {
            z<-iconv(z,from="",to="ASCII//TRANSLIT",sub="")
            z<-tolower(z)
            trimws(gsub("[^[:alnum:]]+"," ",z))
          }
          quote<-compact(e$quote)
          in_anchor<-grepl(quote,compact(segment),fixed=TRUE)
          if(!in_anchor) {
            if(!grepl(quote,compact(source_text),fixed=TRUE)) {
              warning("W09 ",id," ",key,": quotation not found in full text; ",
                      "requires manual evidence review",call.=FALSE)
            } else {
              warning("W09 ",id," ",key,": evidence matches full text ",
                      "but not cited anchor ",e$anchor,call.=FALSE)
            }
          }
        }
      }
    }
    invisible(TRUE)
  }
  # Field specifications are sent once per model request, not embedded as
  # repeated prose in the archived short prompt.
  defs<-paste(utils::capture.output(utils::write.csv(spec,
    row.names=FALSE,na="")),collapse="\n")
  rule<-paste0(
    "\n\nOUTPUT CONTRACT (stable keys from the accompanying CSV): ",
    "Return only JSON with record_id, publication_extraction_status ",
    "('cases_identified' or 'no_qualifying_case'), and cases. ",
    "Each case has case_id, assessment_reason, case_boundary_uncertainty ",
    "(string or null), and fields. Include exactly every field_id in the CSV. ",
    "For one-option/free-text fields, return ",
    "{value:string_or_null,reporting_status:'reported|not_reported|explicitly_none',",
    "evidence:[{quote:string,anchor:string}]}. ",
    "For multiple-option or multiple-free-text fields, return ",
    "{values:[string],reporting_status:'reported|not_reported|explicitly_none',",
    "evidence:[{quote:string,anchor:string}]}. ",
    "Each distinct factor/barrier becomes a separate string entry in values; ",
    "explain explicit non-influence in the entry, not as an invented factor. ",
    "Use null/empty arrays with not_reported, never fabricate compulsory data. ",
    "Use only permitted fixed choices stated in each field definition; ",
    "country codes should be ISO3. All reported substantive fields require ",
    "verbatim quotes and the exact PAGE/SECTION/TEXT BLOCK anchor. ",
    "Use case_id <record_id>::case_001 and sequential numbering. ",
    "No cases: cases=[] with no_qualifying_case. ",
    "Do not output mechanism classifications.\n\nAUTHORITATIVE FIELD CSV:\n",defs)
  pending<-record_ids[!record_ids%in%done_ids]
  pending<-head(pending,as.integer(max_records))
  message("[W09 case data] ",length(done_ids)," checkpointed; ",
          length(pending)," selected for this call.")
  for(id in pending) {
    payload<-w09_text_payload(w09,id,max_pages=max_pages)
    # Hash-bound source selections ensure a resume never silently substitutes
    # a different full text for a completed ID.
    old_sources<-readjl(source_selection)
    known<-Filter(function(z)identical(z$record_id,id),old_sources)
    if(length(known)>1L)stop("Duplicate source selections: ",id)
    if(length(known)==1L&&
       (!identical(known[[1L]]$sha256,payload$sha256)||
        !identical(known[[1L]]$path,payload$path)))
      stop("Source full text changed since earlier attempt: ",id)
    if(!length(known)) {
      cat(jsonlite::toJSON(list(record_id=id,path=payload$path,
        sha256=payload$sha256,format=payload$format),
        auto_unbox=TRUE,null="null"),"\n",
        file=source_selection,append=TRUE,sep="")
    }
    # Revalidate archived model responses before any paid request.
    history<-readjl(file.path(dest,"invalid_responses.jsonl"))
    history<-Filter(function(z)identical(z$record_id,id)&&
      is.character(z$raw)&&length(z$raw)==1L,history)
    # Evaluate all archived responses and prefer the fewest evidence flags.
    # Schema and field-value rules remain strict. Evidence exceptions are
    # explicitly audited and mean the checkpoint requires human review.
    candidates<-list()
    archived_issues<-character()
    for(k in seq_along(history)) {
      flags<-character()
      issue<-NULL
      old_obj<-tryCatch({
        candidate<-jsonlite::fromJSON(history[[k]]$raw,simplifyVector=FALSE)
        withCallingHandlers(check_result(candidate,id,payload$text),
          warning=function(w) {
            flags<<-c(flags,conditionMessage(w))
            invokeRestart("muffleWarning")
          })
        candidate
      },error=function(e) {
        issue<<-conditionMessage(e)
        NULL
      })
      if(!is.null(issue))
        archived_issues<-c(archived_issues,
          paste0("archived_response_",k,": ",issue))
      if(!is.null(old_obj))
        candidates[[length(candidates)+1L]]<-list(
          index=k,obj=old_obj,flags=flags)
    }
    if(length(candidates)) {
      best<-candidates[[which.min(vapply(candidates,
        function(z)length(z$flags),integer(1)))]]
      cat(jsonlite::toJSON(best$obj,auto_unbox=TRUE,null="null"),"\n",
          file=ledger,append=TRUE,sep="")
      cat(jsonlite::toJSON(list(record_id=id,
          archived_response_index=best$index,
          recovery="offline_revalidation",
          evidence_review_required=length(best$flags)>0L,
          evidence_flags=as.list(best$flags),
          validation_rule="relaxed_evidence_with_manual_review_flags"),
          auto_unbox=TRUE,null="null"),"\n",
          file=file.path(dest,"revalidation_audit.jsonl"),
          append=TRUE,sep="")
      message("[W09 case data] Recovered archived response ",best$index,
              " for ",id," (",length(best$obj$cases)," cases; ",
              length(best$flags)," evidence review flags)")
      next
    }
    if(revalidate_only) {
      cat(jsonlite::toJSON(list(record_id=id,
          status="no_archived_response_valid",
          issues=as.list(archived_issues)),
          auto_unbox=TRUE,null="null"),"\n",
          file=file.path(dest,"offline_revalidation_failures.jsonl"),
          append=TRUE,sep="")
      message("[W09 case data] No archived response passed revalidation for ",
              id,"; no API request made. ",
              if(length(archived_issues))
                paste(archived_issues,collapse=" | ")
              else "No archived responses.")
      next
    }
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(
        list(role="system",content=list(list(type="input_text",
             text=paste(txt,rule)))),
        list(role="user",content=list(list(type="input_text",
          text=jsonlite::toJSON(list(record_id=id,
            fulltext=payload$text,source_format=payload$format,
            source_sha256=payload$sha256),auto_unbox=TRUE,null="null"))))),
      text=list(format=list(type="json_object")))
    success<-FALSE
    for(attempt in seq_len(3L)) {
      resp<-NULL
      for(transport in seq_len(3L)) {
        out<-tryCatch(httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_options(http_version=2L)|> # libcurl HTTP/1.1, avoid HTTP/2 framing errors
          httr2::req_timeout(300)|>httr2::req_perform(),error=function(e)e)
        if(!inherits(out,"error")){resp<-out;break}
        msg<-conditionMessage(out)
        if(!grepl("timed? ?out|timeout|connection reset|failed to connect|HTTP2 framing|HTTP/2|curl_fetch_memory|stream.*reset|SSL.*read",
                  msg,ignore.case=TRUE)||transport==3L)
          stop("[W09 case data] API transport failure; checkpoint preserved: ",msg)
        message("[W09 case data] ",id,": transient request failure, retry ",
                transport+1L,"/3. Previous request may have incurred usage.")
        Sys.sleep(c(5,15)[[transport]])
      }
      response<-httr2::resp_body_json(resp,simplifyVector=FALSE)
      parts<-unlist(lapply(response$output,function(z)
        unlist(lapply(z$content,function(p)
          if(identical(p$type,"output_text"))p$text else NULL),
          use.names=FALSE)),use.names=FALSE)
      issue<-NULL
      obj<-tryCatch({
        if(!length(parts))stop("Missing model JSON text.")
        x<-jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE)
        check_result(x,id,payload$text)
        x
      },error=function(e){issue<<-conditionMessage(e);NULL})
      if(!is.null(obj)) {
        cat(jsonlite::toJSON(obj,auto_unbox=TRUE,null="null"),"\n",
            file=ledger,append=TRUE,sep="")
        message("[W09 case data] Saved ",id," (",length(obj$cases)," cases)")
        success<-TRUE;break
      }
      log<-file.path(dest,"invalid_responses.jsonl")
      cat(jsonlite::toJSON(list(record_id=id,attempt=attempt,issue=issue,
        response_id=response$id,raw=if(length(parts))parts[[1L]] else NULL),
        auto_unbox=TRUE,null="null"),"\n",file=log,append=TRUE,sep="")
      if(attempt<3L) {
        body$input[[1L]]$content[[1L]]$text<-paste(txt,rule,
          "RETRY: The previous response failed validation:",issue,
          "Correct field shapes and quote/anchor fidelity. Do not invent data.")
      }
    }
    if(!success) {
      cat(jsonlite::toJSON(list(record_id=id,
          status="failed_after_three_attempts",
          reason="model_validation",
          input_sha256=payload$sha256),
          auto_unbox=TRUE,null="null"),"\n",
          file=file.path(dest,"batch_failures.jsonl"),
          append=TRUE,sep="")
      warning("[W09 case data] Invalid model output after three Luna ",
              "attempts for ",id,". Recorded in batch_failures.jsonl; ",
              "continuing to next publication.",call.=FALSE)
    }
  }
  invisible(dest)
}
