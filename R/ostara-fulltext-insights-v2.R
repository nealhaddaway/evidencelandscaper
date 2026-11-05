#' Validate the revised four-category W09 full-text insights response
#'
#' Structural errors stop processing; quotation or anchor discrepancies are
#' retained as explicit human-review flags, never silently verified.
.w09_validate_insights_v2 <- function(z,id,fulltext,codes) {
  one <- function(x) is.character(x)&&length(x)==1L&&!is.na(x)
  required <- c("record_id","status","assessment_reason","insights")
  if(!is.list(z)||!setequal(names(z),required)||
     !identical(z$record_id,id)||!is.list(z$insights)||
     !one(z$status)||!z$status%in%c("insights_identified","no_relevant_insights","unclear")||
     !one(z$assessment_reason)||!nzchar(trimws(z$assessment_reason)))
    stop("Invalid W09 insights-v2 publication envelope")
  if((length(z$insights)>0L)!=identical(z$status,"insights_identified"))
    stop("W09 insights-v2 status/count mismatch")
  categories<-c("empirical_finding","broader_lesson","recommendation","future_prospect")
  bases<-c("original_empirical","literature_synthesis","author_interpretation",
           "recommendation","speculative_proposal")
  scopes<-c("multiple_cases","generalised_single_case","literature_wide",
            "conceptual","not_specified","not_applicable")
  flags<-list()
  for(i in seq_along(z$insights)) {
    x<-z$insights[[i]]
    fields<-c("insight_id","category","insight_summary","insight_subtype",
              "evidence_basis","support_scope","context","mechanism_ids",
              "source_quote","source_anchor","caveats")
    if(!is.list(x)||!setequal(names(x),fields))
      stop("W09 insights-v2 insight keys do not match revised prompt: ",i)
    text_fields<-setdiff(fields,"mechanism_ids")
    if(any(!vapply(x[text_fields],one,logical(1)))||
       any(!nzchar(trimws(unlist(x[c("insight_id","insight_summary",
                                      "insight_subtype","source_quote",
                                      "source_anchor")],use.names=FALSE)))))
      stop("W09 insights-v2 required insight text missing: ",i)
    if(!identical(x$insight_id,sprintf("%s::insight_%03d",id,i)))
      stop("W09 insights-v2 insight_id must be sequential and publication-scoped: ",i)
    if(!x$category%in%categories||!x$evidence_basis%in%bases||
       !x$support_scope%in%scopes)
      stop("W09 insights-v2 invalid category, evidence basis, or support scope: ",i)
    if(!is.list(x$mechanism_ids)&&!is.character(x$mechanism_ids))
      stop("W09 insights-v2 mechanism_ids must be an array: ",i)
    mids<-unlist(x$mechanism_ids,use.names=FALSE)
    if(!is.character(mids)||anyDuplicated(mids)||any(!mids%in%codes))
      stop("W09 insights-v2 invalid or duplicate approved mechanism ID: ",i)
    causes<-character()
    if(!grepl("^\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]$",x$source_anchor))
      causes<-c(causes,"invalid_source_anchor")
    at<-regexpr(x$source_anchor,fulltext,fixed=TRUE)[[1L]]
    if(at<1L)causes<-c(causes,"anchor_not_in_input")
    if(!grepl(x$source_quote,fulltext,fixed=TRUE))
      causes<-c(causes,"quote_not_exact_in_input")
    if(at>0L) {
      after<-substring(fulltext,at+nchar(x$source_anchor))
      next_at<-regexpr("\\[(?:PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",after,perl=TRUE)[[1L]]
      segment<-if(next_at>0L)substr(after,1L,next_at-1L) else after
      if(!grepl(x$source_quote,segment,fixed=TRUE))
        causes<-c(causes,"quote_not_in_cited_anchor")
    }
    if(length(causes))
      flags[[length(flags)+1L]]<-list(
        insight_id=x$insight_id,reasons=as.list(unique(causes)),
        source_quote=x$source_quote,source_anchor=x$source_anchor)
  }
  flags
}

#' Extract revised W09 Insights from all downloaded full texts in one W09 queue
#'
#' Independence: the model receives only record_id, fulltext and the explicitly
#' supplied approved mechanism ontology. W08, W07 classifications, case
#' extraction and mechanism-inventory results are NEVER supplied or consulted.
#' Selecting all downloaded W09 queue records does not use W08 case status.
#'
#' @param w09 Directory with retrieval_queue.csv and pdfs/ or structured/ files.
#' @param prompt_file Explicit revised Insights prompt TXT.
#' @param ontology_file Explicit approved 30-code ontology CSV.
#' @param record_ids Optional subset from W09 queue; NULL uses all downloaded.
#' @param max_records Maximum new assessments; 0 means all.
#' @param model OpenAI Luna model.
#' @return Versioned output directory, invisibly.
#' @export
extract_ostara_fulltext_insights_v2 <- function(
    w09,prompt_file,ontology_file,record_ids=NULL,max_records=0L,
    model="gpt-5.6-luna") {
  if(!identical(model,"gpt-5.6-luna"))stop("Only gpt-5.6-luna is supported.")
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer (0=all).")
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    if(!file.exists(p))return(list())
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  prompt<-paste(readLines(prompt_file,warn=FALSE,encoding="UTF-8"),collapse="\n")
  if(!nzchar(trimws(prompt)))stop("Empty insights prompt.")
  ontology<-utils::read.csv(ontology_file,stringsAsFactors=FALSE,
                            fileEncoding="UTF-8-BOM",check.names=FALSE)
  expected<-c("mechanism_id","collective_action_type",
              "high_level_mechanism","fine_level_mechanism","definition")
  if(!identical(names(ontology),expected)||nrow(ontology)!=30L||
     anyNA(ontology)||any(!nzchar(trimws(as.matrix(ontology))))||
     anyDuplicated(ontology$mechanism_id))
    stop("The approved ontology must contain 30 complete five-column definitions.")
  codes<-as.character(ontology$mechanism_id)
  queue_path<-file.path(w09,"retrieval_queue.csv")
  queue<-utils::read.csv(queue_path,stringsAsFactors=FALSE)
  if(!"record_id"%in%names(queue)||anyDuplicated(queue$record_id)||
     anyNA(queue$record_id))stop("Invalid W09 retrieval queue.")
  ids<-as.character(queue$record_id)
  if(!is.null(record_ids)) {
    if(!is.character(record_ids)||anyNA(record_ids)||
       anyDuplicated(record_ids)||any(!record_ids%in%ids))
      stop("record_ids not unique or not in W09 queue.")
    ids<-record_ids
  }
  ids<-sort(ids)
  get_source<-function(id) {
    candidates<-c(file.path(w09,"structured",paste0(id,".xml")),
                  file.path(w09,"structured",paste0(id,".json")),
                  file.path(w09,"pdfs",paste0(id,".pdf")))
    present<-candidates[file.exists(candidates)]
    if(!length(present))"" else normalizePath(present[[1L]],winslash="/",mustWork=TRUE)
  }
  source_paths<-vapply(ids,get_source,character(1))
  source_hashes<-vapply(source_paths,function(x)if(nzchar(x))sha(x) else "",character(1))
  inventory<-data.frame(record_id=ids,source_path=unname(source_paths),
    source_sha256=unname(source_hashes),
    available=nzchar(source_paths),stringsAsFactors=FALSE)
  available<-ids[nzchar(source_paths)]
  if(!length(available))stop("No downloaded full texts in this W09 queue.")
  key<-substr(digest::digest(c(sha(queue_path),sha(prompt_file),
      sha(ontology_file),ids,source_hashes,model),algo="sha256"),1L,12L)
  dest<-file.path(w09,"fulltext_insights_v2",paste0("v2-",key))
  manpath<-file.path(dest,"manifest.json")
  manifest<-list(schema="ostara-fulltext-insights-v2",
    source_w09=w09,queue_sha256=sha(queue_path),
    prompt_sha256=sha(prompt_file),ontology_sha256=sha(ontology_file),
    model=model,selected_ids=as.list(ids),available_ids=as.list(available),
    source_sha256=as.list(unname(source_hashes)),
    selection_rule="all_downloaded_fulltexts_independent_of_W08_and_W09_case_coding")
  if(file.exists(manpath)) {
    old<-jsonlite::fromJSON(manpath,simplifyVector=FALSE)
    if(!identical(old,manifest))stop("Insights-v2 checkpoint provenance mismatch.")
  } else {
    if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Insights-v2 destination exists without manifest.")
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(prompt_file,file.path(dest,"prompt.txt"))||
       !file.copy(ontology_file,file.path(dest,"ontology.csv")))
      stop("Could not archive approved inputs.")
    utils::write.csv(inventory,file.path(dest,"source_inventory.csv"),row.names=FALSE)
    jsonlite::write_json(manifest,manpath,pretty=TRUE,auto_unbox=TRUE)
  }
  if(!identical(sha(file.path(dest,"prompt.txt")),sha(prompt_file))||
     !identical(sha(file.path(dest,"ontology.csv")),sha(ontology_file)))
    stop("Archived Insights-v2 prompt/ontology checksums changed.")
  results<-file.path(dest,"insights_assessments.jsonl")
  done<-readjl(results)
  done_ids<-vapply(done,function(x)x$record_id,character(1))
  if(anyDuplicated(done_ids)||any(!done_ids%in%available))
    stop("Insights-v2 checkpoint record IDs invalid.")
  todo<-setdiff(available,done_ids)
  if(max_records>0L)todo<-head(todo,max_records)
  if(length(todo)&&!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY required for ",length(todo)," pending full texts.")
  ontology_text<-paste(utils::capture.output(utils::write.csv(
    ontology,row.names=FALSE)),collapse="\n")
  contract<-paste0(
    "\n\nWORKFLOW OUTPUT CONTRACT: Return JSON top-level keys exactly ",
    "record_id,status,assessment_reason,insights. ",
    "status = insights_identified if insights is nonempty; ",
    "no_relevant_insights if none found in sufficient text; ",
    "unclear if available text cannot support reliable assessment. ",
    "For each insight output exactly: insight_id,category,insight_summary,",
    "insight_subtype,evidence_basis,support_scope,context,mechanism_ids,",
    "source_quote,source_anchor,caveats. ",
    "insight_id must be <record_id>::insight_001 and sequential. ",
    "category: empirical_finding|broader_lesson|recommendation|future_prospect. ",
    "evidence_basis: original_empirical|literature_synthesis|author_interpretation|",
    "recommendation|speculative_proposal. ",
    "support_scope: multiple_cases|generalised_single_case|literature_wide|",
    "conceptual|not_specified|not_applicable. ",
    "context and caveats may be empty strings, mechanism_ids is an array ",
    "of supplied approved codes or []. Never fabricate quotes or anchors.")
  instructions<-paste0(prompt,contract)
  failures<-file.path(dest,"failed_attempts.jsonl")
  for(id in todo) {
    payload<-tryCatch(w09_text_payload(w09,id,max_pages=0L),
                      error=function(e)e)
    if(inherits(payload,"error")) {
      cat(as.character(jsonlite::toJSON(
        list(record_id=id,status="unprocessed_fulltext",reason=conditionMessage(payload)),
        auto_unbox=TRUE,null="null")),"\n",
        file=file.path(dest,"unprocessed_fulltexts.jsonl"),append=TRUE,sep="")
      message("[FT Insights v2] Skipped unreadable/oversized ",id,
              ": ",conditionMessage(payload))
      next
    }
    expected_sha<-unname(source_hashes[match(id,ids)])
    if(!identical(sha(payload$path),expected_sha))
      stop("Full text changed during Insights extraction: ",id)
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(
        list(role="system",content=list(list(type="input_text",text=instructions))),
        list(role="user",content=list(list(type="input_text",text=as.character(
          jsonlite::toJSON(list(record_id=id,fulltext=payload$text,
            approved_ontology=ontology_text),auto_unbox=TRUE,null="null")))))),
      text=list(format=list(type="json_object")))
    accepted<-NULL
    for(attempt in seq_len(3L)) {
      out<-tryCatch({
        response<-httr2::request("https://api.openai.com/v1/responses") |>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
          httr2::req_body_json(body,auto_unbox=TRUE) |>
          httr2::req_timeout(300) |>
          httr2::req_perform() |>
          httr2::resp_body_json(simplifyVector=FALSE)
        parts<-unlist(lapply(response$output,function(x)
          lapply(x$content,function(p)
            if(identical(p$type,"output_text"))p$text else NULL)),
          use.names=FALSE)
        if(!length(parts))stop("No model output text.")
        z<-jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE)
        flags<-.w09_validate_insights_v2(z,id,payload$text,codes)
        z$evidence_review_required<-length(flags)>0L
        z$evidence_review_flags<-flags
        z
      },error=function(e)e)
      if(!inherits(out,"error")) {accepted<-out;break}
      cat(as.character(jsonlite::toJSON(
        list(record_id=id,attempt=attempt,error=conditionMessage(out)),
        auto_unbox=TRUE,null="null")),"\n",file=failures,append=TRUE,sep="")
      message("[FT Insights v2] ",id," attempt ",attempt,
              "/3: ",conditionMessage(out))
    }
    if(is.null(accepted))next
    accepted$provenance<-list(source_path=payload$path,
      source_sha256=payload$sha256,source_format=payload$format,
      prompt_sha256=sha(prompt_file),ontology_sha256=sha(ontology_file),
      model=model,independent_of_prior_coding=TRUE,
      source_identity_verified=FALSE)
    cat(as.character(jsonlite::toJSON(accepted,auto_unbox=TRUE,null="null")),
        "\n",file=results,append=TRUE,sep="")
    message("[FT Insights v2] ",id,": ",length(accepted$insights),
            " insight(s); flags=",length(accepted$evidence_review_flags))
  }
  invisible(dest)
}
