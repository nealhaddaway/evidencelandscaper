#' Validate an independent full-text mechanism inventory and case sweep.
#'
#' This validator intentionally never consults W09 case extraction or eligible
#' case IDs. Existing legacy "implemented_case" is a mechanism status, not a
#' reference to a separately extracted case.
.w09_validate_independent_mechanism_sweep <- function(z,record_id,approved_codes,
                                                       fulltext) {
  sc<-function(v) {
    if(is.null(v)||!length(v))return("")
    paste(as.character(unlist(v,use.names=FALSE)),collapse="; ")
  }
  quoted<-function(x,what) {
    quote<-sc(x$source_quote);anchor<-sc(x$source_anchor)
    if(!nzchar(quote)||!nzchar(anchor)||
       !grepl(quote,fulltext,fixed=TRUE)||
       !grepl(anchor,fulltext,fixed=TRUE)||
       !grepl("^\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]$",anchor))
      stop(what,": missing or non-verbatim full-text evidence/anchor")
    invisible(TRUE)
  }
  if(!identical(sc(z$record_id),record_id)||
     !is.list(z$mechanism_inventory)||
     !is.list(z$new_novelty)||
     !is.list(z$case_presence_assessment))
    stop("Incorrect independent full-text output contract")
  mechanisms<-z$mechanism_inventory
  actual<-vapply(mechanisms,function(x)sc(x$mechanism_id),character(1))
  if(anyDuplicated(actual)||!setequal(actual,approved_codes))
    stop("Every approved ontology mechanism must appear exactly once")
  statuses<-c("implemented_case","mentioned_only","proposed_only",
              "not_identified","unclear")
  for(m in mechanisms) {
    if(!is.null(m$case_ids))
      stop("Independent mechanism inventory must not have case_ids")
    if(!sc(m$status)%in%statuses)
      stop("Invalid mechanism inventory status")
    if(sc(m$status)!="not_identified")quoted(m,"Mechanism inventory")
  }
  for(n in z$new_novelty) {
    if(!is.null(n$case_ids))
      stop("Independent novelty must not have case_ids")
    if(!sc(n$support_status)%in%c("implemented","proposed","unclear")||
       !nzchar(sc(n$proposed_label))||!nzchar(sc(n$working_definition)))
      stop("Invalid novel mechanism proposal")
    quoted(n,"New mechanism")
  }
  sweep<-z$case_presence_assessment
  status<-sc(sweep$case_status)
  if(!status%in%c("case_present","no_case","unclear")||
     !is.list(sweep$candidate_cases)||!nzchar(sc(sweep$assessment_reason)))
    stop("Invalid independent full-text case presence assessment")
  count<-sweep$candidate_case_count
  if(status=="unclear") {
    if(!is.null(count))stop("Unclear full-text case count must be null")
  } else if(!is.numeric(count)||length(count)!=1L||is.na(count)||
            count!=as.integer(count)||count<0L||
            (status=="no_case"&&count!=0L)||
            (status=="case_present"&&count<1L)||
            length(sweep$candidate_cases)!=count)
    stop("Full-text case-presence count inconsistent with classification")
  if(!identical(sc(sweep$w08_comparison),"not_provided"))
    stop("W08 comparison is unavailable without a validated joined W08 source")
  if(!is.logical(sweep$review_required)||
     length(sweep$review_required)!=1L||is.na(sweep$review_required)||
     !identical(sweep$review_required,status=="unclear"))
    stop("Inconsistent case review_required flag")
  for(ca in sweep$candidate_cases) {
    if(!nzchar(sc(ca$candidate_label))||
       !nzchar(sc(ca$case_description))||
       !nzchar(sc(ca$eligibility_reason)))
      stop("Incomplete provisional case candidate")
    quoted(ca,"Case presence candidate")
  }
  invisible(TRUE)
}

#' Code all available included full texts for mechanisms and case presence
#'
#' Completely independent of case-extraction registries or W09 case IDs.
#' Publications are selected from the W07 included coding layer and the W09
#' retrieval queue, then actual available XML/JSON/PDF files. Missing full text
#' and records absent from W09 are recorded, not silently deemed negative.
#' Results are checkpointed in an immutable versioned directory. Human review
#' is required before case/insights routing and ontology changes.
#'
#' @param w09 W09 retrieval run directory (with retrieval_queue.csv).
#' @param w07 Completed W07 run directory (ontology.csv and coding.jsonl).
#' @param record_ids Optional specific included W07 record IDs; default all W07.
#' @param max_records Maximum NEW LLM assessments; 0 means all available.
#' @param model Luna model.
#' @param output_dir Optional new output base directory.
#' @return Versioned mechanism inventory run directory, invisibly.
#' @export
extract_ostara_all_fulltext_mechanisms <- function(
    w09,w07,record_ids=NULL,max_records=0L,model="gpt-5.6-luna",
    output_dir=NULL) {
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  w07<-normalizePath(w07,winslash="/",mustWork=TRUE)
  if(!grepl("-luna$",model)||length(model)!=1L||is.na(model))
    stop("Only a Luna model is permitted for new full-text assessments")
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer")
  files<-c(queue=file.path(w09,"retrieval_queue.csv"),
           coding=file.path(w07,"coding.jsonl"),
           ontology=file.path(w07,"ontology.csv"))
  if(any(!file.exists(files)))stop("Missing W09 queue or W07 archive: ",
                                   paste(files[!file.exists(files)],collapse=", "))
  prompt<-system.file("w09","fulltext_mechanisms_supplement_v1.txt",
                      package="evidencelandscaper")
  criteria<-system.file("w08","case_identification_prompt_v3.txt",
                        package="evidencelandscaper")
  if(!nzchar(prompt)||!nzchar(criteria))
    stop("Package is missing mechanism sweep prompt or W08 case criteria")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  lines<-function(p)readLines(p,warn=FALSE,encoding="UTF-8")
  readjl<-function(p)lapply(Filter(nzchar,lines(p)),
                              jsonlite::fromJSON,simplifyVector=FALSE)
  ont<-utils::read.csv(files[["ontology"]],stringsAsFactors=FALSE,
                        check.names=FALSE,fileEncoding="UTF-8-BOM")
  if(!"mechanism_id"%in%names(ont)||anyDuplicated(ont$mechanism_id))
    stop("Invalid archived W07 mechanism ontology")
  codes<-as.character(ont$mechanism_id)
  w07rows<-readjl(files[["coding"]])
  w07ids<-vapply(w07rows,function(z)as.character(z$record_id),character(1))
  if(!length(w07ids)||anyDuplicated(w07ids)||anyNA(w07ids))
    stop("W07 record IDs are missing or duplicated")
  w07index<-setNames(w07rows,w07ids)
  if(is.null(record_ids))record_ids<-w07ids
  if(!is.character(record_ids)||!length(record_ids)||anyNA(record_ids)||
     anyDuplicated(record_ids)||any(!record_ids%in%w07ids))
    stop("record_ids must be unique included W07 publication IDs")
  record_ids<-sort(record_ids)
  queue<-utils::read.csv(files[["queue"]],stringsAsFactors=FALSE)
  if(!"record_id"%in%names(queue)||anyDuplicated(queue$record_id))
    stop("Invalid or duplicate W09 retrieval queue record IDs")
  in_queue<-record_ids%in%as.character(queue$record_id)
  choose_source<-function(id) {
    paths<-c(file.path(w09,"structured",paste0(id,".xml")),
             file.path(w09,"structured",paste0(id,".json")),
             file.path(w09,"pdfs",paste0(id,".pdf")))
    p<-paths[file.exists(paths)]
    if(!length(p))"" else normalizePath(p[[1L]],winslash="/",mustWork=TRUE)
  }
  source_paths<-vapply(record_ids,function(id)
    if(id%in%queue$record_id)choose_source(id) else "",character(1))
  source_hashes<-vapply(source_paths,function(p)
    if(nzchar(p))sha(p) else "",character(1))
  available<-in_queue&nzchar(source_paths)
  inventory<-data.frame(record_id=record_ids,
    availability=ifelse(!in_queue,"missing_from_w09_queue",
      ifelse(available,"available","full_text_missing")),
    source_path=unname(source_paths),source_sha256=unname(source_hashes),
    stringsAsFactors=FALSE)
  ids<-record_ids[available]
  if(!length(ids))
    stop("No included W07 publications have retrievable W09 full-text files")
  message("[FT mechanisms] ",length(record_ids)," W07 records selected; ",
          length(ids)," with W09 files, ",sum(!in_queue),
          " absent from retrieval queue, ",sum(in_queue&!available),
          " without available full text")
  base<-if(is.null(output_dir))file.path(w09,"fulltext_mechanism_inventory")
        else normalizePath(output_dir,winslash="/",mustWork=TRUE)
  key<-substr(digest::digest(c(sha(files[["queue"]]),
    sha(files[["coding"]]),sha(files[["ontology"]]),sha(prompt),
    sha(criteria),record_ids,source_hashes,model),algo="sha256"),1L,12L)
  dest<-file.path(base,paste0("v2-",key))
  manifest_path<-file.path(dest,"manifest.json")
  manifest<-list(schema="ostara-fulltext-mechanism-all-v2",
    source_w09=w09,source_w07=w07,
    w09_queue_sha256=sha(files[["queue"]]),
    w07_coding_sha256=sha(files[["coding"]]),
    w07_ontology_sha256=sha(files[["ontology"]]),
    prompt_sha256=sha(prompt),w08_criteria_sha256=sha(criteria),
    model=model,selected_ids=as.list(record_ids),
    available_ids=as.list(ids),
    source_sha256=as.list(unname(source_hashes)))
  if(file.exists(manifest_path)) {
    prior<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(prior,manifest))
      stop("Full-text mechanism checkpoint provenance conflict")
  } else {
    if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Output directory already exists without a valid manifest")
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    jsonlite::write_json(manifest,manifest_path,auto_unbox=TRUE,pretty=TRUE)
    utils::write.csv(inventory,file.path(dest,"source_inventory.csv"),
                     row.names=FALSE,na="")
    if(!file.copy(prompt,file.path(dest,"prompt.txt"))||
       !file.copy(criteria,file.path(dest,"w08_criteria.txt")))
      stop("Failed to archive prompts")
  }
  if(!identical(sha(file.path(dest,"prompt.txt")),sha(prompt))||
     !identical(sha(file.path(dest,"w08_criteria.txt")),sha(criteria)))
    stop("Archived prompt or W08 case definitions do not match")
  saved_inventory<-utils::read.csv(file.path(dest,"source_inventory.csv"),
                                    stringsAsFactors=FALSE)
  if(!identical(saved_inventory,inventory))
    stop("Source inventory changed for an existing checkpoint")
  results<-file.path(dest,"mechanism_assessments.jsonl")
  done<-if(file.exists(results))readjl(results) else list()
  finished<-vapply(done,function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(finished)||any(!finished%in%ids))
    stop("Checkpoint contains duplicate or unexpected publication IDs")
  todo<-setdiff(ids,finished)
  if(max_records>0L)todo<-head(todo,max_records)
  if(length(todo)&&!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY required for ",length(todo)," remaining assessments; ",
         "the run manifest and inventory are saved at ",dest)
  w08lines<-lines(criteria)
  start<-which(trimws(w08lines)=="DEFINITIONS")
  end<-which(trimws(w08lines)=="CLASSIFICATION")
  if(length(start)!=1L||length(end)!=1L||end<=start+1L)
    stop("W08 criteria headings not recognised")
  case_criteria<-paste(w08lines[start:(end-1L)],collapse="\n")
  instructions<-paste(lines(prompt),collapse="\n")
  ontology_text<-utils::capture.output(utils::write.csv(ont,row.names=FALSE))
  for(id in todo) {
    source<-w09_text_payload(w09,id,max_pages=0L)
    if(!identical(sha(source$path),source_hashes[match(id,record_ids)]))
      stop("Full-text file changed during extraction: ",id)
    input<-list(record_id=id,
      approved_ontology=ontology_text,
      w08_case_inclusion_criteria=case_criteria,
      final_w08_case_status=NULL,
      fulltext=source$text)
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",
         content=list(list(type="input_text",text=instructions))),
        list(role="user",content=list(list(type="input_text",
         text=as.character(jsonlite::toJSON(input,auto_unbox=TRUE,null="null")))))),
      text=list(format=list(type="json_object")))
    result<-NULL
    for(attempt in seq_len(3L)) {
      candidate<-tryCatch({
        response<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        parts<-unlist(lapply(response$output,function(x)
          lapply(x$content,function(p)
            if(identical(p$type,"output_text"))p$text else NULL)),
          use.names=FALSE)
        if(!length(parts))stop("Model returned no output text")
        z<-jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE)
        .w09_validate_independent_mechanism_sweep(
          z,id,codes,source$text)
        z
      },error=function(e)e)
      if(!inherits(candidate,"error")){result<-candidate;break}
      message("[FT mechanisms] ",id," attempt ",attempt,
              "/3 failed validation or transport: ",conditionMessage(candidate))
    }
    if(is.null(result))
      stop("Full-text mechanism extraction failed for ",id,
           ". Prior validated records remain saved in ",results)
    result$provenance<-list(source_path=source$path,
      source_sha256=source$sha256,source_format=source$format,
      w07_ontology_sha256=sha(files[["ontology"]]),
      w08_case_criteria_sha256=sha(criteria),
      model=model,prompt_sha256=sha(prompt))
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=results,append=TRUE,sep="")
    message("[FT mechanisms] Completed ",id)
  }
  invisible(dest)
}
