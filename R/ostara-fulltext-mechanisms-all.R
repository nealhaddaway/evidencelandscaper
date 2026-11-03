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
    if(sc(m$status)!="not_identified") {
      quoted(m,"Mechanism inventory")
    } else if(nzchar(sc(m$source_quote))||nzchar(sc(m$source_anchor))) {
      stop("Unidentified mechanism must not have invented evidence")
    }
  }
  for(n in z$new_novelty) {
    if(!is.null(n$case_ids))
      stop("Independent novelty must not have case_ids")
    if(!sc(n$support_status)%in%c("implemented","proposed","unclear")||
       !nzchar(sc(n$proposed_label))||!nzchar(sc(n$working_definition))||
       (nzchar(sc(n$closest_approved_mechanism_id))&&
         !sc(n$closest_approved_mechanism_id)%in%approved_codes))
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
  for(ca in sweep$candidate_cases) {
    if(!nzchar(sc(ca$candidate_label))||
       !nzchar(sc(ca$case_description))||
       !sc(ca$implementation_status)%in%c("implemented","proposed","historical","unclear")||
       !nzchar(sc(ca$eligibility_reason)))
      stop("Incomplete provisional case candidate")
    quoted(ca,"Case presence candidate")
  }
  invisible(TRUE)
}

#' Code all available included full texts for mechanisms and case presence
#'
#' Completely independent of case-extraction registries or W09 case IDs.
#' Publications are selected from the full W09 included retrieval queue,
#' independently of W07 abstract mechanism or case coding. Missing full text
#' and records absent from W09 are recorded, not silently deemed negative.
#' Results are checkpointed in an immutable versioned directory. Human review
#' is required before case/insights routing and ontology changes.
#'
#' @param w09 W09 retrieval run directory (with retrieval_queue.csv).
#' @param w07 Optional W07 directory used only as a fallback ontology source.
#' @param record_ids Optional W09 included record IDs; default the entire queue.
#' @param max_records Maximum NEW LLM assessments; 0 means all available.
#' @param model Luna model.
#' @param output_dir Optional new output base directory.
#' @param ontology_file Explicit approved CSV ontology, overriding w07/ontology.csv.
#' @param prompt_file Optional locally revised standalone full-text prompt.
#' @return Versioned mechanism inventory run directory, invisibly.
#' @export
extract_ostara_all_fulltext_mechanisms <- function(
    w09,w07=NULL,record_ids=NULL,max_records=0L,model="gpt-5.6-luna",
    output_dir=NULL,ontology_file=NULL,prompt_file=NULL) {
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  if(!is.null(w07))w07<-normalizePath(w07,winslash="/",mustWork=TRUE)
  if(!is.character(model)||length(model)!=1L||is.na(model)||
     !grepl("-luna$",model))
    stop("Only a Luna model is permitted for new full-text assessments")
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer")
  if(is.null(ontology_file)) {
    if(is.null(w07))stop("Provide ontology_file or a W07 ontology directory")
    ontology_file<-file.path(w07,"ontology.csv")
  }
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  files<-c(queue=file.path(w09,"retrieval_queue.csv"),ontology=ontology_file)
  if(any(!file.exists(files)))stop("Missing W09 queue or approved ontology")
  prompt<-if(is.null(prompt_file))
    system.file("w09","fulltext_mechanisms_supplement_v1.txt",
                package="evidencelandscaper") else
    normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  if(!nzchar(prompt))
    stop("Package is missing full-text mechanism and case assessment prompt")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  lines<-function(p)readLines(p,warn=FALSE,encoding="UTF-8")
  readjl<-function(p)lapply(Filter(nzchar,lines(p)),
                              jsonlite::fromJSON,simplifyVector=FALSE)
  ont<-utils::read.csv(files[["ontology"]],stringsAsFactors=FALSE,
                        check.names=FALSE,fileEncoding="UTF-8-BOM")
  expected_columns<-c("mechanism_id","collective_action_type",
    "high_level_mechanism","fine_level_mechanism","definition")
  if(!identical(names(ont),expected_columns)||nrow(ont)!=30L||
     anyNA(ont)||any(!nzchar(trimws(as.matrix(ont))))||
     anyDuplicated(ont$mechanism_id)||
     any(!grepl("^M[0-9]+\\.[0-9]+\\.[0-9]+$",ont$mechanism_id)))
    stop("Approved ontology must have 30 unique, nonempty IDs and five canonical columns")
  codes<-as.character(ont$mechanism_id)
  queue<-utils::read.csv(files[["queue"]],stringsAsFactors=FALSE)
  if(!"record_id"%in%names(queue)||anyDuplicated(queue$record_id)||
     anyNA(queue$record_id))stop("Invalid W09 retrieval queue IDs")
  queue_ids<-as.character(queue$record_id)
  if(is.null(record_ids))record_ids<-queue_ids
  if(!is.character(record_ids)||!length(record_ids)||anyNA(record_ids)||
     anyDuplicated(record_ids)||any(!record_ids%in%queue_ids))
    stop("record_ids must be unique W09 included publication IDs")
  record_ids<-sort(record_ids)
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
    stop("No selected W09 publications have retrievable full-text files")
  message("[FT mechanisms] ",length(record_ids)," included records selected; ",
          length(ids)," with W09 files, ",sum(!in_queue),
          " absent from retrieval queue, ",sum(in_queue&!available),
          " without available full text")
  base<-if(is.null(output_dir))file.path(w09,"fulltext_mechanism_inventory")
        else normalizePath(output_dir,winslash="/",mustWork=TRUE)
  key<-substr(digest::digest(c(sha(files[["queue"]]),
    sha(files[["ontology"]]),sha(prompt),
    record_ids,source_hashes,model),algo="sha256"),1L,12L)
  dest<-file.path(base,paste0("v4-",key))
  manifest_path<-file.path(dest,"manifest.json")
  manifest<-list(schema="ostara-fulltext-mechanism-all-v4",
    source_w09=w09,source_w07=if(is.null(w07))"" else w07,
    w09_queue_sha256=sha(files[["queue"]]),
    approved_ontology_sha256=sha(files[["ontology"]]),
    prompt_sha256=sha(prompt),
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
       !file.copy(files[["ontology"]],file.path(dest,"ontology.csv")))
      stop("Failed to archive full-text prompt and approved ontology")
  }
  if(!identical(sha(file.path(dest,"prompt.txt")),sha(prompt))||
     !identical(sha(file.path(dest,"ontology.csv")),sha(files[["ontology"]])))
    stop("Archived full-text prompt does not match")
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
  instructions<-paste(lines(prompt),collapse="\n")
  ontology_text<-utils::capture.output(utils::write.csv(ont,row.names=FALSE))
  unreadable<-file.path(dest,"unreadable_fulltexts.jsonl")
  for(id in todo) {
    source<-tryCatch(w09_text_payload(w09,id,max_pages=0L),
                     error=function(e)e)
    if(inherits(source,"error")) {
      event<-list(record_id=id,source_sha256=source_hashes[match(id,record_ids)],
                  error=conditionMessage(source))
      cat(as.character(jsonlite::toJSON(event,auto_unbox=TRUE)),
          "\n",file=unreadable,append=TRUE,sep="")
      message("[FT mechanisms] Unreadable source ",id,": ",
              conditionMessage(source),". Skipping and retaining checkpoint.")
      next
    }
    if(!identical(unname(sha(source$path)),
                  unname(source_hashes[match(id,record_ids)])))
      stop("Full-text file changed during extraction: ",id)
    input<-list(record_id=id,
      approved_ontology=ontology_text,
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
    if(is.null(result)) {
      message("[FT mechanisms] Failed ",id,
              "; prior validated records remain saved in ",results)
      next
    }
    result$provenance<-list(source_identity_verified=FALSE,
      source_path=source$path,
      source_sha256=source$sha256,source_format=source$format,
      approved_ontology_sha256=sha(files[["ontology"]]),
      model=model,prompt_sha256=sha(prompt))
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=results,append=TRUE,sep="")
    message("[FT mechanisms] Completed ",id)
  }
  invisible(dest)
}
