#' Supplement W09 case extraction with exhaustive full-text mechanism coding
#'
#' Reads archived, already retrieved W09 full texts without changing W07/W09
#' model responses. The input review manifest selects exact W09/W10 evidence.
#' One checkpointed model call per publication; max_records=0 processes all.
#' @param review_dir Existing directory from prepare_ostara_mechanism_review_for_map().
#' @param max_records Upper bound on NEW API calls (0 for all).
#' @param model Model name; gpt-6-luna by default.
#' @return Versioned output directory with append-only mechanism_assessments.jsonl.
#' @export
extract_ostara_fulltext_mechanisms <- function(
    review_dir,max_records=0L,model="gpt-6-luna") {
  review_dir<-normalizePath(review_dir,mustWork=TRUE)
  man<-file.path(review_dir,"manifest.json")
  if(!file.exists(man))stop("Mechanism-review manifest not found")
  meta<-jsonlite::fromJSON(man,simplifyVector=FALSE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  # Resolve the original W09 source by checksum, never by most recent folder.
  p<-dirname(dirname(dirname(review_dir)))
  roots<-list.files(file.path(p,"runs","W09"),
                    pattern="^fulltext_case_registry_normalised\\.jsonl$",
                    recursive=TRUE,full.names=TRUE)
  roots<-roots[vapply(roots,function(f)
    identical(sha(f),meta$w09_registry_sha256),logical(1))]
  if(length(roots)!=1L)stop("Cannot resolve exactly one original W09 registry by checksum")
  registry<-roots[[1L]]
  extract_dir<-dirname(registry)
  # The W09 pilot always contains retrieval_queue.csv; locate its ancestor.
  parent<-extract_dir
  while(!file.exists(file.path(parent,"retrieval_queue.csv"))) {
    next_parent<-dirname(parent)
    if(identical(next_parent,parent))stop("W09 retrieval queue not found")
    parent<-next_parent
  }
  w09<-parent
  w07<-meta$w07_run
  if(is.null(w07)||!dir.exists(w07))stop("Reviewed W09 record has no archived W07 run")
  onto_path<-file.path(w07,"ontology.csv")
  coded_path<-file.path(w07,"coding.jsonl")
  if(!all(file.exists(c(onto_path,coded_path))))stop("Missing archived W07 files")
  onto<-utils::read.csv(onto_path,stringsAsFactors=FALSE,check.names=FALSE)
  if(!"mechanism_id"%in%names(onto)||anyDuplicated(onto$mechanism_id))
    stop("Invalid W07 ontology")
  readjl<-function(f)lapply(Filter(nzchar,readLines(f,warn=FALSE,encoding="UTF-8")),
                            jsonlite::fromJSON,simplifyVector=FALSE)
  coding<-readjl(coded_path)
  coded<-setNames(coding,vapply(coding,function(z)z$record_id,character(1)))
  records<-readjl(registry)
  # Use the actual completed W10 eligibility ledger, not merely mechanism
  # review rows: zero-mechanism publications must still be analysed.
  w10files<-list.files(dirname(extract_dir),pattern="^w10_decisions\\.jsonl$",
    recursive=TRUE,full.names=TRUE)
  w10files<-w10files[vapply(w10files,function(f)
    identical(sha(f),meta$w10_eligibility_sha256),logical(1))]
  if(length(w10files)!=1L)
    stop("Cannot locate the exact W10 eligibility ledger from review provenance")
  first<-readjl(w10files[[1L]])
  decision<-setNames(vapply(first,function(z)z$overall,character(1)),
    vapply(first,function(z)z$observation_id,character(1)))
  if(anyDuplicated(names(decision)))stop("Duplicate W10 observation decisions")
  if(!is.null(meta$second_pass_dir)) {
    second_path<-file.path(meta$second_pass_dir,"fulltext_decisions.jsonl")
    if(!file.exists(second_path))stop("Missing archived W10 second-pass decisions")
    for(z in readjl(second_path)) {
      if(!z$observation_id%in%names(decision))
        stop("Second-pass W10 assessment has unknown observation")
      decision[[z$observation_id]]<-z$overall
    }
  }
  eligible_observations<-names(decision)[decision=="eligible"]
  ids<-unique(sub("-case-[0-9]{3}$","",eligible_observations))
  if(any(!grepl("-case-[0-9]{3}$",eligible_observations)))
    stop("Unexpected W10 case identifier format")
  if(any(!ids%in%vapply(records,function(z)z$record_id,character(1))))
    stop("Review records inconsistent with W09")
  records<-setNames(records,vapply(records,function(z)z$record_id,character(1)))
  if(!is.numeric(max_records)||length(max_records)!=1L||
     is.na(max_records)||max_records<0||max_records!=as.integer(max_records))
    stop("max_records must be a nonnegative integer")
  if(!length(ids))stop("No W10-eligible case publications in current review")
  prompt_path<-system.file("w09","fulltext_mechanisms_supplement_v1.txt",
                            package="evidencelandscaper")
  if(!nzchar(prompt_path))stop("Supplemental extraction prompt missing")
  output<-file.path(review_dir,"fulltext_mechanism_supplement",
    paste0("v1-",substr(sha(prompt_path),1,12),"-",
           substr(sha(onto_path),1,12),"-",
           gsub("[^A-Za-z0-9._-]","_",model)))
  manifest_path<-file.path(output,"manifest.json")
  manifest<-list(schema="ostara-fulltext-mechanisms-v1",
    w09_registry_sha256=sha(registry),w07_ontology_sha256=sha(onto_path),
    w07_coding_sha256=sha(coded_path),prompt_sha256=sha(prompt_path),
    model=model,eligible_record_ids=as.list(ids))
  if(file.exists(manifest_path)) {
    old<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    for(k in names(manifest))if(!identical(manifest[[k]],old[[k]]))
      stop("Supplemental run provenance conflict: ",k)
  } else {
    if(dir.exists(output)&&length(list.files(output,all.files=TRUE,no..=TRUE)))
      stop("Supplemental output directory exists without valid manifest")
    dir.create(output,recursive=TRUE,showWarnings=FALSE)
    jsonlite::write_json(manifest,manifest_path,auto_unbox=TRUE,pretty=TRUE)
    file.copy(prompt_path,file.path(output,"prompt.txt"))
  }
  results<-file.path(output,"mechanism_assessments.jsonl")
  old<-if(file.exists(results))readjl(results)else list()
  finished<-vapply(old,function(z)z$record_id,character(1))
  if(anyDuplicated(finished)||any(!finished%in%ids))
    stop("Supplemental checkpoint contains duplicate or unexpected records")
  todo<-setdiff(ids,finished)
  if(max_records>0L)todo<-head(todo,max_records)
  if(length(todo)&&!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY required for supplementary extraction")
  instructions<-paste(readLines(prompt_path,warn=FALSE),collapse="\n")
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  for(id in todo) {
    source<-w09_text_payload(w09,id,max_pages=0L)
    original<-coded[[id]]
    if(is.null(original))stop("W07 source record absent: ",id)
    codes<-if(is.null(original$mechanisms))character() else
      vapply(original$mechanisms,function(m)m$mechanism_id,character(1))
    cases<-records[[id]]$cases
    case_whitelist<-eligible_observations[startsWith(eligible_observations,paste0(id,"-case-"))]
    input<-list(record_id=id,eligible_case_ids=as.list(case_whitelist),
      approved_ontology=utils::capture.output(utils::write.csv(onto,row.names=FALSE)),
      w07_abstract_mechanism_ids=as.list(codes),
      w07_abstract_novelty=original$proposed_new_mechanism,
      existing_w09_ontology_gaps=records[[id]]$ontology_gaps,
      existing_w09_cases=lapply(seq_along(cases),function(j)
        list(case_id=paste0(id,"-case-",sprintf("%03d",j)),
             case_name=cases[[j]]$case_name,
             case_description=cases[[j]]$case_description)),
      fulltext=source$text)
    response_body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",
                   text=instructions))),
                 list(role="user",content=list(list(type="input_text",
                   text=as.character(jsonlite::toJSON(input,
                     auto_unbox=TRUE,null="null")))))),
      text=list(format=list(type="json_object")))
    result<-NULL
    for(attempt in seq_len(3L)) {
      candidate<-tryCatch({
        raw<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(response_body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        parts<-unlist(lapply(raw$output,function(x)
          lapply(x$content,function(z)
            if(identical(z$type,"output_text"))z$text else NULL)),
          use.names=FALSE)
        if(!length(parts))stop("Empty model response")
        z<-jsonlite::fromJSON(parts[[1]],simplifyVector=FALSE)
        if(!identical(z$record_id,id)||!is.list(z$mechanism_inventory)||
           !is.list(z$abstract_novelty_validation)||!is.list(z$new_novelty))
          stop("Incorrect supplementary output contract")
        actual<-vapply(z$mechanism_inventory,function(x)scalar(x$mechanism_id),
                       character(1))
        if(anyDuplicated(actual)||!setequal(actual,as.character(onto$mechanism_id)))
          stop("Output must assess every approved ontology mechanism exactly once")
        valid<-c("implemented_case","mentioned_only","proposed_only",
                 "not_identified","unclear")
        if(any(!vapply(z$mechanism_inventory,function(x)
          scalar(x$status)%in%valid,logical(1))))
          stop("Invalid mechanism inventory statuses")
        candidate_case_ids<-function(x)
          as.character(unlist(x$case_ids,use.names=FALSE))
        for(entry in z$mechanism_inventory) {
          status<-scalar(entry$status)
          ids_for_entry<-candidate_case_ids(entry)
          if(status=="implemented_case") {
            if(!length(ids_for_entry)||any(!ids_for_entry%in%case_whitelist))
              stop("Implemented mechanism has no W10-eligible case")
          } else if(length(ids_for_entry))stop("Nonimplemented mechanism linked to cases")
          if(status%in%c("implemented_case","mentioned_only","proposed_only")&&
             (!nzchar(scalar(entry$source_quote))||
              !nzchar(scalar(entry$source_anchor))))
            stop("Positive mechanism assessment lacks text quotation and anchor")
        }
        for(entry in z$new_novelty) {
          if(!length(candidate_case_ids(entry))||
             any(!candidate_case_ids(entry)%in%case_whitelist)||
             !nzchar(scalar(entry$source_quote))||
             !nzchar(scalar(entry$source_anchor)))
            stop("Novel case mechanism requires eligible case and source evidence")
        }
        if(isTRUE(original$ontology_gap)&&
           !length(z$abstract_novelty_validation))
          stop("W07 abstract novelty proposal was not reassessed")
        z
      },error=function(e)e)
      if(!inherits(candidate,"error")){result<-candidate;break}
      message("[FT mechanism supplement] ",id," attempt ",attempt,
              "/3: ",conditionMessage(candidate))
    }
    if(is.null(result))stop("Incomplete extraction for ",id,
                           "; completed records remain checkpointed")
    result$eligible_case_ids<-as.list(case_whitelist)
    result$provenance<-list(source_path=source$path,
      source_sha256=source$sha256,source_format=source$format,
      w09_registry_sha256=sha(registry),w07_ontology_sha256=sha(onto_path),
      model=model,prompt_sha256=sha(prompt_path))
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=results,append=TRUE,sep="")
    message("[FT mechanism supplement] ",id," saved")
  }
  invisible(output)
}
