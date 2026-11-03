#' Supplement W09 case extraction with exhaustive full-text mechanism coding
#'
#' Reads archived, already retrieved W09 full texts without changing W07/W09
#' model responses. The input review manifest selects exact W09/W10 evidence.
#' One checkpointed model call per publication; max_records=0 processes all.
#' @param review_dir Existing directory from prepare_ostara_mechanism_review_for_map().
#' @param max_records Upper bound on NEW API calls (0 for all).
#' @param model Model name; gpt-5.6-luna by default.
#' @return Versioned output directory with append-only mechanism_assessments.jsonl.
#' @export
extract_ostara_fulltext_mechanisms <- function(
    review_dir,max_records=0L,model="gpt-5.6-luna") {
  if(!is.character(model)||length(model)!=1L||is.na(model)||!grepl("-luna$",model))
    stop("Only Luna models are permitted for new API calls.")
  review_dir<-normalizePath(review_dir,mustWork=TRUE)
  man<-file.path(review_dir,"manifest.json")
  if(!file.exists(man))stop("Mechanism-review manifest not found")
  meta<-jsonlite::fromJSON(man,simplifyVector=FALSE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  eligibility_first<-identical(meta$mode,"eligibility_first")
  if(eligibility_first) {
    extract_dir<-normalizePath(meta$extraction_dir,mustWork=TRUE)
    registry<-file.path(extract_dir,"fulltext_case_registry_normalised.jsonl")
    w09<-normalizePath(meta$w09,mustWork=TRUE)
    if(!file.exists(registry)||
       !identical(sha(registry),meta$w09_registry_sha256)||
       !file.exists(file.path(w09,"retrieval_queue.csv")))
      stop("Original W09 source missing or checksum changed")
    wm<-jsonlite::fromJSON(file.path(extract_dir,"manifest.json"),
                            simplifyVector=FALSE)
    if(!identical(wm$eligibility_ledger_sha256,
                  meta$eligibility_final_sha256))
      stop("W09 extraction not linked to approved eligibility")
    map<-dirname(dirname(dirname(w09)))
    state_file<-file.path(map,"runs","orchestrated",meta$run_name,"state.json")
    if(!file.exists(state_file))stop("Named run state missing")
    state<-jsonlite::fromJSON(state_file,simplifyVector=FALSE)
    if(!identical(normalizePath(state$w09$extraction_dir,mustWork=TRUE),
                  extract_dir))
      stop("Named run extraction directory changed")
    original_path<-file.path(state$preextraction$run,
                              "candidate_case_decisions.jsonl")
    final_path<-file.path(state$preextraction$run,
                           "final_case_eligibility.csv")
    if(!all(file.exists(c(original_path,final_path)))||
       !identical(sha(original_path),meta$eligibility_ledger_sha256)||
       !identical(sha(final_path),meta$eligibility_final_sha256))
      stop("Approved eligibility records changed")
  } else {
    # Preserve the legacy W09/W10 provenance path unchanged.
    p<-dirname(dirname(dirname(review_dir)))
    roots<-list.files(file.path(p,"runs","W09"),
                      pattern="^fulltext_case_registry_normalised\\.jsonl$",
                      recursive=TRUE,full.names=TRUE)
    roots<-roots[vapply(roots,function(f)
      identical(sha(f),meta$w09_registry_sha256),logical(1))]
    if(length(roots)!=1L)
      stop("Cannot resolve exactly one original W09 registry by checksum")
    registry<-roots[[1L]]
    extract_dir<-dirname(registry)
    parent<-extract_dir
    while(!file.exists(file.path(parent,"retrieval_queue.csv"))) {
      next_parent<-dirname(parent)
      if(identical(next_parent,parent))stop("W09 retrieval queue not found")
      parent<-next_parent
    }
    w09<-parent
  }
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
  if(eligibility_first) {
    audit<-utils::read.csv(final_path,colClasses="character",
                           stringsAsFactors=FALSE)
    approved<-audit[audit$decision=="eligible",
                    c("record_id","case_id"),drop=FALSE]
    eligible_observations<-as.character(approved$case_id)
    ids<-unique(as.character(approved$record_id))
    if(!length(ids)||anyDuplicated(eligible_observations)||
       !setequal(ids,vapply(records,function(z)z$record_id,character(1))))
      stop("Approved eligibility and W09 publication IDs differ")
    for(z in records) {
      actual<-vapply(z$cases,function(ca)
        if(is.null(ca$eligibility_case_id))"" else
          as.character(ca$eligibility_case_id),character(1))
      expected<-eligible_observations[approved$record_id==z$record_id]
      if(anyDuplicated(actual)||!setequal(actual,expected))
        stop("Extracted case IDs differ from approved cases: ",z$record_id)
    }
  } else {
    w10files<-list.files(dirname(extract_dir),
      pattern="^w10_decisions\\.jsonl$",recursive=TRUE,full.names=TRUE)
    w10files<-w10files[vapply(w10files,function(f)
      identical(sha(f),meta$w10_eligibility_sha256),logical(1))]
    if(length(w10files)!=1L)
      stop("Cannot locate exact W10 eligibility ledger from provenance")
    first<-readjl(w10files[[1L]])
    decision<-setNames(vapply(first,function(z)z$overall,character(1)),
      vapply(first,function(z)z$observation_id,character(1)))
    if(anyDuplicated(names(decision)))
      stop("Duplicate W10 observation decisions")
    if(!is.null(meta$second_pass_dir)) {
      second_path<-file.path(meta$second_pass_dir,"fulltext_decisions.jsonl")
      if(!file.exists(second_path))
        stop("Missing archived W10 second-pass decisions")
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
  }
  records<-setNames(records,vapply(records,function(z)z$record_id,
                                   character(1)))
  if(!is.numeric(max_records)||length(max_records)!=1L||
     is.na(max_records)||max_records<0||max_records!=as.integer(max_records))
    stop("max_records must be a nonnegative integer")
  if(!length(ids))stop("No eligible case publications in current review")
  prompt_path<-system.file("w09","fulltext_mechanisms_supplement_v1.txt",
                            package="evidencelandscaper")
  w08_criteria_path<-system.file("w08","case_identification_prompt_v3.txt",
                                  package="evidencelandscaper")
  if(!nzchar(prompt_path)||!nzchar(w08_criteria_path))
    stop("Full-text mechanism/case-sweep prompt or W08 case criteria missing")
  output<-file.path(review_dir,"fulltext_mechanism_supplement",
    paste0("v2-",substr(sha(prompt_path),1,12),"-",
           substr(sha(onto_path),1,12),"-",
           gsub("[^A-Za-z0-9._-]","_",model)))
  manifest_path<-file.path(output,"manifest.json")
  manifest<-list(schema="ostara-fulltext-mechanisms-case-sweep-v2",
    w09_registry_sha256=sha(registry),w07_ontology_sha256=sha(onto_path),
    w07_coding_sha256=sha(coded_path),prompt_sha256=sha(prompt_path),
    w08_criteria_sha256=sha(w08_criteria_path),
    model=model,eligible_record_ids=as.list(ids))
  if(eligibility_first) {
    manifest$eligibility_mode<-"eligibility_first"
    manifest$eligibility_ledger_sha256<-meta$eligibility_ledger_sha256
    manifest$eligibility_final_sha256<-meta$eligibility_final_sha256
  }
  if(file.exists(manifest_path)) {
    old<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    for(k in names(manifest))if(!identical(manifest[[k]],old[[k]]))
      stop("Supplemental run provenance conflict: ",k)
  } else {
    if(dir.exists(output)&&length(list.files(output,all.files=TRUE,no..=TRUE)))
      stop("Supplemental output directory exists without valid manifest")
    dir.create(output,recursive=TRUE,showWarnings=FALSE)
    jsonlite::write_json(manifest,manifest_path,auto_unbox=TRUE,pretty=TRUE)
    if(!file.copy(prompt_path,file.path(output,"prompt.txt"))||
       !file.copy(w08_criteria_path,file.path(output,"w08_case_criteria.txt")))
      stop("Could not archive full-text prompt and W08 criteria")
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
  if(!identical(sha(file.path(output,"prompt.txt")),sha(prompt_path))||
     !identical(sha(file.path(output,"w08_case_criteria.txt")),sha(w08_criteria_path)))
    stop("Archived mechanism prompt or case criteria changed")
  instructions<-paste(readLines(prompt_path,warn=FALSE),collapse="\n")
  w08_lines<-readLines(w08_criteria_path,warn=FALSE,encoding="UTF-8")
  definitions<-which(trimws(w08_lines)=="DEFINITIONS")
  classification<-which(trimws(w08_lines)=="CLASSIFICATION")
  if(length(definitions)!=1L||length(classification)!=1L||
     classification<=definitions+1L)
    stop("Archived W08 case inclusion criteria headings changed; review manually")
  # Supply just the W08 definitions and inclusion/exclusion boundaries.
  # The W08 response schema belongs to abstract screening and must not
  # compete with the separate W09 full-text JSON output contract.
  w08_case_criteria<-paste(w08_lines[definitions:(classification-1L)],
                            collapse="\n")
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  for(id in todo) {
    source<-w09_text_payload(w09,id,max_pages=0L)
    # Case IDs are deliberately not supplied: mechanism detection is independent
    # of the existing case extraction; full-text case sweep uses W08 criteria.
    input<-list(record_id=id,
      approved_ontology=utils::capture.output(utils::write.csv(onto,row.names=FALSE)),
      w08_case_inclusion_criteria=w08_case_criteria,
      final_w08_case_status=NULL,
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
           !is.list(z$new_novelty))
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
        if(is.null(z$case_presence_assessment)||
           !is.list(z$case_presence_assessment))
          stop("Full-text case-presence assessment is missing")
        sweep<-z$case_presence_assessment
        cs<-scalar(sweep$case_status)
        if(!cs%in%c("case_present","no_case","unclear"))
          stop("Invalid full-text case status")
        if(!is.list(sweep$candidate_cases))
          stop("Invalid full-text candidate cases array")
        cnt<-sweep$candidate_case_count
        if(cs=="unclear") {
          if(!is.null(cnt))stop("Unclear case count must be null")
        } else {
          if(!is.numeric(cnt)||length(cnt)!=1L||is.na(cnt)||
             cnt!=as.integer(cnt)||cnt<0L||
             (cs=="no_case"&&cnt!=0L)||
             (cs=="case_present"&&cnt<1L)||
             length(sweep$candidate_cases)!=cnt)
            stop("Case presence/count mismatch")
        }
        comparison<-scalar(sweep$w08_comparison)
        if(!comparison%in%c("agrees","disagrees","not_comparable","not_provided"))
          stop("Invalid W08 case comparison")
        # The current case-extraction review has no authoritative W08 join.
        # Until it does, W08 comparison must be explicitly not_provided.
        if(comparison!="not_provided")
          stop("W08 comparison cannot be asserted without an authoritative W08 input")
        if(!is.logical(sweep$review_required)||
           length(sweep$review_required)!=1L||
           is.na(sweep$review_required)||
           !identical(sweep$review_required,cs=="unclear"))
          stop("Case review_required inconsistent with case status and absent W08")
        if(!nzchar(scalar(sweep$assessment_reason)))
          stop("Missing full-text case assessment reason")
        check_evidence<-function(x,context) {
          quote<-scalar(x$source_quote)
          anchor<-scalar(x$source_anchor)
          if(!nzchar(quote)||!nzchar(anchor)||
             !grepl(quote,source$text,fixed=TRUE)||
             !grepl(anchor,source$text,fixed=TRUE)||
             !grepl("^\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]$",anchor))
            stop(context," has unverified quotation or anchor")
        }
        for(ca in sweep$candidate_cases) {
          if(!nzchar(scalar(ca$candidate_label))||
             !nzchar(scalar(ca$case_description))||
             !nzchar(scalar(ca$eligibility_reason)))
            stop("Incomplete provisional full-text case candidate")
          check_evidence(ca,"Case sweep")
        }
        for(entry in z$mechanism_inventory) {
          if(!is.null(entry$case_ids))
            stop("Mechanism inventory must not contain case IDs")
          if(!identical(scalar(entry$status),"not_identified"))
            check_evidence(entry,"Mechanism inventory")
        }
        for(entry in z$new_novelty) {
          if(!is.null(entry$case_ids))
            stop("Novel mechanisms must not contain case IDs")
          if(!scalar(entry$support_status)%in%c("implemented","proposed","unclear")||
             !nzchar(scalar(entry$proposed_label))||
             !nzchar(scalar(entry$working_definition)))
            stop("Invalid new novelty record")
          check_evidence(entry,"New mechanism")
        }
        z
      },error=function(e)e)
      if(!inherits(candidate,"error")){result<-candidate;break}
      message("[FT mechanism supplement] ",id," attempt ",attempt,
              "/3: ",conditionMessage(candidate))
    }
    if(is.null(result))stop("Incomplete extraction for ",id,
                           "; completed records remain checkpointed")
    result$provenance<-list(source_path=source$path,
      source_sha256=source$sha256,source_format=source$format,
      w09_registry_sha256=sha(registry),w07_ontology_sha256=sha(onto_path),
      model=model,prompt_sha256=sha(prompt_path),
      w08_criteria_sha256=sha(w08_criteria_path))
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=results,append=TRUE,sep="")
    message("[FT mechanism supplement] ",id," saved")
  }
  invisible(output)
}
