#' Identify case evidence in a sample of W04-retained abstracts
#'
#' W08 is independent of W07. One JSON response is stored per publication;
#' separate CSVs make the candidate cases and all publication assessments
#' reviewable. Existing W04/W07 files are read-only.
#' @param n Randomly sampled retained publications (default 20); 0 selects all.
#' @param w04_run Optional explicit finalised W04 run, including sampled runs.
#' @param seed Sampling seed, recorded in the manifest.
#' @param model OpenAI Responses model.
#' @param map_path Local map root folder (defaults to the Ostara map).
#' @param prompt_file Optional custom UTF-8 case-identification prompt.
#' @return W08 pilot run directory, invisibly.
#' @export
trial_w08_cases <- function(n=20L,seed=20261023L,model="gpt-6-luna",
                            map_path=NULL,prompt_file=NULL,w04_run=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(is.null(prompt_file)) {
    prompt_file<-system.file("w08","case_identification_prompt_v2.txt",
                             package="evidencelandscaper")
  } else {
    prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  }
  if(!nzchar(prompt_file)||!file.exists(prompt_file))
    stop("W08 case-identification prompt not found.")
  form_file<-system.file("w08","case_identification_fields_v2.csv",
                         package="evidencelandscaper")
  if(!nzchar(form_file))stop("W08 extraction field schema not packaged.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY is required for the W08 case pilot.")
  if(length(n)!=1L||is.na(n)||n!=as.integer(n)||n<0L)
    stop("n must be a nonnegative integer (0 means all retained publications).")
  if(length(seed)!=1L||is.na(seed)||seed!=as.integer(seed))
    stop("seed must be one integer.")
  n<-as.integer(n);seed<-as.integer(seed)
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  readjl<-function(path) {
    l<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  roots<-list.dirs(file.path(map_path,"runs","W01"),
                   recursive=FALSE,full.names=TRUE)
  runs<-unlist(lapply(roots,function(r) {
    d<-file.path(r,"W04_local")
    if(dir.exists(d))list.dirs(d,recursive=FALSE,full.names=TRUE)
    else character()
  }),use.names=FALSE)
  if(!is.null(w04_run)) {
    w04_run<-normalizePath(w04_run,winslash="/",mustWork=TRUE)
    runs<-runs[runs==w04_run]
  } else runs<-runs[grepl("^full-",basename(runs))]
  runs<-runs[file.exists(file.path(runs,"finalised","summary.json"))]
  if(length(runs)!=1L)
    stop("Expected exactly one finalised W04 full run; select a map with a unique run.")
  w04<-runs[[1L]]
  final_path<-file.path(w04,"finalised","workflow04_final_screening_layer.jsonl")
  sm<-jsonlite::fromJSON(file.path(w04,"finalised","summary.json"))
  if(!identical(sm$status,"final")||
     (is.null(w04_run)&&!identical(sm$run_type,"full"))||
     !identical(sm$final_layer_sha256,sha(final_path)))
    stop("W04 finalised screening integrity check failed.")
  screened<-readjl(final_path)
  retained<-Filter(function(z)identical(z$screening$decision,"retain"),screened)
  if(length(screened)!=sm$records||length(retained)!=sm$retain)
    stop("Final W04 count mismatch.")
  if(n>length(retained))stop("n exceeds the number of retained publications.")
  if(n==0L)n<-length(retained)
  id<-vapply(retained,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(id))stop("Duplicate retained publication IDs.")
  # Keep the original RNG state of the caller unchanged.
  has_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(has_seed)old_seed<-get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    if(has_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)
    else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
      rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed)
  selected<-sample(sort(id),size=n,replace=FALSE)
  canonical_file<-file.path(dirname(dirname(w04)),"W02_main",
                            "finalised","canonical.jsonl")
  if(!file.exists(canonical_file))stop("W02 canonical source not found.")
  original<-readjl(canonical_file)
  source<-setNames(original,vapply(original,function(z)
    as.character(z$identity$record_id),character(1)))
  if(any(!selected %in% names(source)))stop("Selected W08 IDs missing from W02.")
  corrected<-list()
  if(isTRUE(sm$reconciled_abstracts)) {
    rr<-file.path(w04,"abstract_rescreens")
    dirs<-list.dirs(rr,recursive=FALSE,full.names=TRUE)
    dirs<-dirs[file.exists(file.path(dirs,"manifest.json"))]
    if(length(dirs)!=1L)stop("Cannot uniquely locate W04 abstract rescreen.")
    path<-file.path(dirs[[1L]],"inputs","submitted_abstracts.csv")
    if(!file.exists(path))stop("Corrected W04 abstract CSV missing.")
    changes<-utils::read.csv(path,stringsAsFactors=FALSE,
                              fileEncoding="UTF-8-BOM")
    if(anyDuplicated(changes$record_id))stop("Duplicated abstract corrections.")
    corrected<-setNames(as.list(changes$abstract),changes$record_id)
  }
  prompt<-paste(readLines(prompt_file,warn=FALSE,encoding="UTF-8"),
                collapse="\n")
  if(!nzchar(trimws(prompt)))stop("Empty W08 prompt.")
  tag<-paste0("sample-",n,"-seed-",seed,"-",substr(sha(final_path),1,10),
              "-",substr(sha(prompt_file),1,10))
  if(!identical(model,"gpt-5.6")) tag<-paste0(tag,"-model-",gsub("[^A-Za-z0-9._-]","_",model))
  dest<-file.path(map_path,"runs","W08",tag)
  manifest_path<-file.path(dest,"manifest.json")
  out<-file.path(dest,"case_assessments.jsonl")
  info<-list(schema="local-w08-case-pilot-v1",status="prepared",
    source_w04=w04,source_w04_sha256=sha(final_path),
    canonical_w02_sha256=sha(canonical_file),
    prompt_sha256=sha(prompt_file),schema_sha256=sha(form_file),
    model=model,seed=seed,sample_size=n,selected_record_ids=as.list(selected))
  if(file.exists(manifest_path)) {
    prior<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    for(field in c("source_w04_sha256","canonical_w02_sha256",
                   "prompt_sha256","schema_sha256","model","seed",
                   "sample_size","selected_record_ids"))
      if(!identical(prior[[field]],info[[field]]))
        stop("W08 run provenance changed; refusing to reuse output: ",field)
    if(!identical(sha(file.path(dest,"prompt.txt")),sha(prompt_file))||
       !identical(sha(file.path(dest,"fields.csv")),sha(form_file)))
      stop("W08 archived instructions changed.")
  } else {
    if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Unexpected non-empty W08 destination without manifest.")
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(prompt_file,file.path(dest,"prompt.txt"))||
       !file.copy(form_file,file.path(dest,"fields.csv")))
      stop("Failed to archive W08 inputs.")
    jsonlite::write_json(info,manifest_path,auto_unbox=TRUE,pretty=TRUE)
  }
  done<-if(file.exists(out))readjl(out) else list()
  done_ids<-vapply(done,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(done_ids)||any(!done_ids %in% selected))
    stop("W08 saved responses contain duplicate or unexpected record IDs.")
  validate<-function(z,record_id,input_text) {
    one<-function(v) is.character(v)&&length(v)==1L&&!is.na(v)
    if(!one(z$record_id)||!identical(z$record_id,record_id))
      stop("W08 returned mismatched record ID: ",record_id)
    st<-z$case_evidence_status
    if(!one(st)||!st%in%c("specific_case","multiple_cases",
                          "no_identifiable_case","unclear"))
      stop("W08 invalid case status: ",record_id)
    cases<-z$cases
    if(is.null(cases))stop("W08 missing cases array: ",record_id)
    if(!is.list(cases))stop("W08 cases must be an array: ",record_id)
    k<-length(cases)
    if((st=="specific_case"&&k!=1L)||(st=="multiple_cases"&&k<2L)||
       (st=="no_identifiable_case"&&k!=0L)||(st=="unclear"&&k>1L))
      stop("W08 status/case-count inconsistency: ",record_id)
    if(!one(z$fulltext_priority)||
       !z$fulltext_priority%in%c("high","medium","low")||
       !one(z$assessment_reason)||!nzchar(trimws(z$assessment_reason)))
      stop("W08 priority/reason missing: ",record_id)
    for(ca in cases) {
      for(f in c("case_description","supporting_evidence","confidence",
                 "implementation_status"))
        if(!one(ca[[f]])||!nzchar(trimws(ca[[f]])))
          stop("W08 incomplete case data: ",record_id," field ",f)
      for(f in c("collective_action_confidence","collective_action_basis","case_kind"))
        if(!one(ca[[f]])||!nzchar(trimws(ca[[f]])))
          stop("W08 v2 missing case field: ",record_id," ",f)
      if(!ca$collective_action_confidence%in%c("high","medium","low") ||
         !ca$case_kind%in%c("collective_arrangement","research_activity",
                            "commercial_service","social_mobilisation","other_unclear"))
        stop("W08 v2 invalid case classification: ",record_id)
      if(!ca$confidence%in%c("high","medium","low")||
         !ca$implementation_status%in%c("implemented","proposed",
                                         "historical","unclear"))
        stop("W08 invalid case categories: ",record_id)
      # Evidence must be an excerpt from submitted title or abstract.
      norm<-function(x)gsub("[[:space:]]+"," ",trimws(x))
      if(!grepl(norm(ca$supporting_evidence),norm(input_text),fixed=TRUE))
        stop("W08 evidence excerpt not in input: ",record_id)
    }
    invisible(TRUE)
  }
  # Verify any saved responses before resuming.
  for(z in done) {
    rid<-z$record_id;can<-source[[rid]]$canonical
    abs<-if(!is.null(corrected[[rid]]))corrected[[rid]] else can$abstract
    txt<-paste(unlist(c(can$title,abs),use.names=FALSE),collapse=" ")
    validate(z,rid,txt)
  }
  # An explicitly archived partial result can reach checkpoint 2 without
  # repeating failed model calls. Verify the append-only validated ledger.
  partial_path<-file.path(dest,"summary.json")
  if(file.exists(partial_path)) {
    old_summary<-jsonlite::fromJSON(partial_path,simplifyVector=FALSE)
    statuses<-unlist(old_summary[names(old_summary)=="status"],use.names=FALSE)
    partial_status<-if(length(statuses))tail(statuses,1L) else ""
    unresolved<-setdiff(selected,done_ids)
    archived_unresolved<-unlist(old_summary$unresolved_record_ids,use.names=FALSE)
    if(identical(partial_status,"PILOT_PARTIAL") &&
       identical(old_summary$responses_sha256,sha(out)) &&
       identical(as.integer(old_summary$validated_records),as.integer(length(done))) &&
       setequal(as.character(archived_unresolved),unresolved) &&
       length(unresolved)>0L) {
      corrected<-info
      corrected$status<-"PILOT_PARTIAL"
      corrected$validated_records<-length(done)
      corrected$unresolved_record_ids<-as.list(unresolved)
      corrected$responses_sha256<-sha(out)
      jsonlite::write_json(corrected,partial_path,auto_unbox=TRUE,pretty=TRUE)
      message("[W08] Reusing ",length(done)," validated records; ",
        length(unresolved)," archived unvalidated record(s). No model calls.")
      return(invisible(dest))
    }
  }
  message("[W08] Progress: ",length(done_ids)," of ",length(selected)," records completed; ",length(selected)-length(done_ids)," remaining.")
  progress_index<-length(done_ids)
  failures<-character()
  for(record_id in selected[!selected %in% done_ids]) {
    can<-source[[record_id]]$canonical
    abs<-if(!is.null(corrected[[record_id]]))corrected[[record_id]] else can$abstract
    txt<-paste(unlist(c(can$title,abs),use.names=FALSE),collapse=" ")
    input<-list(record_id=record_id,title=can$title,abstract=abs,
                keywords=if(!is.null(can$author_keywords))
                  can$author_keywords else can$keywords)
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(
        list(role="system",content=list(list(type="input_text",text=prompt))),
        list(role="user",content=list(list(type="input_text",
          text=jsonlite::toJSON(input,auto_unbox=TRUE,null="null"))))),
      text=list(format=list(type="json_object")))
    # Retry validation failures up to four times. Do not relax evidence
    # validation or alter prior completed append-only responses.
    for(attempt in seq_len(5L)) {
      resp<-httr2::request("https://api.openai.com/v1/responses") |>
        httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
        httr2::req_body_json(body,auto_unbox=TRUE) |>
        httr2::req_timeout(180) |> httr2::req_perform() |>
        httr2::resp_body_json(simplifyVector=FALSE)
      pieces<-unlist(lapply(resp$output,function(item)
        unlist(lapply(item$content,function(x)
          if(identical(x$type,"output_text"))x$text else NULL),
          use.names=FALSE)),use.names=FALSE)
      if(!length(pieces))stop("No W08 text response for ",record_id)
      z<-jsonlite::fromJSON(pieces[[1L]],simplifyVector=FALSE)
      issue<-tryCatch({validate(z,record_id,txt);NULL},
                      error=function(e)conditionMessage(e))
      if(is.null(issue))break
      retryable<-startsWith(issue,"W08 evidence excerpt not in input:") ||
        startsWith(issue,"W08 status/case-count inconsistency:")
      if(attempt==5L && retryable) {
        failures<-c(failures,record_id)
        issue_path<-file.path(dest,"invalid_evidence_responses.jsonl")
        cat(jsonlite::toJSON(list(record_id=record_id,model=model,
          validation_error=issue,response=z,attempts=attempt),
          auto_unbox=TRUE,null="null"),"\n",file=issue_path,
          append=TRUE,sep="")
        message("[W08] ",record_id,
                " still has an invalid response; retained for review; continuing.")
        break
      }
      if(!retryable)
        stop(issue)
      message("[W08] Invalid model response for ",record_id,
              "; retrying with explicit validation instruction (attempt ",attempt,"/5).")
      body$input[[1L]]$content[[1L]]$text<-paste0(
        prompt,"\n\nRETRY VALIDATION: Ensure case_evidence_status and ",
        "number of cases agree: specific_case exactly 1, multiple_cases ",
        "at least 2, no_identifiable_case exactly 0, unclear at most 1. ",
        "The previous response may also have given a ",
        "supporting_evidence quotation that was NOT a verbatim substring ",
        "of the supplied title or abstract. Return only exact continuous ",
        "source excerpts in supporting_evidence. Do not paraphrase, ",
        "change punctuation, or quote text from elsewhere. If no exact ",
        "evidence can be found, do not report a supported case.")
    }
    if(record_id %in% failures)next
    cat(jsonlite::toJSON(z,auto_unbox=TRUE,null="null"),"\n",
        file=out,append=TRUE,sep="")
    progress_index<-progress_index+1L
    message("[W08] Record ",progress_index," of ",length(selected),": ",record_id,"; ",z$case_evidence_status,
            " (",length(z$cases)," cases)")
  }
  all<-readjl(out)
  if(length(all)!=n || !setequal(vapply(all,function(z)z$record_id,character(1)),
                                  selected)) {
    missing_ids<-setdiff(selected,vapply(all,function(z)z$record_id,character(1)))
    partial<-info
    partial$status<-"PILOT_PARTIAL"
    partial$validated_records<-length(all)
    partial$unresolved_record_ids<-as.list(missing_ids)
    partial$responses_sha256<-sha(out)
    jsonlite::write_json(partial,file.path(dest,"summary.json"),
                         auto_unbox=TRUE,pretty=TRUE)
    message("[W08] Partial run: ",length(all)," of ",n,
            " validated records. Unresolved IDs: ",paste(missing_ids,collapse=", "),
            ". Invalid responses preserved; checkpoint may proceed with explicit exclusions.")
    return(invisible(dest))
  }
  summary<-table(factor(vapply(all,function(z)z$case_evidence_status,character(1)),
                        levels=c("specific_case","multiple_cases",
                                 "no_identifiable_case","unclear")))
  flat<-do.call(rbind,lapply(all,function(z) {
    if(!length(z$cases))return(NULL)
    do.call(rbind,lapply(seq_along(z$cases),function(i) {
      ca<-z$cases[[i]]
      v<-function(x)if(is.null(x))"" else as.character(x)
      data.frame(record_id=z$record_id,local_case_number=i,
        case_name=v(ca$case_name),case_description=v(ca$case_description),
        case_location=v(ca$case_location),case_sector=v(ca$case_sector),
        implementation_status=v(ca$implementation_status),
        evidence=v(ca$supporting_evidence),confidence=v(ca$confidence),
        collective_action_confidence=v(ca$collective_action_confidence),
        collective_action_basis=v(ca$collective_action_basis),
        case_kind=v(ca$case_kind),
        fulltext_priority=z$fulltext_priority,
        stringsAsFactors=FALSE)
    }))
  }))
  if(is.null(flat))flat<-data.frame(record_id=character(),
    local_case_number=integer(),case_name=character(),case_description=character(),
    case_location=character(),case_sector=character(),
    implementation_status=character(),evidence=character(),confidence=character(),
    fulltext_priority=character(),collective_action_confidence=character(),
    collective_action_basis=character(),case_kind=character())
  overview<-data.frame(
    record_id=vapply(all,function(z)z$record_id,character(1)),
    case_evidence_status=vapply(all,function(z)z$case_evidence_status,character(1)),
    cases_identified=vapply(all,function(z)length(z$cases),integer(1)),
    fulltext_priority=vapply(all,function(z)z$fulltext_priority,character(1)),
    assessment_reason=vapply(all,function(z)z$assessment_reason,character(1)))
  # Never overwrite reviewer-edited CSVs on subsequent calls.
  for(spec in list(list(path=file.path(dest,"case_candidates.csv"),data=flat),
                   list(path=file.path(dest,"publication_assessments.csv"),data=overview))) {
    if(file.exists(spec$path)) {
      previous<-utils::read.csv(spec$path,stringsAsFactors=FALSE)
      if(!identical(previous,spec$data))
        stop("Existing W08 review CSV differs; preserve reviewer edits: ",spec$path)
    } else utils::write.csv(spec$data,spec$path,row.names=FALSE,na="")
  }
  final<-info
  final$status<-"PILOT_COMPLETE"
  final$responses_sha256<-sha(out)
  final$status_counts<-as.list(summary)
  final$candidate_cases<-nrow(flat)
  jsonlite::write_json(final,file.path(dest,"summary.json"),
                       auto_unbox=TRUE,pretty=TRUE)
  message("[W08] Completed ",n," abstract assessments; ",nrow(flat),
          " provisional cases. No W07 outputs altered: ",dest)
  invisible(dest)
}
