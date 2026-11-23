#' Pilot full-text attribution of approved OSTARA mechanisms to registered cases
#'
#' Processes exactly ten case-positive publications, prioritising publications
#' with multiple registered cases. Uses the original source full text, never
#' publication-level mechanism assignments. Input, ontology and source file
#' checksums are locked in a resumable manifest. No prior outputs are changed.
#'
#' @param cases_csv Retrospective case CSV (record_id, case_id, 01. Case name).
#' @param ontology_csv Approved v5 26-code ontology CSV.
#' @param fulltext_manifest_csv CSV with record_id,fulltext_path; paths can be
#'   absolute or relative to this CSV. PDF and plain TXT are supported.
#' @param output_dir New directory for locked pilot outputs; reuse to resume.
#' @param record_ids Optional exact ten record IDs; otherwise choose ten
#'   available case-positive publications, multi-case first, then record_id.
#' @param model Luna model name.
#' @param run_extraction FALSE (default) only validates inputs and creates the
#'   frozen pilot manifest. TRUE makes at most max_new paid calls.
#' @param max_new Maximum new publications assessed per invocation (0 = all).
#' @return Invisibly returns list of output paths and pilot status.
#' @export
run_ostara_case_mechanism_fulltext_pilot <- function(
    cases_csv,ontology_csv,fulltext_manifest_csv,output_dir,
    record_ids=NULL,model="gpt-5.6-luna",run_extraction=FALSE,max_new=10L) {
  if(!is.logical(run_extraction)||length(run_extraction)!=1L||
     is.na(run_extraction))stop("run_extraction must be TRUE or FALSE.")
  if(!is.character(model)||length(model)!=1L||is.na(model)||
     !grepl("-luna$",model))stop("Use an approved Luna model.")
  if(length(max_new)!=1L||is.na(max_new)||max_new<0L||
     max_new!=as.integer(max_new))stop("max_new must be an integer >= 0.")
  for(pkg in c("jsonlite","digest","httr2","pdftools"))
    if(!requireNamespace(pkg,quietly=TRUE))
      stop("Install required package: ",pkg)
  normal<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readcsv<-function(p)utils::read.csv(p,check.names=FALSE,
    stringsAsFactors=FALSE,colClasses="character",
    fileEncoding="UTF-8-BOM",na.strings=character())
  sc<-function(x)if(is.character(x)&&length(x)==1L&&!is.na(x))x else ""
  append_json<-function(p,z)
    cat(as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
        "\n",file=p,append=TRUE,sep="")
  read_jsonl<-function(p) {
    if(!file.exists(p))return(list())
    l<-readLines(p,warn=FALSE,encoding="UTF-8")
    l<-l[nzchar(trimws(l))]
    lapply(l,function(x)jsonlite::fromJSON(x,simplifyVector=FALSE))
  }
  cases_csv<-normal(cases_csv);ontology_csv<-normal(ontology_csv)
  fulltext_manifest_csv<-normal(fulltext_manifest_csv)
  cases<-readcsv(cases_csv);ont<-readcsv(ontology_csv)
  files<-readcsv(fulltext_manifest_csv)
  if(!all(c("record_id","case_id","01. Case name")%in%names(cases))||
     !nrow(cases)||anyDuplicated(cases$case_id)||
     any(!nzchar(trimws(cases$record_id)))||
     any(!nzchar(trimws(cases$case_id)))||
     any(!nzchar(trimws(cases[["01. Case name"]]))))
    stop("Invalid retrospective case register, case IDs or case names.")
  if(!identical(names(ont),
    c("mechanism_id","category","function","mechanism","definition"))||
    nrow(ont)!=26L||anyDuplicated(ont$mechanism_id)||
    any(!nzchar(trimws(as.matrix(ont)))))
    stop("Require the approved 26-code v5 ontology with exact five columns.")
  if(!identical(names(files),c("record_id","fulltext_path"))||
     anyDuplicated(files$record_id)||
     any(!nzchar(trimws(files$record_id)))||
     any(!nzchar(trimws(files$fulltext_path))))
    stop("Full-text manifest must have unique record_id,fulltext_path.")
  resolve_path<-function(p) {
    p<-path.expand(p)
    if(!grepl("^(/|[A-Za-z]:[/\\\\])",p))
      p<-file.path(dirname(fulltext_manifest_csv),p)
    normal(p)
  }
  files$fulltext_path<-vapply(files$fulltext_path,resolve_path,character(1))
  if(any(!tolower(tools::file_ext(files$fulltext_path))%in%c("pdf","txt")))
    stop("Only PDF and TXT full texts are supported.")
  files$source_sha256<-vapply(files$fulltext_path,sha,character(1))
  available<-intersect(unique(cases$record_id),files$record_id)
  count<-table(cases$record_id)
  if(is.null(record_ids)) {
    available<-available[order(-as.integer(count[available]),available)]
    if(length(available)<10L)
      stop("Need ten distinct case-positive publications with full texts; found ",
           length(available),".")
    ids<-head(available,10L)
  } else {
    ids<-as.character(record_ids)
    if(length(ids)!=10L||anyNA(ids)||anyDuplicated(ids)||
       any(!ids%in%available))
      stop("record_ids must identify exactly ten distinct case-positive publications present in the full-text manifest.")
  }
  selection<-files[match(ids,files$record_id),,drop=FALSE]
  selection$case_count<-as.integer(count[selection$record_id])
  selection<-selection[,c("record_id","case_count","fulltext_path","source_sha256")]
  prompt<-system.file("ostara","case_mechanism_attribution_prompt_v1.txt",
                      package="evidencelandscaper")
  if(!nzchar(prompt)||!file.exists(prompt))
    stop("Package is missing case_mechanism_attribution_prompt_v1.txt.")
  hash_manifest<-list(
    schema="ostara-case-mechanism-fulltext-pilot-v1",
    cases_sha256=sha(cases_csv),ontology_sha256=sha(ontology_csv),
    source_manifest_sha256=sha(fulltext_manifest_csv),
    prompt_sha256=sha(prompt),model=model,
    selected_ids=as.list(ids),
    fulltext_sha256=as.list(selection$source_sha256))
  if(!dir.exists(output_dir))dir.create(output_dir,recursive=TRUE)
  output_dir<-normal(output_dir)
  manifest_path<-file.path(output_dir,"manifest.json")
  if(file.exists(manifest_path)) {
    previous<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(previous,hash_manifest))
      stop("Pilot input or model changed. Use a new output_dir; do not overwrite prior results.")
  } else {
    if(length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
      stop("Output directory is populated but has no pilot manifest.")
    jsonlite::write_json(hash_manifest,manifest_path,
                         pretty=TRUE,auto_unbox=TRUE,null="null")
    utils::write.csv(selection,file.path(output_dir,"pilot_selection.csv"),
                     row.names=FALSE,na="")
    if(!isTRUE(file.copy(prompt,file.path(output_dir,"prompt.txt")))||
       !isTRUE(file.copy(ontology_csv,file.path(output_dir,"ontology.csv"))))
      stop("Could not archive pilot prompt or ontology.")
  }
  if(!identical(sha(file.path(output_dir,"prompt.txt")),sha(prompt))||
     !identical(sha(file.path(output_dir,"ontology.csv")),sha(ontology_csv)))
    stop("Archived prompt or ontology checksum mismatch.")
  make_text<-function(p) {
    if(tolower(tools::file_ext(p))=="pdf") {
      page_text<-pdftools::pdf_text(p)
      if(!length(page_text)||all(!nzchar(trimws(page_text))))
        stop("PDF yielded no readable text: ",p)
      paste(vapply(seq_along(page_text),function(i)
        paste0("[PAGE ",i,"]\n",page_text[[i]]),character(1)),
        collapse="\n\n")
    } else {
      txt<-paste(readLines(p,warn=FALSE,encoding="UTF-8"),collapse="\n")
      if(!nzchar(trimws(txt)))stop("Empty text source: ",p)
      if(!grepl("\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",txt))
        txt<-paste0("[TEXT BLOCK 1]\n",txt)
      txt
    }
  }
  # Validate readability of every selected source before any paid calls.
  # Short but non-empty texts are flagged for human inspection.
  preflight<-lapply(seq_len(nrow(selection)),function(i) {
    p<-selection$fulltext_path[[i]]
    parsed<-tryCatch(make_text(p),error=function(e)e)
    if(inherits(parsed,"error"))
      stop("Selected source unreadable: ",selection$record_id[[i]],
           " — ",conditionMessage(parsed))
    data.frame(record_id=selection$record_id[[i]],
      format=tolower(tools::file_ext(p)),
      extracted_characters=nchar(parsed),
      readability_flag=if(nchar(parsed)<1000L)
        "short_text_review_required" else "readable",
      stringsAsFactors=FALSE)
  })
  utils::write.csv(do.call(rbind,preflight),
    file.path(output_dir,"fulltext_readability_audit.csv"),
    row.names=FALSE,na="")
  validate_result<-function(z,id,fulltext) {
    expected<-cases$case_id[cases$record_id==id]
    if(!identical(sc(z$record_id),id)||
       !is.list(z$case_assessments)||
       length(z$case_assessments)!=length(expected))
      stop("Incorrect record ID or number of cases.")
    observed<-vapply(z$case_assessments,function(x)sc(x$case_id),character(1))
    if(anyDuplicated(observed)||!setequal(observed,expected))
      stop("Case IDs differ from supplied register.")
    if(!is.list(z$ontology_gaps)||
       !is.list(z$unattributed_publication_mechanisms))
      stop("Missing ontology gap or unattributed mechanism arrays.")
    allowed<-c("implemented","proposed_only","mentioned_only",
               "unclear","not_identified")
    rows<-list()
    for(ca in z$case_assessments) {
      assessments<-ca$mechanism_assessments
      if(!is.list(assessments)||length(assessments)!=26L)
        stop("Each case must assess exactly 26 ontology mechanisms.")
      codes<-vapply(assessments,function(x)sc(x$mechanism_id),character(1))
      if(anyDuplicated(codes)||!setequal(codes,ont$mechanism_id))
        stop("Missing, duplicate or unknown ontology mechanism IDs.")
      for(a in assessments) {
        status<-sc(a$attribution_status)
        if(!status%in%allowed)stop("Invalid attribution status.")
        quote<-sc(a$source_quote)
        anchor<-sc(a$source_anchor)
        reason<-sc(a$attribution_reason)
        if(!nzchar(trimws(reason)))stop("Attribution reason is missing.")
        if(status=="not_identified") {
          if(nzchar(quote)||nzchar(anchor))
            stop("Unidentified mechanisms must not carry source evidence.")
        } else {
          if(!nzchar(quote)||!nzchar(anchor))
            stop("Supported/unclear attribution lacks source evidence.")
          marker<-gregexpr("\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",
                          fulltext,perl=TRUE)[[1L]]
          anchors<-regmatches(fulltext,list(marker))[[1L]]
          where<-which(anchors==anchor)
          if(length(where)!=1L)
            stop("Source anchor not uniquely present in full text.")
          first<-marker[[where]]+nchar(anchor)
          last<-if(where<length(marker))marker[[where+1L]]-1L
                else nchar(fulltext)
          segment<-substr(fulltext,first,last)
          if(!grepl(quote,segment,fixed=TRUE))
            stop("Source quote is not verbatim within the stated anchor.")
        }
        shared<-a$shared_across_cases
        if(!is.logical(shared)||length(shared)!=1L||is.na(shared))
          stop("shared_across_cases must be true/false.")
        rows[[length(rows)+1L]]<-data.frame(
          record_id=id,case_id=ca$case_id,
          mechanism_id=a$mechanism_id,
          attribution_status=status,
          source_quote=quote,source_anchor=anchor,
          attribution_reason=reason,shared_across_cases=shared,
          stringsAsFactors=FALSE)
      }
    }
    # A shared flag is valid only when at least two distinct registered
    # cases have an evidenced link to the same mechanism.
    tab<-do.call(rbind,rows)
    for(code in unique(tab$mechanism_id)) {
      sub<-tab[tab$mechanism_id==code,,drop=FALSE]
      if(any(sub$shared_across_cases)) {
        evid<-sub$attribution_status%in%
          c("implemented","proposed_only","mentioned_only","unclear")
        if(sum(evid)<2L||
           any(sub$shared_across_cases & !evid))
          stop("Inconsistent shared_across_cases flag for ",code)
      }
    }
    tab
  }
  ledger<-file.path(output_dir,"validated_responses.jsonl")
  rawlog<-file.path(output_dir,"raw_responses.jsonl")
  errlog<-file.path(output_dir,"failed_assessments.jsonl")
  done<-read_jsonl(ledger)
  done_ids<-vapply(done,function(x)sc(x$record_id),character(1))
  if(anyDuplicated(done_ids)||any(!done_ids%in%ids))
    stop("Unexpected or duplicate checkpointed publication IDs.")
  todo<-setdiff(ids,done_ids)
  if(max_new>0L)todo<-head(todo,max_new)
  if(run_extraction&&length(todo)&&!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY is required; no model requests have been made.")
  prompt_text<-paste(readLines(prompt,warn=FALSE,encoding="UTF-8"),
                     collapse="\n")
  for(id in if(run_extraction)todo else character()) {
    path<-selection$fulltext_path[match(id,selection$record_id)]
    expected_hash<-selection$source_sha256[match(id,selection$record_id)]
    if(!identical(sha(path),expected_hash))
      stop("Full text changed after pilot selection: ",id)
    source<-tryCatch(make_text(path),error=function(e)e)
    if(inherits(source,"error")) {
      append_json(errlog,list(record_id=id,error=conditionMessage(source)))
      next
    }
    included<-cases[cases$record_id==id,,drop=FALSE]
    names_context<-grep("^([0][1-9]|1[0-2])\\. ",names(included),value=TRUE)
    names_context<-names_context[!grepl(" \\[(evidence|reporting status)\\]$",names_context)]
    payload<-list(record_id=id,
      cases=lapply(seq_len(nrow(included)),function(i) {
        context<-as.list(included[i,names_context,drop=FALSE])
        list(case_id=included$case_id[[i]],
             case_name=included[["01. Case name"]][[i]],
             reference_context=context)
      }),
      approved_ontology=lapply(seq_len(nrow(ont)),
                               function(i)as.list(ont[i,,drop=FALSE])),
      fulltext=source)
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(
        list(role="system",content=list(list(type="input_text",text=prompt_text))),
        list(role="user",content=list(list(type="input_text",
          text=as.character(jsonlite::toJSON(payload,
            auto_unbox=TRUE,null="null")))))),
      text=list(format=list(type="json_object")))
    success<-FALSE
    for(attempt in seq_len(3L)) {
      result<-tryCatch({
        response<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        parts<-unlist(lapply(response$output,function(x)
          lapply(x$content,function(y)
            if(identical(y$type,"output_text"))y$text else NULL)),
          use.names=FALSE)
        if(!length(parts))stop("Model returned no JSON output.")
        append_json(rawlog,list(record_id=id,attempt=attempt,raw=parts[[1L]]))
        parsed<-jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE)
        validated<-validate_result(parsed,id,source)
        list(response=parsed,rows=validated)
      },error=function(e)e)
      if(!inherits(result,"error")) {
        append_json(ledger,result$response)
        message("[OSTARA case-mechanism] Validated ",id)
        success<-TRUE
        break
      }
      append_json(errlog,list(record_id=id,attempt=attempt,
                              error=conditionMessage(result)))
    }
    if(!success)warning("Unvalidated case-mechanism assessment: ",id,
                        ". Raw responses retained for review.",call.=FALSE)
  }
  completed<-read_jsonl(ledger)
  all_rows<-lapply(completed,function(z) {
    id<-sc(z$record_id)
    path<-selection$fulltext_path[match(id,selection$record_id)]
    validate_result(z,id,make_text(path))
  })
  out<-if(length(all_rows))do.call(rbind,all_rows) else data.frame(
    record_id=character(),case_id=character(),mechanism_id=character(),
    attribution_status=character(),source_quote=character(),
    source_anchor=character(),attribution_reason=character(),
    shared_across_cases=logical(),stringsAsFactors=FALSE)
  long_csv<-file.path(output_dir,"case_mechanism_attributions.csv")
  utils::write.csv(out,long_csv,row.names=FALSE,na="")
  summary<-data.frame(record_id=ids,case_count=selection$case_count,
    validation_status=ifelse(ids%in%vapply(completed,function(z)
      sc(z$record_id),character(1)),"validated","pending_or_failed"),
    stringsAsFactors=FALSE)
  summary_csv<-file.path(output_dir,"pilot_status.csv")
  utils::write.csv(summary,summary_csv,row.names=FALSE,na="")
  review<-out[out$attribution_status!="not_identified",,drop=FALSE]
  review$human_decision<-rep("",nrow(review))
  review$human_notes<-rep("",nrow(review))
  review_file<-file.path(output_dir,"human_review_queue.csv")
  utils::write.csv(review,review_file,row.names=FALSE,na="")
  message("[OSTARA case-mechanism] ",sum(summary$validation_status=="validated"),
          "/10 publications completed. Pilot: ",output_dir)
  invisible(list(output_dir=output_dir,selection=file.path(output_dir,
    "pilot_selection.csv"),status=summary_csv,attributions=long_csv,
    human_review=review_file,
    completed=sum(summary$validation_status=="validated"),total=10L))
}
