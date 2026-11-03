#' Validate standalone full-text insights for an individual publication.
.w09_validate_insights <- function(x,id,fulltext) {
  sc<-function(v)if(is.character(v)&&length(v)==1L&&!is.na(v))v else ""
  if(!identical(sc(x$record_id),id)||!is.list(x$insights)||
     !sc(x$status)%in%c("insights_identified","no_relevant_insights","unclear")||
     !nzchar(sc(x$assessment_reason)))
    stop("Invalid full-text insights output")
  if(!identical(identical(x$status,"insights_identified"),
                length(x$insights)>0L))
    stop("Full-text insights status/count mismatch")
  flags<-list()
  for(v in x$insights) {
    if(!sc(v$insight_type)%in%c("barrier","enabler","empirical_finding",
                              "lesson","recommendation","future_prospect")||
       !sc(v$evidence_basis)%in%c("observed","author_interpretation",
         "stakeholder_report","proposed","unclear")||
       !nzchar(sc(v$summary))||!nzchar(sc(v$reason)))
      stop("Incomplete full-text insight")
    if(!is.null(v$case_ids)||!is.null(v$mechanism_id))
      stop("Insights response must not invent case or ontology IDs")
    quote<-sc(v$source_quote)
    anchor<-sc(v$source_anchor)
    causes<-character()
    if(!nzchar(trimws(quote)))causes<-c(causes,"missing_quote")
    if(!grepl("^\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]$",anchor))
      causes<-c(causes,"missing_or_invalid_anchor")
    at<-if(nzchar(anchor))regexpr(anchor,fulltext,fixed=TRUE)[[1L]] else -1L
    if(nzchar(anchor)&&at<1L)causes<-c(causes,"anchor_not_in_text")
    if(nzchar(quote)) {
      if(!grepl(quote,fulltext,fixed=TRUE))
        causes<-c(causes,"quote_not_verbatim_in_full_text")
      if(at>0L) {
        after<-substring(fulltext,at+nchar(anchor))
        next_at<-regexpr("\\[(?:PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",
                        after,perl=TRUE)[[1L]]
        segment<-if(next_at>0L)substr(after,1L,next_at-1L) else after
        if(!grepl(quote,segment,fixed=TRUE))
          causes<-c(causes,"quote_not_in_cited_anchor")
      }
    }
    if(length(causes))
      flags[[length(flags)+1L]]<-list(
        insight_type=sc(v$insight_type),
        reasons=as.list(unique(causes)),
        source_anchor=anchor,source_quote=quote)
  }
  flags
}

#' Resolve first-10% W08 abstract classifications for insights eligibility.
#'
#' Human adjudication takes precedence if a checksum-valid snapshot exists.
#' No case inference is made for W08-unvalidated or still-unclear publications.
.w09_first10_abstract_case_decisions <- function(w09,w08=NULL) {
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  manpath<-file.path(w09,"manifest.json")
  if(!file.exists(manpath))stop("W09 cohort manifest missing")
  wm<-jsonlite::fromJSON(manpath,simplifyVector=FALSE)
  if(!identical(wm$schema,"w09-first10-all-included-v1"))
    stop("Insights require the all-included first-10% W09 cohort")
  map<-dirname(dirname(dirname(w09)))
  if(is.null(w08)) {
    root<-file.path(map,"runs","W08")
    dirs<-if(dir.exists(root))list.dirs(root,recursive=FALSE,
                                       full.names=TRUE) else character()
    dirs<-dirs[vapply(dirs,function(d) {
      f<-file.path(d,"manifest.json")
      if(!file.exists(f))return(FALSE)
      z<-tryCatch(jsonlite::fromJSON(f,simplifyVector=FALSE),
                  error=function(e)NULL)
      !is.null(z)&&identical(z$source_w04,wm$source_w04)&&
        identical(z$source_w04_sha256,wm$source_w04_layer_sha256)
    },logical(1))]
    if(length(dirs)>1L) {
      adjudicated<-dirs[vapply(dirs,function(d) {
        hr<-file.path(d,"human_adjudication")
        length(list.dirs(hr,recursive=FALSE,full.names=TRUE)[
          file.exists(file.path(list.dirs(hr,recursive=FALSE,
            full.names=TRUE),"manifest.json"))])>0L
      },logical(1))]
      if(length(adjudicated)==1L)dirs<-adjudicated
    }
    if(length(dirs)!=1L)
      stop("Cannot identify exactly one W08 run matching the first-10% W04 ",
           "cohort (found ",length(dirs),"). Supply w08 explicitly.")
    w08<-dirs[[1L]]
  }
  w08<-normalizePath(w08,winslash="/",mustWork=TRUE)
  paths<-file.path(w08,c("manifest.json","summary.json",
                         "case_assessments.jsonl","prompt.txt","fields.csv"))
  if(any(!file.exists(paths)))stop("Incomplete W08 source for insights routing")
  m<-jsonlite::fromJSON(paths[[1L]],simplifyVector=FALSE)
  summ<-jsonlite::fromJSON(paths[[2L]],simplifyVector=FALSE)
  if(!identical(m$source_w04,wm$source_w04)||
     !identical(m$source_w04_sha256,wm$source_w04_layer_sha256)||
     !summ$status%in%c("PILOT_COMPLETE","PILOT_PARTIAL")||
     !identical(summ$responses_sha256,sha(paths[[3L]]))||
     !identical(m$prompt_sha256,sha(paths[[4L]]))||
     !identical(m$schema_sha256,sha(paths[[5L]])))
    stop("W08 cohort or assessment integrity does not match W09")
  z<-readjl(paths[[3L]])
  ids<-vapply(z,function(v)as.character(v$record_id),character(1))
  statuses<-vapply(z,function(v)as.character(v$case_evidence_status),character(1))
  if(anyDuplicated(ids)||any(!statuses%in%c("specific_case","multiple_cases",
                 "no_identifiable_case","unclear")))
    stop("Invalid or duplicated W08 abstract case assessments")
  all_ids<-as.character(unlist(wm$selected_record_ids,use.names=FALSE))
  selected<-as.character(unlist(m$selected_record_ids,use.names=FALSE))
  missing<-setdiff(selected,ids)
  if(!setequal(selected,all_ids)||any(!ids%in%selected)||
     (identical(summ$status,"PILOT_COMPLETE")&&length(missing))||
     (identical(summ$status,"PILOT_PARTIAL")&&
      (!setequal(missing,as.character(unlist(summ$unresolved_record_ids,
                    use.names=FALSE)))||
       length(ids)!=as.integer(summ$validated_records))))
    stop("W08 selected or validated IDs differ from first-10% W09 cohort")
  origin<-rep("W08_model",length(ids))
  human_dirs<-list.dirs(file.path(w08,"human_adjudication"),
                         recursive=FALSE,full.names=TRUE)
  human_dirs<-human_dirs[file.exists(file.path(human_dirs,"manifest.json"))]
  if(length(human_dirs)>1L)
    stop("Multiple W08 human adjudication snapshots: choose a single ",
         "authoritative decision set before full-text insights extraction")
  human_path<-""
  human_sha<-""
  if(length(human_dirs)==1L) {
    human_path<-human_dirs[[1L]]
    hm<-jsonlite::fromJSON(file.path(human_path,"manifest.json"),
                           simplifyVector=FALSE)
    hfile<-file.path(human_path,"effective_case_classifications.csv")
    if(!file.exists(hfile)||!identical(hm$source_w08_sha256,sha(paths[[3L]]))||
       !identical(hm$effective_case_classifications_sha256,sha(hfile)))
      stop("Human W08 adjudication checksum mismatch")
    h<-utils::read.csv(hfile,stringsAsFactors=FALSE)
    if(anyDuplicated(h$record_id)||!setequal(h$record_id,ids))
      stop("W08 human adjudication IDs do not match source")
    h<-h[match(ids,h$record_id),,drop=FALSE]
    if(!identical(as.character(h$model_case_status),statuses))
      stop("Human W08 original model-status mapping changed")
    statuses<-as.character(h$final_case_status)
    origin<-as.character(h$decision_origin)
    if(any(!statuses%in%c("specific_case","multiple_cases",
                         "no_identifiable_case","unclear")))
      stop("Invalid final W08 human case status")
    human_sha<-sha(hfile)
  }
  decisions<-data.frame(record_id=ids,final_abstract_case_status=statuses,
    decision_origin=origin,stringsAsFactors=FALSE)
  list(decisions=decisions,
    no_case_ids=sort(ids[statuses=="no_identifiable_case"]),
    unresolved_ids=sort(c(missing,ids[statuses=="unclear"])),
    w08=w08,w08_sha256=sha(paths[[3L]]),
    human_snapshot=human_path,human_sha256=human_sha)
}

#' Run Luna full-text mechanisms on all available first-10% publications
#'
#' Also runs a distinct evidence-anchored insights extraction for publications
#' with an abstract case classification of no_identifiable_case, using human
#' W08 adjudications when available. The LLM never sees abstract case status,
#' abstract mechanism codes or stage references. Full-text case detection is
#' part of the independent mechanism call, and can disagree with the abstract.
#' Uncertain or unvalidated abstract cases are not silently called negative.
#' All outputs are resumable and source-hash versioned.
#'
#' @param ontology_file CSV with all 30 approved mechanism IDs/definitions.
#'   If NULL, select the authoritative ontology file interactively.
#' @param w09 First-10% all-included W09 run; NULL discovers/prepares this run.
#' @param w08 Optional explicit matching W08 run for abstract-case routing.
#' @param max_records Maximum new mechanism and insight records in each pass;
#'   zero means all available, independently per pass.
#' @param model Luna model (gpt-5.6-luna by default).
#' @param mechanism_prompt_file Optional full-text mechanism prompt (.txt).
#' @param insights_prompt_file Optional independent insights prompt (.txt).
#' @return List of output directory paths, invisibly.
#' @export
run_ostara_first10_fulltext_inventory <- function(
    ontology_file=NULL,w09=NULL,w08=NULL,max_records=0L,
    model="gpt-5.6-luna",mechanism_prompt_file=NULL,
    insights_prompt_file=NULL) {
  if(!is.character(model)||length(model)!=1L||is.na(model)||
     !grepl("-luna$",model))
    stop("Only Luna models are permitted; Sol is not allowed")
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer; 0 means all")
  if(is.null(mechanism_prompt_file)) {
    if(!interactive())stop("Provide mechanism_prompt_file outside interactive R")
    message("[OSTARA FT] Select revised full-text mechanism prompt (.txt)")
    mechanism_prompt_file<-file.choose()
  }
  mechanism_prompt_file<-normalizePath(mechanism_prompt_file,
                                        winslash="/",mustWork=TRUE)
  message("[OSTARA FT] Mechanism prompt selected: ",
          basename(mechanism_prompt_file))
  if(is.null(ontology_file)) {
    if(!interactive())stop("Provide ontology_file outside interactive R")
    message("[OSTARA FT] Select approved mechanism ontology (.csv; 30 IDs)")
    ontology_file<-file.choose()
  }
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  message("[OSTARA FT] Ontology selected: ",basename(ontology_file))
  if(is.null(w09))w09<-prepare_w09_first10_all_included()
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  decisions<-.w09_first10_abstract_case_decisions(w09,w08=w08)
  if(!length(decisions$no_case_ids))
    stop("No W08 abstract-case-negative publications in this first-10% cohort")
  message("[OSTARA FT] W08 no-case abstracts: ",length(decisions$no_case_ids),
          " (",if(nzchar(decisions$human_sha256))"human adjudicated"
              else "model only",")")
  message("[OSTARA FT] Mechanisms and full-text case presence: ",
          "all available included full texts, regardless of W08 status")
  mechanism_dir<-extract_ostara_all_fulltext_mechanisms(
    w09=w09,ontology_file=ontology_file,max_records=max_records,
    model=model,prompt_file=mechanism_prompt_file)
  prompt<-if(is.null(insights_prompt_file))
    system.file("w09","fulltext_insights_v1.txt",
                package="evidencelandscaper")
    else normalizePath(insights_prompt_file,winslash="/",mustWork=TRUE)
  if(!nzchar(prompt)||!file.exists(prompt))
    stop("Package independent full-text insights prompt not found")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    x<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sources<-utils::read.csv(file.path(mechanism_dir,"source_inventory.csv"),
                            stringsAsFactors=FALSE)
  available<-as.character(sources$record_id[sources$availability=="available"])
  all_ids<-decisions$no_case_ids
  ids<-sort(intersect(all_ids,available))
  if(!length(ids)) {
    message("[OSTARA FT] No abstract-case-negative publication has readable ",
            "full text; no insights calls made.")
    reviews<-prepare_ostara_first10_fulltext_review(mechanism_dir)
    return(invisible(list(mechanisms=mechanism_dir,insights=NULL,
                          review=reviews$review_dir)))
  }
  src<-sources[match(ids,sources$record_id),,drop=FALSE]
  key<-substr(digest::digest(c(sha(prompt),
      decisions$w08_sha256,decisions$human_sha256,
      ids,src$source_sha256,model),algo="sha256"),1L,12L)
  dest<-file.path(w09,"fulltext_insights",paste0("v1-",key))
  mp<-file.path(dest,"manifest.json")
  manifest<-list(schema="ostara-first10-ft-insights-v1",
    w09=w09,source_mechanism_run=mechanism_dir,
    source_w08=decisions$w08,
    w08_assessments_sha256=decisions$w08_sha256,
    w08_human_adjudication_snapshot=decisions$human_snapshot,
    w08_human_classifications_sha256=decisions$human_sha256,
    insights_prompt_sha256=sha(prompt),model=model,
    selected_no_abstract_case_ids=as.list(all_ids),
    available_no_abstract_case_ids=as.list(ids),
    source_fulltext_sha256=as.list(as.character(src$source_sha256)))
  if(file.exists(mp)) {
    old<-jsonlite::fromJSON(mp,simplifyVector=FALSE)
    if(!identical(old,manifest))
      stop("Existing insights checkpoint provenance changed")
  } else {
    if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
      stop("Insights destination exists without a manifest")
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    utils::write.csv(decisions$decisions,file.path(dest,
      "w08_abstract_case_decisions.csv"),row.names=FALSE)
    utils::write.csv(data.frame(record_id=all_ids,
       fulltext_available=all_ids%in%ids),file.path(dest,
       "abstract_no_case_fulltext_inventory.csv"),row.names=FALSE)
    if(!file.copy(prompt,file.path(dest,"prompt.txt")))
      stop("Could not archive full-text insights prompt")
    jsonlite::write_json(manifest,mp,auto_unbox=TRUE,pretty=TRUE)
  }
  if(!identical(sha(file.path(dest,"prompt.txt")),sha(prompt)))
    stop("Saved insights prompt has changed")
  ledger<-file.path(dest,"insights_assessments.jsonl")
  done<-if(file.exists(ledger))readjl(ledger) else list()
  finished<-vapply(done,function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(finished)||any(!finished%in%ids))
    stop("Invalid existing insights checkpoint record IDs")
  todo<-setdiff(ids,finished)
  if(max_records>0L)todo<-head(todo,max_records)
  if(length(todo)&&!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("[FT insights] OPENAI_API_KEY missing; checkpoint prepared: ",dest)
  instructions<-paste(readLines(prompt,warn=FALSE,encoding="UTF-8"),
                      collapse="\n")
  failures<-file.path(dest,"insights_failed_attempts.jsonl")
  for(id in todo) {
    # Do not disclose W08 abstract decisions or mechanism codes to the model.
    payload<-tryCatch(w09_text_payload(w09,id,max_pages=0L),
                       error=function(e)e)
    if(inherits(payload,"error")) {
      message("[FT insights] Cannot read ",id,": ",conditionMessage(payload))
      next
    }
    if(!identical(unname(sha(payload$path)),unname(as.character(src$source_sha256[
                                   match(id,src$record_id)]))))
      stop("Full-text content changed during insights extraction: ",id)
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(
        list(role="system",content=list(list(type="input_text",
                                            text=instructions))),
        list(role="user",content=list(list(type="input_text",
          text=as.character(jsonlite::toJSON(
               list(record_id=id,fulltext=payload$text),
               auto_unbox=TRUE,null="null")))))),
      text=list(format=list(type="json_object")))
    result<-NULL
    for(attempt in seq_len(3L)) {
      z<-tryCatch({
        response<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        content<-unlist(lapply(response$output,function(item)
          lapply(item$content,function(p)
            if(identical(p$type,"output_text"))p$text else NULL)),
          use.names=FALSE)
        if(!length(content))stop("Luna returned no insights text")
        x<-jsonlite::fromJSON(content[[1L]],simplifyVector=FALSE)
        flags<-.w09_validate_insights(x,id,payload$text)
        x$evidence_review_required<-length(flags)>0L
        x$evidence_review_flags<-flags
        x
      },error=function(e)e)
      if(!inherits(z,"error")){result<-z;break}
      event<-list(record_id=id,attempt=attempt,
        error=conditionMessage(z))
      cat(as.character(jsonlite::toJSON(event,auto_unbox=TRUE)),
          "\n",file=failures,append=TRUE,sep="")
      message("[FT insights] ",id," attempt ",attempt,
              " failed: ",conditionMessage(z))
    }
    if(is.null(result)) {
      message("[FT insights] Failed ",id,
              "; previously successful records remain saved.")
      next
    }
    if(isTRUE(result$evidence_review_required)) {
      audit<-list(record_id=id,source_sha256=payload$sha256,
        evidence_review_required=TRUE,
        evidence_flags=result$evidence_review_flags)
      cat(as.character(jsonlite::toJSON(audit,auto_unbox=TRUE,null="null")),
          "\n",file=file.path(dest,"insights_evidence_review_flags.jsonl"),
          append=TRUE,sep="")
      message("[FT insights] ",id,": ",
              length(result$evidence_review_flags),
              " evidence issue(s) retained for human review.")
    }
    result$provenance<-list(model=model,
      source_path=payload$path,source_sha256=payload$sha256,
      source_format=payload$format,prompt_sha256=sha(prompt),
      abstract_decision_used_only_for_routing=TRUE,
      human_adjudication_sha256=decisions$human_sha256,
      source_identity_verified=FALSE)
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=ledger,append=TRUE,sep="")
    message("[FT insights] Completed ",id)
  }
  reviews<-prepare_ostara_first10_fulltext_review(mechanism_dir,dest)
  message("[OSTARA FT] Independent mechanisms: ",mechanism_dir)
  message("[OSTARA FT] Abstract-no-case insights: ",dest)
  message("[OSTARA FT] Human-review CSVs: ",reviews$review_dir)
  invisible(list(mechanisms=mechanism_dir,insights=dest,
                 review=reviews$review_dir))
}
