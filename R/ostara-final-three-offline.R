#' Offline repair and strict revalidation of three OSTARA case extractions
#'
#' Uses the exact validated JSON envelope from W09. Never sends model requests.
#' Repairs only mechanically justified status/case normalization, and imports
#' supporting evidence from a different archived response ONLY when that
#' response has exactly the same answer and source-verified quotations. Does
#' not infer unsupported findings or alter locked prompt/schema.
#' @param map_path OSTARA map root.
#' @param commit FALSE previews recovery; TRUE appends validated repaired records.
#' @return Data frame with publication-level outcomes, invisibly.
#' @export
recover_ostara_final_three_offline <- function(map_path,commit=FALSE) {
  if(!is.logical(commit)||length(commit)!=1L||is.na(commit))
    stop("commit must be TRUE or FALSE.")
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  jl<-function(p){
    if(!file.exists(p))return(list())
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
           simplifyVector=FALSE)
  }
  as_id<-function(x)if(is.character(x$record_id)&&length(x$record_id)==1L)
    x$record_id else ""
  map_path<-norm(map_path)
  w09<-norm(file.path(map_path,"runs","W09","ft-mechanism-stage-remaining90"))
  pilot<-norm(file.path(map_path,"reports","ostara_case_pilots",
                         "case_pilot_20_20261113"))
  prompt<-norm(file.path(pilot,"prompt.txt"))
  schema<-norm(file.path(pilot,"case_schema.csv"))
  pilot_manifest<-jsonlite::fromJSON(file.path(pilot,"batch_manifest.json"),
                                      simplifyVector=FALSE)
  if(!identical(sha(prompt),pilot_manifest$staged_prompt_sha256)||
     !identical(sha(schema),pilot_manifest$schema_sha256))
    stop("Locked prompt/schema hashes disagree with pilot manifest.")
  spec<-utils::read.csv(schema,stringsAsFactors=FALSE,check.names=FALSE,
                        fileEncoding="UTF-8-BOM")
  spec<-spec[!grepl("mechanisms of the collective action",spec$Question,
                     ignore.case=TRUE),,drop=FALSE]
  spec$field_id<-sprintf("field_%03d",seq_len(nrow(spec)))
  dest<-norm(file.path(w09,"case_data_extraction",
    paste0("csv-",substr(sha(schema),1,12),"-prompt-",
           substr(sha(prompt),1,12),"-model-gpt-5.6-luna")))
  extraction_manifest<-jsonlite::fromJSON(file.path(dest,"manifest.json"),
                                            simplifyVector=FALSE)
  if(!identical(extraction_manifest$prompt_sha256,sha(prompt))||
     !identical(extraction_manifest$csv_sha256,sha(schema)))
    stop("W09 extraction manifest differs from locked inputs.")
  ledger_file<-file.path(dest,"case_data.jsonl")
  invalid_file<-file.path(dest,"invalid_responses.jsonl")
  target<-c("work-4cfa0cacd9a36144",
            "work-6a77cf0d62cba451",
            "work-9c31363ab815eff1")
  ledger<-jl(ledger_file)
  done<-vapply(ledger,as_id,character(1))
  if(anyDuplicated(done))stop("Existing extraction ledger has duplicate IDs.")
  archived<-jl(invalid_file)
  # This is the identical W09 schema/anchor/quote checker. Its source rules
  # are copied unchanged to avoid weakening the validation contract.
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
            "inter-governmental or multinational","systemic")
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

  compact<-function(z){
    z<-iconv(z,from="",to="ASCII//TRANSLIT",sub="")
    trimws(gsub("[^[:alnum:]]+"," ",tolower(z)))
  }
  evidence_in_source<-function(e,text){
    if(!is.list(e)||!is.character(e$quote)||length(e$quote)!=1L||
       !nzchar(trimws(e$quote))||!is.character(e$anchor)||
       length(e$anchor)!=1L||
       !grepl("^\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]$",e$anchor)||
       !grepl(e$anchor,text,fixed=TRUE))return(FALSE)
    at<-regexpr(e$anchor,text,fixed=TRUE)[[1L]]
    after<-substring(text,at+nchar(e$anchor))
    next_anchor<-regexpr("\\[(?:PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",
                          after,perl=TRUE)[[1L]]
    segment<-if(next_anchor>0L)substr(after,1L,next_anchor-1L) else after
    grepl(compact(e$quote),compact(segment),fixed=TRUE)
  }
  getvals<-function(f,form){
    if(!is.list(f))return(NULL)
    if(grepl("multiple",form,fixed=TRUE)) {
      if(is.null(f$values))return(character())
      return(as.character(unlist(f$values,use.names=FALSE)))
    }
    if(is.null(f$value))return(character())
    if(!is.character(f$value))return(NULL)
    f$value
  }
  # Only borrow evidence for the same case, same field, and exactly the same
  # nonempty answer. Imported quotations must match their source segment.
  borrow<-function(candidates,which,case_i,field,answer,form,text){
    out<-list()
    for(j in seq_along(candidates)){
      if(j==which)next
      z<-candidates[[j]]
      if(!is.list(z$cases)||length(z$cases)<case_i)next
      othercase<-z$cases[[case_i]]
      thiscase<-candidates[[which]]$cases[[case_i]]
      if(!is.list(othercase)||!is.character(othercase$case_id)||
         !identical(othercase$case_id,thiscase$case_id)||
         !is.list(othercase$fields))next
      f<-othercase$fields[[field]]
      if(!is.list(f)||!identical(getvals(f,form),answer)||
         !is.list(f$evidence)||!length(f$evidence))next
      if(all(vapply(f$evidence,evidence_in_source,logical(1),text=text)))
        out<-c(out,f$evidence)
    }
    if(!length(out))return(NULL)
    keys<-vapply(out,function(e)paste(e$anchor,e$quote,sep="\n"),
                 character(1))
    out[!duplicated(keys)]
  }
  report<-list()
  staged<-list()
  for(id in target){
    if(id%in%done){
      report[[length(report)+1L]]<-data.frame(
        record_id=id,result="already_validated",candidate_attempt=NA_integer_,
        edits=0L,review_flags=0L,reason="")
      next
    }
    hist<-Filter(function(z)identical(as_id(z),id)&&
       is.character(z$raw)&&length(z$raw)==1L,archived)
    if(!length(hist)){
      report[[length(report)+1L]]<-data.frame(
        record_id=id,result="unresolved",candidate_attempt=NA_integer_,
        edits=0L,review_flags=NA_integer_,
        reason="No raw archived model responses")
      next
    }
    source<-tryCatch(w09_text_payload(w09,id,max_pages=0L),
                      error=function(e)e)
    if(inherits(source,"error"))stop(id,": ",conditionMessage(source))
    source_selection<-jl(file.path(dest,"source_documents.jsonl"))
    old_source<-Filter(function(z)identical(as_id(z),id),source_selection)
    if(length(old_source)!=1L||
       !identical(old_source[[1L]]$sha256,source$sha256))
      stop("Source identity/hash differs from archived input for ",id)
    parsed<-lapply(hist,function(h)tryCatch(
      jsonlite::fromJSON(h$raw,simplifyVector=FALSE),
      error=function(e)NULL))
    best<-NULL
    failures<-character()
    for(i in seq_along(parsed)) {
      z<-parsed[[i]]
      if(!is.list(z)||!identical(as_id(z),id)||!is.list(z$cases))next
      candidate<-unserialize(serialize(z,NULL))
      edits<-list()
      impossible<-FALSE
      for(j in seq_along(candidate$cases)) {
        case<-candidate$cases[[j]]
        if(!is.list(case)||!is.list(case$fields)){
          impossible<-TRUE;break
        }
        for(k in seq_len(nrow(spec))) {
          key<-spec$field_id[[k]]
          f<-case$fields[[key]]
          if(!is.list(f)){impossible<-TRUE;break}
          form<-spec$Answer_form[[k]]
          vals<-getvals(f,form)
          if(is.null(vals)){impossible<-TRUE;break}
          ev<-f$evidence
          if(is.null(ev)&&!length(vals)) {
            f$evidence<-list()
            edits[[length(edits)+1L]]<-list(case=j,field=key,
              operation="empty_missing_evidence_array")
          }
          # Only a three-letter country code can be case-normalized.
          q<-tolower(trimws(spec$Question[[k]]))
          if(grepl("^where did the case take place \\(country\\)",q)&&
             length(vals)&&all(grepl("^[a-z]{3}$",vals))){
            if(grepl("multiple",form,fixed=TRUE)) {
              f$values<-as.list(toupper(vals))
            } else f$value<-toupper(vals)
            edits[[length(edits)+1L]]<-list(case=j,field=key,
              operation="uppercase_three_letter_country_code")
            vals<-toupper(vals)
          }
          if(length(vals)&&(!is.list(f$evidence)||!length(f$evidence))) {
            replacement<-borrow(parsed,i,j,key,vals,form,source$text)
            if(!is.null(replacement)) {
              f$evidence<-replacement
              edits[[length(edits)+1L]]<-list(case=j,field=key,
                operation="borrow_exact_answer_source_verified_evidence",
                evidence_count=length(replacement))
            }
          }
          status<-f$reporting_status
          valid_status<-is.character(status)&&length(status)==1L&&
            !is.na(status)&&status%in%
              c("reported","not_reported","explicitly_none")
          if(!valid_status||
             (identical(status,"not_reported")&&length(vals))) {
            if(length(vals)&&is.list(f$evidence)&&length(f$evidence)&&
               all(vapply(f$evidence,evidence_in_source,logical(1),
                          text=source$text))) {
              f$reporting_status<-"reported"
              edits[[length(edits)+1L]]<-list(case=j,field=key,
                operation="status_reported_with_exact_source_evidence")
            } else if(!length(vals)&&is.list(f$evidence)&&!length(f$evidence)) {
              f$reporting_status<-"not_reported"
              edits[[length(edits)+1L]]<-list(case=j,field=key,
                operation="status_not_reported_for_empty_answer")
            }
          }
          case$fields[[key]]<-f
        }
        if(impossible)break
        candidate$cases[[j]]<-case
      }
      if(impossible)next
      flags<-character();reason<-NULL
      ok<-tryCatch({
        withCallingHandlers(check_result(candidate,id,source$text),
          warning=function(w) {
            flags<<-c(flags,conditionMessage(w))
            invokeRestart("muffleWarning")
          })
        TRUE
      },error=function(e){reason<<-conditionMessage(e);FALSE})
      if(!ok){failures<-c(failures,paste0("attempt_",i,": ",reason));next}
      # All previously existing evidence may still carry review flags. Keep
      # these visible; never claim they have been independently verified.
      score<-c(length(flags),length(edits))
      if(is.null(best)||
         score[[1L]]<best$score[[1L]]||
         (score[[1L]]==best$score[[1L]]&&
          score[[2L]]<best$score[[2L]]))
        best<-list(candidate=candidate,edits=edits,flags=flags,
                   attempt=i,score=score,source_sha256=source$sha256)
    }
    if(is.null(best)){
      report[[length(report)+1L]]<-data.frame(
        record_id=id,result="unresolved",candidate_attempt=NA_integer_,
        edits=0L,review_flags=NA_integer_,
        reason=paste(tail(failures,3L),collapse=" | "))
    }else {
      staged[[id]]<-best
      report[[length(report)+1L]]<-data.frame(
        record_id=id,result="recoverable",candidate_attempt=best$attempt,
        edits=length(best$edits),review_flags=length(best$flags),reason="")
    }
  }
  results<-do.call(rbind,report)
  outdir<-file.path(map_path,"reports","ostara_case_fulltext_locked",
                    "offline_recovery_final_three")
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  report_path<-file.path(outdir,"recovery_status.csv")
  utils::write.csv(results,report_path,row.names=FALSE,na="")
  # Preserve complete pre-repair JSON and edits for audit (raw API responses
  # remain untouched). Each record is appended only after strict validation.
  auditfile<-file.path(dest,"targeted_offline_recovery_audit.jsonl")
  if(commit&&length(staged)) {
    present<-vapply(jl(ledger_file),as_id,character(1))
    for(id in names(staged)) {
      if(id%in%present)stop("Concurrent checkpoint update detected: ",id)
      z<-staged[[id]]
      audit<-list(record_id=id,method="source_verified_offline_recovery_v1",
        archived_response_index=z$attempt,
        source_sha256=z$source_sha256,prompt_sha256=sha(prompt),
        schema_sha256=sha(schema),edits=z$edits,
        evidence_review_required=length(z$flags)>0L,
        evidence_flags=as.list(z$flags))
      cat(as.character(jsonlite::toJSON(audit,auto_unbox=TRUE,null="null")),
          "\n",file=auditfile,append=TRUE,sep="")
      cat(as.character(jsonlite::toJSON(z$candidate,auto_unbox=TRUE,null="null")),
          "\n",file=ledger_file,append=TRUE,sep="")
      message("[OSTARA recovery] Saved ",id," after ",
              length(z$edits)," auditable repair(s).")
      present<-c(present,id)
    }
  }
  message("[OSTARA recovery] ",sum(results$result=="recoverable"),
    " recoverable; ",sum(results$result=="unresolved")," unresolved; ",
    if(commit)"validated candidates appended" else "dry run only",
    ". Report: ",report_path)
  invisible(results)
}
