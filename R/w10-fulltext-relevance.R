#' Reassess W10 uncertain and ineligible case observations against full text
#'
#' Preserves the initial W10 ledger. Each append-only full-text judgement
#' concerns the original case observation, not the publication as a whole.
#' @param w09 Verified W09 retrieval directory.
#' @param extraction_dir Existing full-text extraction directory.
#' @param w10_run Existing complete first-pass W10 directory.
#' @param w04_prompt Archived W04 criteria.
#' @param model OpenAI Responses model.
#' @return Second-pass audit directory.
#' @export
verify_w10_fulltext_cases <- function(w09,extraction_dir,w10_run,w04_prompt,model="gpt-5.6-luna") {
  w09<-normalizePath(w09,mustWork=TRUE)
  extraction_dir<-normalizePath(extraction_dir,mustWork=TRUE)
  w10_run<-normalizePath(w10_run,mustWork=TRUE)
  w04_prompt<-normalizePath(w04_prompt,mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  previous<-file.path(w10_run,"w10_decisions.jsonl")
  src<-file.path(extraction_dir,"fulltext_case_registry_normalised.jsonl")
  first_manifest<-jsonlite::fromJSON(file.path(w10_run,"manifest.json"),simplifyVector=FALSE)
  if(!identical(first_manifest$decisions_sha256,sha(previous)) ||
     !identical(first_manifest$w09_registry_sha256,sha(src)) ||
     !identical(first_manifest$w04_prompt_sha256,sha(w04_prompt)))
    stop("First-pass W10 provenance differs from archived inputs.")
  lines<-function(path) {
    x<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  originals<-lines(src)
  decisions<-lines(previous)
  all_ids<-unlist(lapply(originals,function(z)
    vapply(seq_along(z$cases),function(j)paste0(z$record_id,"-case-",sprintf("%03d",j)),
           character(1))),use.names=FALSE)
  ids<-vapply(decisions,function(x)x$observation_id,character(1))
  if(anyDuplicated(ids)||!setequal(all_ids,ids))
    stop("First-pass W10 does not cover all case observations.")
  selected<-Filter(function(x)x$overall%in%c("uncertain","ineligible"),decisions)
  target<-file.path(w10_run,paste0("fulltext-pass-",substr(sha(previous),1,12),
                                  "-",gsub("[^A-Za-z0-9_-]","_",model)))
  dir.create(target,recursive=TRUE,showWarnings=FALSE)
  ledger<-file.path(target,"fulltext_decisions.jsonl")
  saved<-if(file.exists(ledger))lines(ledger) else list()
  done<-vapply(saved,function(x)x$observation_id,character(1))
  candidates<-vapply(selected,function(x)x$observation_id,character(1))
  if(anyDuplicated(done)||any(!done%in%candidates))stop("Invalid second-pass checkpoint.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY"))&&length(done)<length(selected))
    stop("OPENAI_API_KEY required for unfinished second-pass checks.")
  prompt<-paste(readLines(w04_prompt,warn=FALSE,encoding="UTF-8"),collapse="\n")
  rules_path<-system.file("w10","case_relevance_v2.txt",package="evidencelandscaper")
  if(!nzchar(rules_path))stop("Installed W10 v2 prompt missing.")
  rules_sha<-sha(rules_path)
  if(!identical(first_manifest$w10_rules_sha256,rules_sha))
    stop("First-pass W10 decisions use a different eligibility definition; run verify_cases with W10 v2 first.")
  instruction<-paste(readLines(rules_path,warn=FALSE,encoding="UTF-8"),collapse="\n")
  instruction<-paste0(instruction,
    "\n\nSECOND-PASS OUTPUT: JSON observation_id, criteria A/B/C/D ",
    "each with status, rationale, quote, anchor, and overall. ",
    "Read the case-specific original full text, not merely its summary. ",
    "All nonempty quotations must be verbatim from that supplied source. ",
    "\n\nARCHIVED W04 PROMPT (historical; W10 v2 B boundary governs):\n",prompt)
  lookup<-list()
  for(z in originals)for(j in seq_along(z$cases))
    lookup[[paste0(z$record_id,"-case-",sprintf("%03d",j))]]<-
      list(record_id=z$record_id,case=z$cases[[j]])
  # PDF/XML extraction frequently inserts line breaks and non-breaking spaces
  # within otherwise verbatim quotations. Match only after whitespace folding;
  # never use fuzzy semantic similarity or silently replace the model quote.
  fold_whitespace<-function(x) {
    x<-gsub("[\\p{Zs}\\t\\r\\n]+"," ",x,perl=TRUE)
    trimws(gsub("[[:space:]]+"," ",x,perl=TRUE))
  }
  for(x in selected[!candidates%in%done]) {
    id<-x$observation_id
    item<-lookup[[id]]
    payload<-w09_text_payload(w09,item$record_id,max_pages=0L)
    source_instruction<-if(identical(payload$format,"pdf"))
      "SOURCE FORMAT PDF: quote exact words as supplied and cite [PAGE n] anchors."
    else if(identical(payload$format,"xml"))
      "SOURCE FORMAT XML: use ONLY verbatim wording in the supplied [SECTION n] text. Cite [SECTION n], NEVER [PAGE n]. Do not copy a PDF rendering or reintroduce line-wrap hyphens."
    else
      "SOURCE FORMAT JSON: use ONLY verbatim wording in the supplied [TEXT BLOCK n] text. Cite [TEXT BLOCK n], NEVER [PAGE n]."
    body_instruction<-paste(instruction,source_instruction,sep="\n\n")
    input<-jsonlite::toJSON(list(observation_id=id,case=item$case,
      initial_assessment=x,source_format=payload$format,
      source_anchor_type=payload$anchors,fulltext=payload$text),
      auto_unbox=TRUE,null="null")
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",text=body_instruction))),
                 list(role="user",content=list(list(type="input_text",text=input)))),
      text=list(format=list(type="json_object")))
    # Output contract is separate from the approved case eligibility rules.
    output_contract<-paste0(
      "Return exactly one JSON OBJECT with observation_id equal to ",
      as.character(jsonlite::toJSON(id,auto_unbox=TRUE)),
      "; criteria must be a JSON OBJECT with exactly four keys A, B, C, D. ",
      "Each criterion must be an object with status (pass/fail/uncertain), ",
      "rationale (string), quote (string), anchor (string). ",
      "Include overall (eligible/ineligible/uncertain). ",
      "Do not return arrays, renamed criteria, markdown or extra cases.")
    body$input[[1L]]$content[[1L]]$text<-paste(
      body_instruction,output_contract,sep="\n\n")
    good<-FALSE
    for(attempt in seq_len(3L)) {
      parts<-character()
      z<-tryCatch({
        response<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        parts<-unlist(lapply(response$output,function(t)
          unlist(lapply(t$content,function(y)
            if(identical(y$type,"output_text"))y$text else NULL),
            use.names=FALSE)),use.names=FALSE)
        if(!length(parts))stop("No model output.")
        z<-jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE)
        if(!identical(z$observation_id,id)||!is.list(z$criteria)||
           !setequal(names(z$criteria),c("A","B","C","D")))
          stop("Unexpected case ID or criteria.")
        invalid_quotes<-character()
        statuses<-vapply(c("A","B","C","D"),function(k) {
          a<-z$criteria[[k]]
          if(is.null(a$status)||!a$status%in%c("pass","fail","uncertain")||
             !is.character(a$rationale)||length(a$rationale)!=1L||
             !nzchar(trimws(a$rationale))||
             !is.character(a$quote)||length(a$quote)!=1L||
             !is.character(a$anchor)||length(a$anchor)!=1L)
            stop("Invalid criterion fields.")
          if(nzchar(a$quote)&&!grepl(fold_whitespace(a$quote),
              fold_whitespace(payload$text),fixed=TRUE)) {
            invalid_quotes<<-c(invalid_quotes,k)
          }
          if(nzchar(a$quote)&&!nzchar(a$anchor))
            stop("Missing quotation source anchor.")
          if(nzchar(a$anchor)&&!grepl(a$anchor,payload$text,fixed=TRUE))
            stop("Unknown full-text source anchor.")
          a$status
        },character(1))
        expected<-if(any(statuses=="fail"))"ineligible" else
          if(all(statuses=="pass"))"eligible" else "uncertain"
        if(!identical(z$overall,expected))stop("Inconsistent overall result.")
        if(length(invalid_quotes))
          stop("Nonverbatim full-text quotation in criterion(s): ",
               paste(invalid_quotes,collapse=", "))
        z
      },error=function(e)e)
      if(!inherits(z,"error")){good<-TRUE;break}
      message("[W10 full text] ",id," attempt ",attempt,
              "/3: ",conditionMessage(z))
      # Preserve the rejected raw response for exact diagnosis without
      # altering accepted decisions or the W10 v2 eligibility rules.
      cat(as.character(jsonlite::toJSON(list(
        observation_id=id,attempt=attempt,error=conditionMessage(z),
        raw_model_output=if(length(parts))parts[[1L]] else NULL),
        auto_unbox=TRUE,null="null")),"\n",
        file=file.path(target,"invalid_fulltext_responses.jsonl"),
        append=TRUE,sep="")
      if(grepl("Unexpected case ID or criteria",conditionMessage(z),fixed=TRUE) ||
         grepl("Invalid criterion fields",conditionMessage(z),fixed=TRUE) ||
         grepl("Inconsistent overall result",conditionMessage(z),fixed=TRUE)) {
        body$input[[1L]]$content[[1L]]$text<-paste(
          body_instruction,output_contract,
          paste0("CORRECTION: Your last response was rejected: ",
            conditionMessage(z),
            ". Keep the same case and substantive W10 v2 criteria, ",
            "but return the exact required fields and the specified ",
            "observation_id. Do not invent evidence."),
          sep="\n\n")
      }
      # A failed quote must not be silently accepted or normalised.
      # A correction request must name the faulty criterion and require exact
      # substrings of the supplied document, without changing the case scope.
      if(grepl("Nonverbatim full-text quotation",conditionMessage(z),fixed=TRUE)) {
        cat(as.character(jsonlite::toJSON(list(observation_id=id,
          attempt=attempt,error=conditionMessage(z),
          response=if(exists("parts",inherits=FALSE)&&length(parts))parts[[1L]] else NULL),
          auto_unbox=TRUE,null="null")),"\n",
          file=file.path(target,"invalid_quote_attempts.jsonl"),
          append=TRUE,sep="")
        body$input[[1L]]$content[[1L]]$text<-paste0(
          body_instruction,"\n\nCORRECTION: Your preceding answer failed ",
          conditionMessage(z),". Quotations must be copied character-for-character ",
          "from the supplied fulltext, with identical punctuation and spacing. ",
          "Line wrapping in PDF extraction is normalised by the validator; ",
          "do not paraphrase or replace words. Use a short, exact passage ",
          "of at least 20 characters when possible. ",
          "If you cannot find an exact source quote, set quote and anchor ",
          "to empty strings and classify the criterion uncertain unless ",
          "other verified evidence conclusively supports the judgement. ",
          "Do not invent source text.")
      }
      if(attempt<3L)Sys.sleep(2^attempt)
    }
    if(!good) {
      # A persistent source-quote mismatch cannot establish eligibility.
      # Preserve it for human verification, but do not block all later cases.
      msg<-if(inherits(z,"error"))conditionMessage(z) else "Invalid W10 response"
      if(!grepl("Nonverbatim full-text quotation",msg,fixed=TRUE))
        stop("W10 full-text pass interrupted; completed cases are checkpointed.")
      z<-list(observation_id=id,overall="uncertain",
        criteria=stats::setNames(lapply(c("A","B","C","D"),function(k)
          list(status="uncertain",
               rationale="Automated full-text assessment could not provide a verifiable source quotation; manual review required.",
               quote="",anchor="")),c("A","B","C","D")),
        manual_review_required=TRUE,verification_error=msg)
      message("[W10 full text] ",id,
        " requires human source verification; preserved as uncertain.")
    }
    z$record_id<-item$record_id
    z$previous_decision<-x$overall
    z$fulltext_source_sha256<-payload$sha256
    z$fulltext_source_path<-payload$path
    z$w09_registry_sha256<-sha(src)
    z$first_pass_sha256<-sha(previous)
    z$w04_prompt_sha256<-sha(w04_prompt)
    z$model<-model
    cat(as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
        "\n",file=ledger,append=TRUE,sep="")
    message("[W10 full text] ",length(done)+1L,"/",length(candidates),
            " ",id,": ",x$overall," -> ",z$overall)
    done<-c(done,id)
  }
  final<-if(file.exists(ledger))lines(ledger) else list()
  if(length(final)!=length(selected))stop("Incomplete second-pass ledger.")
  audit<-data.frame(observation_id=vapply(final,function(z)z$observation_id,character(1)),
    previous=vapply(final,function(z)z$previous_decision,character(1)),
    fulltext=vapply(final,function(z)z$overall,character(1)))
  utils::write.csv(audit,file.path(target,"decision_transitions.csv"),row.names=FALSE)
  jsonlite::write_json(list(first_pass_sha256=sha(previous),
    w09_registry_sha256=sha(src),w04_prompt_sha256=sha(w04_prompt),
    w10_rules_sha256=rules_sha,
    second_pass_sha256=sha(ledger),model=model,
    assessed=length(final),source="verified W09 full texts"),
    file.path(target,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[W10 full text] Complete: ",file.path(target,"decision_transitions.csv"))
  invisible(target)
}
