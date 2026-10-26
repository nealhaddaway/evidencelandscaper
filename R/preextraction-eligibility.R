# Full-text case eligibility gate, independent of W09 detailed extraction.
# New future-run workflow only: existing W09 and W10 ledgers remain immutable.
run_preextraction_eligibility <- function(w09, record_ids, model="gpt-6-luna") {
  w09<-normalizePath(w09,mustWork=TRUE)
  rules<-system.file("w10","case_relevance_v2.txt",package="evidencelandscaper")
  if(!nzchar(rules))stop("Approved W10 v2 rules missing.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))stop("OPENAI_API_KEY required.")
  rule_sha<-digest::digest(file=rules,algo="sha256",serialize=FALSE)
  target<-file.path(w09,paste0("preextraction-eligibility-",substr(rule_sha,1,12),
     "-",gsub("[^A-Za-z0-9_-]","_",model)))
  dir.create(target,recursive=TRUE,showWarnings=FALSE)
  ledger<-file.path(target,"candidate_case_decisions.jsonl")
  items<-if(file.exists(ledger))lapply(Filter(nzchar,readLines(ledger,warn=FALSE)),
     jsonlite::fromJSON,simplifyVector=FALSE) else list()
  done<-vapply(items,function(z)z$record_id,character(1))
  if(anyDuplicated(done)||any(!done%in%record_ids))stop("Unexpected eligibility checkpoint IDs.")
  for(saved in items) {
    if(!identical(saved$prompt_sha256,rule_sha)||
       !identical(saved$model,model))
      stop("Saved pre-extraction eligibility decision uses changed rules or model.")
    current<-w09_text_payload(w09,saved$record_id,max_pages=0L)
    if(!identical(saved$source_sha256,current$sha256))
      stop("Full-text source changed after a prior eligibility decision: ",saved$record_id)
  }
  instructions<-paste(readLines(rules,warn=FALSE,encoding="UTF-8"),collapse="\n")
  instructions<-paste0(instructions,
    "\n\nPRE-EXTRACTION GATE. Identify EACH distinct real-world candidate ",
    "collective action case in this publication, without performing detailed ",
    "case extraction. Do not omit uncertain or apparently ineligible candidates. ",
    "Give each case a concise discriminating description and assess A,B,C,D ",
    "using only this full text. Reply with JSON {record_id, cases:[{case_id, ",
    "description, criteria:{A:{status,rationale,quote,anchor},B:{...},",
    "C:{...},D:{...}},overall}]} where case_id is 001,002,... in ",
    "order of first occurrence. For each criterion return pass, fail, or ",
    "uncertain. overall eligible only if all pass, ineligible if any fail, ",
    "otherwise uncertain. Quotes must be exact substrings and anchors must ",
    "occur in the supplied document. No cases is represented by [].")
  fold<-function(x)trimws(gsub("[[:space:]]+"," ",x,perl=TRUE))
  for(id in record_ids[!record_ids%in%done]) {
    payload<-w09_text_payload(w09,id,max_pages=0L)
    source_rule<-paste0("Source format: ",payload$format,
      "; anchor type: ",payload$anchors,
      ". Use only source-native anchors, never invent page numbers.")
    raw<-jsonlite::toJSON(list(record_id=id,fulltext=payload$text),
                          auto_unbox=TRUE,null="null")
    body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(list(role="system",content=list(list(type="input_text",
        text=paste(instructions,source_rule,sep="\n\n")))),
        list(role="user",content=list(list(type="input_text",text=raw)))),
      text=list(format=list(type="json_object")))
    accepted<-FALSE
    for(attempt in seq_len(3L)) {
      parts<-character()
      result<-tryCatch({
        rsp<-httr2::request("https://api.openai.com/v1/responses")|>
          httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY"))|>
          httr2::req_body_json(body,auto_unbox=TRUE)|>
          httr2::req_timeout(300)|>httr2::req_perform()|>
          httr2::resp_body_json(simplifyVector=FALSE)
        parts<-unlist(lapply(rsp$output,function(t)
          unlist(lapply(t$content,function(y)
            if(identical(y$type,"output_text"))y$text else NULL),
            use.names=FALSE)),use.names=FALSE)
        if(!length(parts))stop("No model output.")
        z<-jsonlite::fromJSON(parts[[1L]],simplifyVector=FALSE)
        if(!identical(z$record_id,id)||!is.list(z$cases))
          stop("Incorrect record ID or missing cases array.")
        caseids<-vapply(z$cases,function(x)x$case_id,character(1))
        if(!identical(caseids,sprintf("%03d",seq_along(caseids))))
          stop("Case IDs must be sequential and unique.")
        for(ca in z$cases) {
          if(!is.character(ca$description)||!nzchar(trimws(ca$description))||
             !is.list(ca$criteria)||
             !setequal(names(ca$criteria),c("A","B","C","D")))
            stop("Invalid candidate case structure.")
          statuses<-vapply(c("A","B","C","D"),function(k) {
            a<-ca$criteria[[k]]
            if(!is.list(a)||!is.character(a$status)||length(a$status)!=1L||
               !a$status%in%c("pass","fail","uncertain")||
               !is.character(a$rationale)||length(a$rationale)!=1L||
               !nzchar(trimws(a$rationale))||
               !is.character(a$quote)||length(a$quote)!=1L||
               !is.character(a$anchor)||length(a$anchor)!=1L)
              stop("Invalid eligibility criterion fields.")
            if(identical(a$status,"pass") &&
               (!nzchar(a$quote)||!nzchar(a$anchor)))
              stop("Passing eligibility criterion requires a source-grounded quotation and anchor.")
            if(xor(nzchar(a$quote),nzchar(a$anchor)))
              stop("Quote and source anchor must both be present or absent.")
            if(nzchar(a$anchor)) {
              pos<-regexpr(a$anchor,payload$text,fixed=TRUE)[[1L]]
              if(pos<0L)stop("Unknown source anchor.")
              start<-pos+nchar(a$anchor)
              remainder<-substring(payload$text,start)
              next_anchor<-regexpr(
                "\\[(PAGE|SECTION|TEXT BLOCK) [0-9]+\\]",
                remainder,perl=TRUE)[[1L]]
              section<-if(next_anchor<0L)remainder else
                substr(remainder,1L,next_anchor-1L)
              if(!grepl(fold(a$quote),fold(section),fixed=TRUE))
                stop("Quotation cannot be verified within its source anchor.")
            }
            a$status
          },character(1))
          expected<-if(any(statuses=="fail"))"ineligible" else
            if(all(statuses=="pass"))"eligible" else "uncertain"
          if(!identical(ca$overall,expected))
            stop("Inconsistent eligibility outcome.")
        }
        z
      },error=function(e)e)
      if(!inherits(result,"error")){accepted<-TRUE;break}
      cat(as.character(jsonlite::toJSON(list(record_id=id,attempt=attempt,
        error=conditionMessage(result),response=if(length(parts))parts[[1L]] else NULL),
        auto_unbox=TRUE,null="null")),"\n",
        file=file.path(target,"invalid_responses.jsonl"),append=TRUE,sep="")
      message("[Pre-extraction eligibility] ",id," attempt ",attempt,
        "/3: ",conditionMessage(result))
      if(attempt<3L) {
        body$input[[1L]]$content[[1L]]$text<-paste0(instructions,
          "\n\n",source_rule,
          "\n\nCORRECT OUTPUT: Earlier response rejected because ",
          conditionMessage(result),
          ". Return ONE JSON object with record_id exactly ",
          id," and cases as an array. Use case_id as a three-character ",
          "STRING: 001, 002, etc. For each case include description; ",
          "criteria as an OBJECT keyed A, B, C, D; each has status, ",
          "rationale, verbatim quote and valid source anchor; and overall. ",
          "Do not alter the eligibility definition or invent evidence.")
      }
    }
    if(!accepted)stop("Pre-extraction gate interrupted; completed documents checkpointed.")
    result$source_sha256<-payload$sha256
    result$prompt_sha256<-rule_sha
    result$model<-model
    cat(as.character(jsonlite::toJSON(result,auto_unbox=TRUE,null="null")),
        "\n",file=ledger,append=TRUE,sep="")
    message("[Pre-extraction eligibility] ",id," complete.")
  }
  all<-lapply(Filter(nzchar,readLines(ledger,warn=FALSE)),
              jsonlite::fromJSON,simplifyVector=FALSE)
  evidence_fields<-unlist(lapply(c("A","B","C","D"),function(k)
    paste0(k,c("_status","_rationale","_quote","_anchor"))),
    use.names=FALSE)
  cases<-unlist(lapply(all,function(z)lapply(z$cases,function(ca) {
    fields<-unlist(lapply(c("A","B","C","D"),function(k) {
      a<-ca$criteria[[k]]
      vals<-list(a$status,a$rationale,a$quote,a$anchor)
      stats::setNames(vals,paste0(k,
        c("_status","_rationale","_quote","_anchor")))
    }),recursive=FALSE)
    do.call(data.frame,c(list(record_id=z$record_id,case_id=ca$case_id,
      description=ca$description,decision=ca$overall,
      source_sha256=z$source_sha256),fields,
      list(stringsAsFactors=FALSE)))
  })),recursive=FALSE)
  tab<-if(length(cases))do.call(rbind,cases) else {
    blank<-data.frame(record_id=character(),case_id=character(),
      description=character(),decision=character(),source_sha256=character())
    for(nm in evidence_fields)blank[[nm]]<-character()
    blank
  }
  utils::write.csv(tab,file.path(target,"candidate_case_eligibility.csv"),
                   row.names=FALSE)
  review<-tab[tab$decision=="uncertain",
               setdiff(names(tab),"decision"),drop=FALSE]
  review$decision<-rep("",nrow(review))
  review$rationale<-rep("",nrow(review))
  review_path<-file.path(target,"manual_eligibility_review.csv")
  if(!file.exists(review_path))
    utils::write.csv(review,review_path,row.names=FALSE)
  jsonlite::write_json(list(schema="preextraction-eligibility-v1",
    w09=w09,prompt_sha256=rule_sha,model=model,
    records=length(all),cases=nrow(tab),
    ledger_sha256=digest::digest(file=ledger,algo="sha256",serialize=FALSE)),
    file.path(target,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(target)
}

#' Finalise human adjudications of uncertain full-text candidate cases
#'
#' Review the generated manual_eligibility_review.csv, entering eligible or
#' ineligible and a substantive rationale for EVERY uncertain candidate.
#' Original model decisions are retained; a separate reviewed final registry
#' is created without re-running any model calls.
finalise_preextraction_eligibility <- function(gate) {
  gate<-normalizePath(gate,mustWork=TRUE)
  raw<-file.path(gate,"candidate_case_eligibility.csv")
  review<-file.path(gate,"manual_eligibility_review.csv")
  ledger<-file.path(gate,"candidate_case_decisions.jsonl")
  if(!all(file.exists(c(raw,review,ledger))))
    stop("Pre-extraction eligibility gate is incomplete.")
  orig<-utils::read.csv(raw,stringsAsFactors=FALSE,check.names=FALSE,
                        colClasses=c(case_id="character"))
  correction<-utils::read.csv(review,stringsAsFactors=FALSE,check.names=FALSE,
                                na.strings=NULL,colClasses=c(case_id="character"))
  required<-c(setdiff(names(orig),"decision"),"decision","rationale")
  if(!identical(names(correction),required))
    stop("Unexpected manual review columns.")
  key<-function(x)paste(x$record_id,x$case_id,sep="/")
  uncertain<-orig[orig$decision=="uncertain",,drop=FALSE]
  if(anyDuplicated(key(correction))||
     !setequal(key(correction),key(uncertain)))
    stop("Manual review must contain exactly the uncertain case IDs.")
  correction<-correction[match(key(uncertain),key(correction)),,drop=FALSE]
  if(nrow(correction)&&
     (any(!correction$decision%in%c("eligible","ineligible"))||
      any(!nzchar(trimws(correction$rationale)))))
    stop("Resolve every uncertain case with eligible/ineligible and a rationale.")
  fixed_fields<-setdiff(names(correction),c("decision","rationale"))
  if(nrow(correction)&&
     !identical(correction[fixed_fields],uncertain[fixed_fields]))
    stop("Manual review changed original case identities or model evidence.")
  final<-orig
  final$initial_decision<-final$decision
  final$decision_source<-rep("model",nrow(final))
  final$adjudication_rationale<-rep("",nrow(final))
  i<-match(key(uncertain),key(final))
  if(length(i)) {
    final$decision[i]<-correction$decision
    final$decision_source[i]<-"human"
    final$adjudication_rationale[i]<-correction$rationale
  }
  if(any(final$decision=="uncertain"))
    stop("Final eligibility registry contains unresolved cases.")
  destination<-file.path(gate,"final_case_eligibility.csv")
  checksum<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(file.exists(destination)) {
    old<-utils::read.csv(destination,stringsAsFactors=FALSE,na.strings=NULL,
                         colClasses=c(case_id="character"))
    if(!identical(old,final))
      stop("An existing sealed eligibility registry differs. Never overwrite.")
  } else utils::write.csv(final,destination,row.names=FALSE)
  manifestpath<-file.path(gate,"final_review_manifest.json")
  provenance<-list(schema="preextraction-final-v1",
    original_ledger_sha256=checksum(ledger),
    original_audit_sha256=checksum(raw),
    manual_review_sha256=checksum(review),
    final_audit_sha256=checksum(destination),
    adjudicated=nrow(correction),eligible=sum(final$decision=="eligible"),
    ineligible=sum(final$decision=="ineligible"))
  if(file.exists(manifestpath)) {
    prior<-jsonlite::fromJSON(manifestpath,simplifyVector=FALSE)
    for(k in names(provenance))
      if(!identical(prior[[k]],provenance[[k]]))
        stop("Existing final review provenance differs: ",k)
  } else {
    jsonlite::write_json(provenance,manifestpath,
                        auto_unbox=TRUE,pretty=TRUE)
  }
  invisible(destination)
}
