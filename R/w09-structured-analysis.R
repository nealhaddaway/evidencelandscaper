# Internal: prepare a verified W09 full-text payload (XML > JSON > PDF).
w09_text_payload <- function(w09,id,max_pages=0L) {
 candidates<-c(file.path(w09,"structured",paste0(id,".xml")),
               file.path(w09,"structured",paste0(id,".json")),
               file.path(w09,"pdfs",paste0(id,".pdf")))
 path<-candidates[file.exists(candidates)]
 if(!length(path))stop("No full-text file available for ",id)
 path<-path[[1L]]
 ext<-tolower(tools::file_ext(path))
 if(ext=="pdf") {
   if(!requireNamespace("pdftools",quietly=TRUE))
     stop("Install pdftools to process PDF full texts.")
   pages<-pdftools::pdf_text(path)
   if(max_pages>0L)pages<-head(pages,max_pages)
   if(!length(pages)||!any(nzchar(trimws(pages))))
     stop("PDF has no readable text for ",id)
   content<-paste(vapply(seq_along(pages),function(i)
     paste0("[PAGE ",i,"]\n",pages[[i]]),character(1)),collapse="\n\n")
   anchors<-"PDF page numbers"
   n<-length(pages)
 } else if(ext=="xml") {
   if(!requireNamespace("xml2",quietly=TRUE))
     stop("Install xml2 to process structured ScienceDirect XML.")
   doc<-xml2::read_xml(path)
   nodes<-xml2::xml_find_all(doc,
      "//*[local-name()='service-error' or local-name()='error-response']")
   if(length(nodes))stop("ScienceDirect XML contains an error, not full text.")
   # Preserve headings, paragraphs, captions and section identifiers.
   parts<-xml2::xml_find_all(doc,
      "//*[local-name()='title' or local-name()='section-title' or local-name()='para' or local-name()='ce:para' or local-name()='p' or local-name()='caption']")
   if(!length(parts))stop("ScienceDirect XML has no identifiable article body.")
   val<-trimws(xml2::xml_text(parts))
   val<-val[nzchar(val)]
   if(!length(val)||sum(nchar(val))<500L)
     stop("ScienceDirect XML appears metadata-only or incomplete.")
   content<-paste(paste0("[SECTION ",seq_along(val),"] ",val),collapse="\n\n")
   anchors<-"ordered XML content sections; not verified page numbers"
   n<-length(val)
 } else {
   obj<-jsonlite::fromJSON(path,simplifyVector=FALSE)
   if(!is.null(obj$"service-error")||is.null(obj$"full-text-retrieval-response"))
      stop("ScienceDirect JSON contains no full-text retrieval payload.")
   flatten<-function(x,depth=0L) {
     if(depth>60L)return(character())
     if(is.character(x))return(x[nzchar(trimws(x))])
     if(!is.list(x))return(character())
     unlist(lapply(x,flatten,depth=depth+1L),use.names=FALSE)
   }
   elements<-flatten(obj$"full-text-retrieval-response")
   # Restrict model input to plausible narrative material rather than
   # identifiers, URLs, metadata fragments, or one-line abstracts.
   elements<-elements[nchar(trimws(elements))>=60L]
   if(!length(elements)||sum(nchar(elements))<500L)
     stop("ScienceDirect JSON has insufficient extractable full text.")
   content<-paste(paste0("[TEXT BLOCK ",seq_along(elements),"] ",
                         elements),collapse="\n\n")
   anchors<-"ordered JSON text blocks; not verified page numbers"
   n<-length(elements)
 }
 if(nchar(content,type="bytes")>600000L)
   stop("Full-text payload exceeds 600 kB; no silent truncation.")
 list(path=path,format=ext,text=content,units=n,
      anchors=anchors,sha256=digest::digest(file=path,algo="sha256",
                                           serialize=FALSE))
}

#' Analyse W09 PDFs and ScienceDirect XML/JSON
#'
#' Prioritises XML, then JSON, then PDF. For structured text with no stable
#' page mapping, the model must cite section/text-block anchors and MUST NOT
#' fabricate page numbers. Human source identity confirmation is required.
#' @param w09 W09 pilot directory.
#' @param record_ids IDs manually checked against the full-text documents.
#' @param model OpenAI model.
#' @param max_pages PDF pages to include (0 includes all). Ignored for XML/JSON.
#' @return Append-only full-text extraction JSONL.
#' @export
analyse_w09_cases <- function(w09,record_ids,model="gpt-5.6",max_pages=0L) {
 if(!nzchar(Sys.getenv("OPENAI_API_KEY")))stop("OPENAI_API_KEY is required.")
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
 if(missing(record_ids)||!length(record_ids)||anyDuplicated(record_ids)||
    any(!record_ids%in%q$record_id))stop("Supply unique identity-verified W09 record IDs.")
 if(length(max_pages)!=1L||is.na(max_pages)||max_pages!=as.integer(max_pages)||
    max_pages<0L)stop("max_pages must be a non-negative integer.")
 promptpath<-file.path(w09,"prompt.txt")
 if(!file.exists(promptpath))stop("W09 prompt is missing.")
 prompt<-paste(readLines(promptpath,warn=FALSE,encoding="UTF-8"),
               collapse="\n")
 out<-file.path(w09,"fulltext_extraction.jsonl")
 lines<-if(file.exists(out))readLines(out,warn=FALSE) else character()
 done<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
              simplifyVector=FALSE)
 ids<-vapply(done,function(x)x$record_id,character(1))
 if(anyDuplicated(ids)||any(!ids%in%q$record_id))
   stop("Existing W09 extraction ledger contains invalid IDs.")
 for(id in record_ids[!record_ids%in%ids]) {
   payload<-w09_text_payload(w09,id,as.integer(max_pages))
   # The v1 prompt requests page_number. Override it explicitly where source
   # content has no pagination, rather than hallucinating article page numbers.
   instruction<-paste(prompt,
     "\n\nSOURCE-ANCHOR OVERRIDE: Source format =",payload$format,
     ". Anchor convention:",payload$anchors,
     ". For XML and JSON, each supporting evidence item MUST use source_anchor",
     " (e.g. [SECTION 3] or [TEXT BLOCK 5]) and MUST set page_number to null.",
     " Do not invent page numbers. Include source_anchor in verification_evidence",
     " and every reported_claims item. For PDF, use [PAGE n] and real page_number.",
     " Short source_quote must be verbatim from supplied text.")
   input<-list(record_id=id,source_format=payload$format,
     source_sha256=payload$sha256,units_submitted=payload$units,
     fulltext=payload$text)
   body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
     input=list(list(role="system",content=list(list(type="input_text",
       text=instruction))),list(role="user",content=list(list(
       type="input_text",text=jsonlite::toJSON(input,auto_unbox=TRUE,
                                              null="null"))))),
     text=list(format=list(type="json_object")))
   resp<-httr2::request("https://api.openai.com/v1/responses") |>
     httr2::req_auth_bearer_token(Sys.getenv("OPENAI_API_KEY")) |>
     httr2::req_body_json(body,auto_unbox=TRUE) |>
     httr2::req_timeout(300) |> httr2::req_perform() |>
     httr2::resp_body_json(simplifyVector=FALSE)
   chunks<-unlist(lapply(resp$output,function(item)
     unlist(lapply(item$content,function(x)
       if(identical(x$type,"output_text"))x$text else NULL),
       use.names=FALSE)),use.names=FALSE)
   if(!length(chunks))stop("W09 response missing for ",id)
   result<-jsonlite::fromJSON(chunks[[1L]],simplifyVector=FALSE)
   if(!identical(result$record_id,id)||
      !result$verification_status%in%c("cases_confirmed",
       "no_collective_action_case","uncertain","fulltext_unreadable")||
      !is.list(result$cases))stop("W09 response schema failure for ",id)
   result$provenance<-list(source_path=payload$path,
      source_format=payload$format,source_sha256=payload$sha256,
      units_submitted=payload$units,source_anchors=payload$anchors,
      model=model,prompt_sha256=digest::digest(file=promptpath,
        algo="sha256",serialize=FALSE))
   cat(jsonlite::toJSON(result,auto_unbox=TRUE,null="null"),"\n",
       file=out,append=TRUE,sep="")
   message("[W09] Analysed ",id," (",payload$format,"): ",
           result$verification_status)
 }
 invisible(out)
}
