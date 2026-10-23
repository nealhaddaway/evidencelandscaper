#' Retrieve open-access W09 pilot PDFs using OpenAlex
#'
#' Only explicitly invoked requests are made. OA links are discovery hints,
#' not proof that the content matches the intended article. Every returned PDF
#' must be checked before model analysis. Existing PDFs are never overwritten.
#' @param w09 W09 pilot directory.
#' @param max_records Maximum number of missing PDFs to attempt.
#' @return Path to append-only retrieval log.
#' @export
retrieve_w09_oa <- function(w09,max_records=6L) {
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
 if(is.na(max_records)||length(max_records)!=1L||max_records<1L)
   stop("max_records must be positive.")
 max_records<-as.integer(max_records)
 logfile<-file.path(w09,"oa_retrieval.jsonl")
 existing<-if(file.exists(logfile))readLines(logfile,warn=FALSE) else character()
 done<-lapply(existing[nzchar(trimws(existing))],jsonlite::fromJSON,simplifyVector=FALSE)
 attempted<-vapply(done,function(z)z$record_id,character(1))
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 normalise_doi<-function(x) {
   x<-trimws(x)
   x<-sub("^https?://(dx\\.)?doi\\.org/","",x,ignore.case=TRUE)
   sub("^doi:\\s*","",x,ignore.case=TRUE)
 }
 todo<-q[!q$record_id%in%attempted &
         !file.exists(file.path(w09,"pdfs",paste0(q$record_id,".pdf"))),,drop=FALSE]
 todo<-head(todo,max_records)
 for(i in seq_len(nrow(todo))) {
   id<-todo$record_id[[i]];doi<-normalise_doi(todo$doi[[i]])
   result<-list(record_id=id,doi=doi,status="not_found",
                source_url=NULL,pdf_sha256=NULL,note="")
   target<-file.path(w09,"pdfs",paste0(id,".pdf"))
   if(!nzchar(doi)||grepl(";",doi,fixed=TRUE)) {
     result$status<-"manual_lookup_required"
     result$note<-"Missing or ambiguous DOI."
   } else {
     tryCatch({
       req<-httr2::request("https://api.openalex.org/works") |>
         httr2::req_url_query(filter=paste0("doi:",doi),per_page=1) |>
         httr2::req_timeout(30)
       response<-httr2::req_perform(req)
       works<-httr2::resp_body_json(response,simplifyVector=FALSE)$results
       if(length(works)) {
         locations<-c(list(works[[1]]$best_oa_location),
                      works[[1]]$locations)
         urls<-unique(Filter(function(x)is.character(x)&&length(x)==1L&&
           grepl("^https://",x),
           lapply(locations,function(z)z$pdf_url)))
         if(length(urls)) {
           for(url in urls) {
             tmp<-tempfile(fileext=".pdf")
             ok<-tryCatch({
               httr2::request(url) |>
                 httr2::req_timeout(90) |>
                 httr2::req_perform(path=tmp)
               TRUE
             },error=function(e)FALSE)
             if(ok && file.exists(tmp) && file.info(tmp)$size>100L) {
               header<-readBin(tmp,what="raw",n=5)
               if(identical(header,charToRaw("%PDF-")) && !file.exists(target)) {
                 if(!file.copy(tmp,target,overwrite=FALSE))
                   stop("Cannot save W09 PDF.")
                 result$status<-"downloaded_unverified"
                 result$source_url<-url
                 result$pdf_sha256<-sha(target)
                 unlink(tmp)
                 break
               }
             }
             unlink(tmp)
           }
         }
       }
     },error=function(e) {
       result$status<-"retrieval_error"
       result$note<-conditionMessage(e)
     })
   }
   cat(jsonlite::toJSON(result,auto_unbox=TRUE,null="null"),"\n",
       file=logfile,append=TRUE,sep="")
   message("[W09] ",id,": ",result$status)
 }
 invisible(logfile)
}

#' Run exploratory full-text case extraction on locally available W09 PDFs
#'
#' This stage is independent of W07, saves each response incrementally, and
#' requires a human to confirm that the PDF matches its publication before use.
#' @param w09 W09 pilot folder.
#' @param record_ids IDs whose PDF correspondence has been checked manually.
#' @param model OpenAI model.
#' @param max_pages Maximum pages sent per publication (0 means full text).
#' @return Local JSONL results path.
#' @export
analyse_w09_cases <- function(w09,record_ids,model="gpt-5.6",max_pages=0L) {
 if(!requireNamespace("pdftools",quietly=TRUE))
   stop("Install pdftools first: install.packages('pdftools')")
 if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
   stop("OPENAI_API_KEY is required.")
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
 if(missing(record_ids)||!length(record_ids)||anyDuplicated(record_ids)||
    any(!record_ids%in%q$record_id))
   stop("Explicitly provide unique verified record_ids from this W09 queue.")
 if(length(max_pages)!=1L||is.na(max_pages)||max_pages<0L)
   stop("max_pages must be a non-negative integer.")
 promptpath<-file.path(w09,"prompt.txt")
 if(!file.exists(promptpath))stop("W09 prompt not archived.")
 prompt<-paste(readLines(promptpath,warn=FALSE,encoding="UTF-8"),collapse="\n")
 out<-file.path(w09,"fulltext_extraction.jsonl")
 saved<-if(file.exists(out))readLines(out,warn=FALSE) else character()
 saved<-lapply(saved[nzchar(trimws(saved))],jsonlite::fromJSON,simplifyVector=FALSE)
 ids_done<-vapply(saved,function(z)z$record_id,character(1))
 if(anyDuplicated(ids_done)||any(!ids_done %in% q$record_id))
   stop("Existing W09 extraction IDs conflict.")
 for(id in record_ids[!record_ids%in%ids_done]) {
   pdf<-file.path(w09,"pdfs",paste0(id,".pdf"))
   if(!file.exists(pdf))stop("No verified PDF supplied for ",id)
   pages<-pdftools::pdf_text(pdf)
   if(max_pages>0L)pages<-head(pages,as.integer(max_pages))
   if(!length(pages)||!any(nzchar(trimws(pages))))
     stop("PDF has no extractable text: ",id)
   text<-paste(vapply(seq_along(pages),function(i)
     paste0("[PAGE ",i,"]\n",pages[[i]]),character(1)),collapse="\n\n")
   # Large documents must not be silently truncated by this function.
   if(nchar(text,type="bytes")>600000L)
     stop("PDF exceeds 600 kB extracted text. Select a smaller, explicitly scoped document.")
   input<-list(record_id=id,pdf_sha256=digest::digest(file=pdf,algo="sha256",
                  serialize=FALSE),page_count=length(pages),fulltext=text)
   body<-list(model=model,store=FALSE,reasoning=list(effort="low"),
      input=list(
       list(role="system",content=list(list(type="input_text",text=prompt))),
       list(role="user",content=list(list(type="input_text",
             text=jsonlite::toJSON(input,auto_unbox=TRUE,null="null"))))),
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
   if(!length(chunks))stop("No W09 model response for ",id)
   result<-jsonlite::fromJSON(chunks[[1L]],simplifyVector=FALSE)
   if(!identical(result$record_id,id)||
      !result$verification_status%in%c("cases_confirmed",
         "no_collective_action_case","uncertain","fulltext_unreadable") ||
      !is.list(result$cases))
      stop("Invalid W09 full-text result for ",id)
   result$provenance<-list(pdf_sha256=input$pdf_sha256,
      pages_submitted=length(pages),model=model,
      prompt_sha256=digest::digest(file=promptpath,algo="sha256",serialize=FALSE))
   cat(jsonlite::toJSON(result,auto_unbox=TRUE,null="null"),"\n",
       file=out,append=TRUE,sep="")
   message("[W09] Extracted ",id,": ",result$verification_status)
 }
 invisible(out)
}
