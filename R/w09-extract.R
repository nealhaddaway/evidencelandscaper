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

