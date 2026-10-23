#' Retrieve W09 full texts using the completed OpenAlex availability check
#'
#' Reuses eligible locally archived documents, then attempts the documented
#' source locations. Preserves original downloads and logs. Unverified
#' identities are never silently accepted.
#' @param w09 W09 47-candidate run directory.
#' @return Path to the append-only retrieval log.
#' @export
retrieve_w09_from_availability <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                         stringsAsFactors=FALSE)
 check<-file.path(w09,"openalex_fulltext_check.jsonl")
 if(!file.exists(check))stop("Run check_w09_fulltext() first.")
 readjl<-function(p) {
   if(!file.exists(p))return(list())
   z<-readLines(p,warn=FALSE)
   lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 records<-readjl(check)
 matches<-setNames(records,vapply(records,function(z)z$record_id,character(1)))
 if(anyDuplicated(names(matches)))stop("Duplicate availability records.")
 if(!all(queue$record_id%in%names(matches)))stop("Availability check incomplete.")
 log<-file.path(w09,"w09_retrieval_availability.jsonl")
 history<-readjl(log)
 done<-vapply(history,function(z)z$record_id,character(1))
 if(anyDuplicated(done))stop("Duplicate retrieval log entries.")
 dir.create(file.path(w09,"structured"),recursive=TRUE,showWarnings=FALSE)
 dir.create(file.path(w09,"pdfs"),recursive=TRUE,showWarnings=FALSE)
 norm<-function(x)trimws(gsub("[^a-z0-9]+"," ",tolower(as.character(x))))
 # Files from earlier runs may only be reused when their queue metadata agrees.
 olddirs<-list.dirs(dirname(w09),recursive=FALSE,full.names=TRUE)
 olddirs<-setdiff(olddirs,w09)
 other<-lapply(olddirs,function(d) {
   p<-file.path(d,"retrieval_queue.csv")
   if(!file.exists(p))return(NULL)
   tryCatch(utils::read.csv(p,stringsAsFactors=FALSE),error=function(e)NULL)
 })
 filetype<-function(f) {
   ext<-tolower(tools::file_ext(f))
   if(!file.exists(f)||is.na(file.info(f)$size)||file.info(f)$size<100) return(FALSE)
   if(ext=="pdf") {
     con<-file(f,"rb");on.exit(close(con))
     return(identical(readBin(con,"raw",n=5),charToRaw("%PDF-")))
   }
   if(ext=="xml")return(!inherits(tryCatch(xml2::read_xml(f),error=function(e)e),"error"))
   if(ext=="json")return(!inherits(tryCatch(jsonlite::fromJSON(f),error=function(e)e),"error"))
   FALSE
 }
 pending<-queue[!queue$record_id%in%done,,drop=FALSE]
 for(i in seq_len(nrow(pending))) {
   id<-pending$record_id[[i]]
   row<-pending[i,,drop=FALSE]
   entry<-list(record_id=id,availability_status=matches[[id]]$status,
     source="none",format="none",status="not_retrieved",file=NULL,
     sha256=NULL,attempts=list())
   dest<-c(file.path(w09,"structured",paste0(id,c(".xml",".json"))),
           file.path(w09,"pdfs",paste0(id,".pdf")))
   for(f in dest)if(filetype(f)) {
     entry$status<-"existing_unverified";entry$file<-f
     entry$format<-tolower(tools::file_ext(f));entry$source<-"current_run"
     entry$sha256<-digest::digest(file=f,algo="sha256",serialize=FALSE)
     break
   }
   # Import an unchanged matching item only when source ID and bibliography agree.
   if(is.null(entry$file))for(j in seq_along(olddirs)) {
     tab<-other[[j]]
     if(is.null(tab))next
     ix<-which(tab$record_id==id)
     if(length(ix)!=1L)next
     if(!identical(norm(tab$title[ix]),norm(row$title)))next
     if(!identical(norm(tab$doi[ix]),norm(row$doi)))next
     src<-c(file.path(olddirs[j],"structured",paste0(id,c(".xml",".json"))),
            file.path(olddirs[j],"pdfs",paste0(id,".pdf")))
     for(k in seq_along(src))if(filetype(src[k])&&!file.exists(dest[k])) {
       if(!file.copy(src[k],dest[k],overwrite=FALSE))stop("Failed copying archive.")
       entry$status<-"copied_unverified";entry$file<-dest[k]
       entry$format<-tolower(tools::file_ext(dest[k]))
       entry$source<-basename(olddirs[j])
       entry$sha256<-digest::digest(file=dest[k],algo="sha256",serialize=FALSE)
       break
     }
     if(!is.null(entry$file))break
   }
   # Attempt the exact locations returned by the earlier verified metadata
   # lookup, not a fresh search. Only a correctly signed document is saved.
   data<-matches[[id]]
   if(is.null(entry$file)&&identical(data$status,"matched")) {
     sources<-data$sources
     if(is.null(sources))sources<-list()
     urls<-unique(unlist(lapply(sources,function(z)z$pdf_url),
                         use.names=FALSE))
     urls<-urls[!is.na(urls)&grepl("^https://",urls)]
     for(url in urls) {
       tmp<-tempfile(fileext=".pdf")
       request<-tryCatch(httr2::request(url)|>httr2::req_timeout(60)|>
                 httr2::req_perform(path=tmp),error=function(e)e)
       ok<-filetype(tmp)
       entry$attempts[[length(entry$attempts)+1L]]<-list(
          source="OpenAlex_location",url=url,
          status=if(ok)"downloaded" else if(inherits(request,"error"))
            conditionMessage(request) else "invalid_document")
       if(ok&&!file.exists(dest[3])) {
         if(!file.copy(tmp,dest[3],overwrite=FALSE))stop("Failed saving document.")
         entry$status<-"downloaded_unverified";entry$file<-dest[3]
         entry$format<-"pdf";entry$source<-"OpenAlex_location"
         entry$sha256<-digest::digest(file=dest[3],algo="sha256",serialize=FALSE)
       }
       unlink(tmp)
       if(!is.null(entry$file))break
     }
   }
   cat(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null"),"\n",
       file=log,append=TRUE,sep="")
   message("[W09 retrieval] ",i,"/",nrow(pending)," ",id,": ",entry$status)
 }
 invisible(log)
}

#' Audit saved W09 documents without modifying them
#' @param w09 W09 run directory.
#' @return CSV report path.
#' @export
report_w09_fulltext <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
 readjl<-function(p) {
  if(!file.exists(p))return(list())
  z<-readLines(p,warn=FALSE)
  lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 logs<-c("w09_retrieval_availability.jsonl","w09_retrieval_v3.jsonl",
         "w09_retrieval_v3_key_retry.jsonl")
 history<-unlist(lapply(file.path(w09,logs),readjl),recursive=FALSE)
 statuses<-split(history,vapply(history,function(z)z$record_id,character(1)))
 rows<-lapply(q$record_id,function(id) {
   options<-c(file.path(w09,"structured",paste0(id,c(".xml",".json"))),
              file.path(w09,"pdfs",paste0(id,".pdf")))
   present<-options[file.exists(options)]
   parsed<-vapply(present,function(f) {
     ext<-tools::file_ext(f)
     if(ext=="xml")return(!inherits(tryCatch(xml2::read_xml(f),error=function(e)e),"error"))
     if(ext=="json")return(!inherits(tryCatch(jsonlite::fromJSON(f),error=function(e)e),"error"))
     con<-file(f,"rb");on.exit(close(con),add=TRUE)
     identical(readBin(con,"raw",n=5),charToRaw("%PDF-"))
   },logical(1))
   good<-present[parsed]
   data.frame(record_id=id,preferred_format=if(length(good))
    toupper(tools::file_ext(good[[1L]])) else "none",
    file=if(length(good))good[[1L]] else "",
    has_parseable_file=length(good)>0,
    substantive_fulltext_verified=FALSE,
    identity_verified=FALSE,
    status=if(length(good))"requires_content_identity_review" else
      "no_file_retrieved",stringsAsFactors=FALSE)
 })
 report<-do.call(rbind,rows)
 path<-file.path(w09,"w09_fulltext_audit.csv")
 if(file.exists(path)) {
   prior<-file.path(w09,paste0("w09_fulltext_audit-",
       substr(digest::digest(file=path,algo="sha256",serialize=FALSE),1,12),
       ".csv"))
   if(!file.exists(prior)&&!file.copy(path,prior))stop("Unable to archive previous audit")
 }
 utils::write.csv(report,path,row.names=FALSE)
 message("[W09 audit] ",sum(report$has_parseable_file),"/",nrow(report),
         " files pass format parsing; substantive content and identity unverified.")
 invisible(path)
}
