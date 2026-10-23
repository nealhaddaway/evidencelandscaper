#' W09 retrieval with structured text first: OpenAlex, then ScienceDirect
#'
#' Reattempts missing full texts despite earlier v2 negative results.
#' Preserves old logs and all existing files. OpenAlex only supplies PDF
#' locations and verified DOIs; ScienceDirect preference is XML, JSON, PDF.
#' @param w09 W09 prepared pilot folder.
#' @param max_records Maximum number of publications checked.
#' @return Path to append-only v3 retrieval log.
#' @export
retrieve_w09_structured <- function(w09,max_records=6L) {
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                        stringsAsFactors=FALSE)
 if(length(max_records)!=1L||is.na(max_records)||max_records<1L||
    max_records!=as.integer(max_records))stop("max_records must be positive integer.")
 log<-file.path(w09,"w09_retrieval_v3.jsonl")
 readjl<-function(path) {
   if(!file.exists(path))return(list())
   z<-readLines(path,warn=FALSE)
   lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 history<-readjl(log)
 done<-vapply(history,function(z)z$record_id,character(1))
 if(anyDuplicated(done))stop("Duplicate record IDs in W09 v3 log.")
 scalar<-function(x)if(is.null(x)||!length(x)||all(is.na(x)))"" else
   trimws(as.character(x[[1L]]))
 doi_clean<-function(x) {
   x<-sub("^https?://(dx\\.)?doi\\.org/","",scalar(x),ignore.case=TRUE)
   x<-sub("^doi:\\s*","",x,ignore.case=TRUE)
   if(grepl("^10\\.[0-9]{4,9}/[^[:space:];]+$",x))x else ""
 }
 nt<-function(x) {
   y<-iconv(scalar(x),to="ASCII//TRANSLIT",sub="")
   trimws(gsub("[^a-z0-9]+"," ",tolower(y)))
 }
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 request_oa<-function(url) {
   r<-httr2::request(url)|>httr2::req_timeout(35)|>httr2::req_perform()
   httr2::resp_body_json(r,simplifyVector=FALSE)$results
 }
 get_pdf<-function(url,target) {
   if(!is.character(url)||length(url)!=1L||!grepl("^https://",url))
     return(FALSE)
   tmp<-tempfile(fileext=".pdf")
   on.exit(unlink(tmp),add=TRUE)
   ok<-tryCatch({
     httr2::request(url)|>httr2::req_timeout(90)|>
       httr2::req_perform(path=tmp)
     TRUE
   },error=function(e)FALSE)
   if(!ok||!file.exists(tmp)||file.info(tmp)$size<100L)return(FALSE)
   con<-file(tmp,"rb");magic<-readBin(con,"raw",n=5);close(con)
   if(!identical(magic,charToRaw("%PDF-"))||file.exists(target))return(FALSE)
   if(!file.copy(tmp,target,overwrite=FALSE))stop("Cannot preserve OA PDF.")
   TRUE
 }
 pdf_dir<-file.path(w09,"pdfs")
 struct_dir<-file.path(w09,"structured")
 dir.create(struct_dir,recursive=TRUE,showWarnings=FALSE)
 pending<-queue[!queue$record_id%in%done &
   !file.exists(file.path(pdf_dir,paste0(queue$record_id,".pdf"))) &
   !file.exists(file.path(struct_dir,paste0(queue$record_id,".xml"))) &
   !file.exists(file.path(struct_dir,paste0(queue$record_id,".json"))),,drop=FALSE]
 pending<-head(pending,as.integer(max_records))
 for(i in seq_len(nrow(pending))) {
   id<-pending$record_id[[i]]
   doi<-doi_clean(pending$doi[[i]])
   title<-scalar(pending$title[[i]])
   pdf<-file.path(pdf_dir,paste0(id,".pdf"))
   entry<-list(record_id=id,queued_doi=doi,openalex_doi="",
     doi_used="",identifier_provenance="",openalex_match="none",
     openalex_status="not_found",sciencedirect_status="not_attempted",
     sciencedirect_attempts=list(),retrieval_status="not_found",
     saved_file=NULL,saved_sha256=NULL,notes="")
   tryCatch({
     works<-list()
     if(nzchar(doi)) {
       works<-request_oa(paste0("https://api.openalex.org/works?filter=doi:",
            utils::URLencode(doi,reserved=TRUE),"&per_page=3"))
     }
     if(!length(works)&&nzchar(title)) {
       works<-request_oa(paste0("https://api.openalex.org/works?search=",
             utils::URLencode(title,reserved=TRUE),"&per_page=10"))
     }
     matched<-Filter(function(w){
       wd<-doi_clean(w$doi)
       same_title<-nzchar(nt(title))&&identical(nt(title),nt(w$display_name))
       same_doi<-nzchar(doi)&&identical(tolower(doi),tolower(wd))
       !(nzchar(doi)&&nzchar(wd)&&!same_doi)&&
          (same_title||same_doi)
     },works)
     if(length(matched)==1L) {
       w<-matched[[1L]]
       entry$openalex_match<-"unique_verified"
       entry$openalex_doi<-doi_clean(w$doi)
       # A uniquely matched DOI can fill missing queue DOI, but conflicting
       # or ambiguous records must never be used for ScienceDirect.
       if(nzchar(doi)) {
         entry$doi_used<-doi;entry$identifier_provenance<-"W02_queue"
       } else if(nzchar(entry$openalex_doi)) {
         entry$doi_used<-entry$openalex_doi
         entry$identifier_provenance<-"OpenAlex_exact_title"
       }
       entry$openalex_status<-"matched_no_pdf"
       locations<-c(list(w$best_oa_location),w$locations)
       urls<-unique(unlist(lapply(locations,function(loc)
         if(is.null(loc))NULL else loc$pdf_url),use.names=FALSE))
       urls<-urls[!is.na(urls)&grepl("^https://",urls)]
       for(url in urls)if(get_pdf(url,pdf)) {
         entry$openalex_status<-"downloaded_unverified"
         entry$retrieval_status<-"downloaded_unverified"
         entry$saved_file<-pdf
         entry$saved_sha256<-sha(pdf)
         break
       }
     } else entry$openalex_match<-if(length(matched))"ambiguous" else "no_verified_match"
   },error=function(e){
     entry$openalex_status<-"lookup_error"
     entry$notes<-paste("OpenAlex:",conditionMessage(e))
   })
   if(!file.exists(pdf)) {
     key<-Sys.getenv("SCOPUS_API_TOKEN")
     inst<-Sys.getenv("SCOPUS_INSTTOKEN")
     if(!nzchar(entry$doi_used)) {
       entry$sciencedirect_status<-"no_verified_doi"
     } else if(!nzchar(key)) {
       entry$sciencedirect_status<-"no_api_key"
     } else {
       base<-paste0("https://api.elsevier.com/content/article/doi/",
                  utils::URLencode(entry$doi_used,reserved=TRUE))
       for(fmt in c("text/xml","application/json","application/pdf")) {
         ext<-switch(fmt,"text/xml"="xml","application/json"="json",
                     "application/pdf"="pdf")
         target<-if(ext=="pdf")pdf else
           file.path(struct_dir,paste0(id,".",ext))
         tmp<-tempfile(fileext=paste0(".",ext))
         req<-httr2::request(base)|>
           httr2::req_url_query(view="FULL",httpAccept=fmt)|>
           httr2::req_headers("X-ELS-APIKey"=key,Accept=fmt)|>
           httr2::req_timeout(90)
         if(nzchar(inst))req<-httr2::req_headers(req,"X-ELS-Insttoken"=inst)
         response<-tryCatch(httr2::req_perform(req,path=tmp),
                             error=function(e)e)
         event<-list(format=fmt,http_status=NULL,content_type=NULL,
                     result="failed",detail="")
         if(inherits(response,"error")) {
           if(inherits(response,"httr2_http")) {
             event$http_status<-tryCatch(httr2::resp_status(response$response),
                                         error=function(e)NULL)
           }
           event$detail<-conditionMessage(response)
         } else {
           event$http_status<-httr2::resp_status(response)
           event$content_type<-httr2::resp_header(response,"content-type")
           valid<-FALSE
           if(file.exists(tmp)&&file.info(tmp)$size>20L) {
             if(ext=="pdf") {
               con<-file(tmp,"rb");magic<-readBin(con,"raw",n=5);close(con)
               valid<-identical(magic,charToRaw("%PDF-"))
             } else {
               raw<-paste(readLines(tmp,warn=FALSE,encoding="UTF-8"),
                          collapse="\n")
               if(ext=="xml") {
                 valid<-grepl("<(?:\\?xml|full-text-retrieval-response|xocs:doc|article)",raw,
                             perl=TRUE)&&
                   !grepl("error-response|service-error",substr(raw,1,2000),
                          ignore.case=TRUE) &&
                   nchar(raw)>1000
               } else {
                 parsed<-tryCatch(jsonlite::fromJSON(raw,simplifyVector=FALSE),
                                  error=function(e)NULL)
                 valid<-is.list(parsed)&&
                   !is.null(parsed$"full-text-retrieval-response") &&
                   is.null(parsed$"service-error") &&
                   nchar(raw)>1000
               }
             }
           }
           if(valid&&!file.exists(target)) {
             if(!file.copy(tmp,target,overwrite=FALSE))
               stop("Unable to save ScienceDirect response.")
             event$result<-"saved_unverified"
             entry$retrieval_status<-"downloaded_unverified"
             entry$saved_file<-target
             entry$saved_sha256<-sha(target)
           } else if(valid) {
             event$result<-"already_present"
           } else {
             event$result<-"invalid_or_metadata_only"
             event$detail<-"Response did not pass basic format checks; no extraction eligibility inferred."
           }
         }
         unlink(tmp)
         entry$sciencedirect_attempts[[length(entry$sciencedirect_attempts)+1L]]<-event
         if(event$result%in%c("saved_unverified","already_present")) {
           entry$sciencedirect_status<-paste0("saved_",ext,"_unverified")
           break
         }
       }
       if(identical(entry$sciencedirect_status,"not_attempted"))
         entry$sciencedirect_status<-"no_valid_fulltext_response"
     }
   } else entry$sciencedirect_status<-"skipped_openalex_pdf"
   cat(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null"),"\n",
       file=log,append=TRUE,sep="")
   message("[W09] ",id,": ",entry$openalex_status,"; ",
           entry$sciencedirect_status)
 }
 invisible(log)
}
