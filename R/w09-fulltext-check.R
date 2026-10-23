#' Check OpenAlex full-text availability before W09 retrieval
#'
#' Metadata-only check. Does not download documents, alter retrieval queues or
#' infer that OA status guarantees retrievable full text. Resumes completed rows.
#' The caller should supply a case-only W09 queue.
#' @param w09 W09 run directory.
#' @return Path to an append-only JSONL availability assessment.
#' @export
check_w09_fulltext <- function(w09) {
  w09<-normalizePath(w09,mustWork=TRUE)
  queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                        stringsAsFactors=FALSE)
  out<-file.path(w09,"openalex_fulltext_check.jsonl")
  lines<-if(file.exists(out))readLines(out,warn=FALSE) else character()
  old<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
              simplifyVector=FALSE)
  done<-vapply(old,function(x)x$record_id,character(1))
  if(anyDuplicated(done))stop("Duplicate check entries; inspect before resuming.")
  clean<-function(x) {
    x<-trimws(if(length(x)&&!is.na(x[[1L]]))as.character(x[[1L]]) else "")
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x,ignore.case=TRUE)
    sub("^doi:","",x,ignore.case=TRUE)
  }
  norm<-function(x)trimws(gsub("[^a-z0-9]+"," ",tolower(x)))
  pending<-queue[!queue$record_id%in%done,,drop=FALSE]
  for(i in seq_len(nrow(pending))) {
    id<-pending$record_id[[i]]
    doi<-clean(pending$doi[[i]])
    title<-as.character(pending$title[[i]])
    result<-list(record_id=id,status="not_found",is_oa=NULL,
      oa_status=NULL,has_fulltext=NULL,has_pdf_url=FALSE,
      has_landing_page=FALSE,location_count=0L,
      pdf_url_count=0L,landing_page_count=0L,
      sources=list(),error=NULL)
    tryCatch({
      url<-if(nzchar(doi))
        paste0("https://api.openalex.org/works/https://doi.org/",
          utils::URLencode(doi,reserved=TRUE)) else
        paste0("https://api.openalex.org/works?search=",
          utils::URLencode(title,reserved=TRUE),"&per_page=10")
      response<-httr2::request(url)|>httr2::req_timeout(35)|>
        httr2::req_perform()
      body<-httr2::resp_body_json(response,simplifyVector=FALSE)
      works<-if(is.null(body$results))list(body) else body$results
      matches<-Filter(function(z){
        wd<-clean(z$doi)
        (nzchar(doi)&&identical(tolower(doi),tolower(wd)))||
          (nzchar(title)&&identical(norm(title),norm(z$display_name)))
      },works)
      if(length(matches)==1L) {
        w<-matches[[1L]]
        locs<-c(list(w$best_oa_location),w$locations)
        locs<-Filter(Negate(is.null),locs)
        links<-function(k)unique(unlist(lapply(locs,function(z)z[[k]]),
                                 use.names=FALSE))
        pdf<-links("pdf_url");pdf<-pdf[!is.na(pdf)&nzchar(pdf)]
        landing<-links("landing_page_url")
        landing<-landing[!is.na(landing)&nzchar(landing)]
        result$status<-"matched"
        result$is_oa<-w$open_access$is_oa
        result$oa_status<-w$open_access$oa_status
        result$has_fulltext<-w$has_fulltext
        result$location_count<-length(locs)
        result$pdf_url_count<-length(pdf)
        result$landing_page_count<-length(landing)
        result$has_pdf_url<-length(pdf)>0
        result$has_landing_page<-length(landing)>0
        result$sources<-lapply(locs,function(z)list(
          source=if(is.null(z$source$display_name))NULL else z$source$display_name,
          is_oa=z$is_oa,version=z$version,
          pdf_url=z$pdf_url,landing_page_url=z$landing_page_url))
      } else result$status<-if(length(matches)>1)"ambiguous" else "no_verified_match"
    },error=function(e){
      result$status<-"lookup_error"
      result$error<-conditionMessage(e)
    })
    cat(jsonlite::toJSON(result,auto_unbox=TRUE,null="null"),
        "\n",file=out,append=TRUE,sep="")
    message("[W09 availability] ",i,"/",nrow(pending)," ",id,": ",result$status)
  }
  invisible(out)
}
