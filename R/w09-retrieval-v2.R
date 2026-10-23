#' Continue W09 retrieval: OpenAlex DOI/title, then ScienceDirect
#'
#' Non-destructive: existing PDFs and the original oa_retrieval.jsonl are
#' preserved. Logs attempts in w09_retrieval_v2.jsonl. Retrieved PDFs remain
#' unverified until manually matched against the publication.
#' @param w09 Prepared W09 directory.
#' @param max_records Maximum number of remaining records to attempt.
#' @return W09 v2 retrieval-log path.
#' @export
retrieve_w09_fulltexts <- function(w09,max_records=6L) {
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<1L)
    stop("max_records must be a positive integer.")
  log<-file.path(w09,"w09_retrieval_v2.jsonl")
  readlog<-function() {
    if(!file.exists(log))return(list())
    l<-readLines(log,warn=FALSE)
    lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  history<-readlog()
  done<-vapply(history,function(z)z$record_id,character(1))
  if(anyDuplicated(done))stop("Duplicate completed W09 v2 retrieval attempts.")
  scalar<-function(x)if(is.null(x)||!length(x)||all(is.na(x)))""
    else trimws(as.character(x[[1L]]))
  doi_clean<-function(x) {
    x<-sub("^https?://(dx\\.)?doi\\.org/","",scalar(x),ignore.case=TRUE)
    x<-sub("^doi:\\s*","",x,ignore.case=TRUE)
    if(!grepl("^10\\.[0-9]{4,9}/[^[:space:];]+$",x))"" else x
  }
  norm_title<-function(x) {
    x<-tolower(iconv(scalar(x),to="ASCII//TRANSLIT",sub=""))
    trimws(gsub("[^a-z0-9]+"," ",x))
  }
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  get_work<-function(req) {
    r<-httr2::req_perform(httr2::req_timeout(req,35))
    httr2::resp_body_json(r,simplifyVector=FALSE)$results
  }
  download_pdf<-function(url,target,headers=NULL) {
    if(!grepl("^https://",url))return(FALSE)
    tmp<-tempfile(fileext=".pdf")
    on.exit(unlink(tmp),add=TRUE)
    req<-httr2::request(url)|>httr2::req_timeout(90)
    if(!is.null(headers))req<-do.call(httr2::req_headers,c(list(req),headers))
    ok<-tryCatch({httr2::req_perform(req,path=tmp);TRUE},
                 error=function(e)FALSE)
    if(!ok||!file.exists(tmp)||file.info(tmp)$size<100L)return(FALSE)
    con<-file(tmp,"rb");header<-readBin(con,"raw",n=5);close(con)
    if(!identical(header,charToRaw("%PDF-"))||file.exists(target))return(FALSE)
    if(!file.copy(tmp,target,overwrite=FALSE))stop("Could not copy PDF.")
    TRUE
  }
  pending<-q[!q$record_id%in%done &
     !file.exists(file.path(w09,"pdfs",paste0(q$record_id,".pdf"))),,drop=FALSE]
  pending<-head(pending,as.integer(max_records))
  for(i in seq_len(nrow(pending))) {
    z<-pending[i,,drop=FALSE];id<-z$record_id[[1L]]
    doi<-doi_clean(z$doi);title<-scalar(z$title)
    target<-file.path(w09,"pdfs",paste0(id,".pdf"))
    entry<-list(record_id=id,doi=doi,title=title,
      openalex_status="not_found",sciencedirect_status="not_attempted",
      status="not_found",source_url=NULL,pdf_sha256=NULL,note="")
    tryCatch({
      works<-list()
      if(nzchar(doi)) {
        works<-get_work(httr2::request("https://api.openalex.org/works") |>
             httr2::req_url_query(filter=paste0("doi:",doi),per_page=3))
      }
      if(!length(works) && nzchar(title)) {
        works<-get_work(httr2::request("https://api.openalex.org/works") |>
             httr2::req_url_query(search=title,per_page=8))
      }
      valid<-Filter(function(w) {
        wt<-norm_title(w$display_name)
        querytitle<-norm_title(title)
        same_title<-nzchar(querytitle)&&identical(wt,querytitle)
        wdoi<-doi_clean(w$doi)
        same_doi<-nzchar(doi)&&nzchar(wdoi)&&identical(tolower(wdoi),tolower(doi))
        # Never accept a DOI contradiction or an unrelated title.
        !(nzchar(doi)&&nzchar(wdoi)&&!same_doi) &&
          (same_doi||same_title)
      },works)
      if(length(valid)!=1L) {
        entry$openalex_status<-if(length(valid)>1L)"ambiguous_match" else "no_verified_match"
      } else {
        w<-valid[[1L]]
        entry$openalex_status<-"matched_no_pdf"
        locations<-c(list(w$best_oa_location),w$locations)
        urls<-unique(unlist(lapply(locations,function(loc)
          if(is.null(loc))NULL else loc$pdf_url),use.names=FALSE))
        urls<-urls[!is.na(urls)&grepl("^https://",urls)]
        for(url in urls)if(download_pdf(url,target)) {
          entry$openalex_status<-"downloaded_unverified"
          entry$status<-"downloaded_unverified"
          entry$source_url<-url
          entry$pdf_sha256<-sha(target)
          break
        }
      }
    },error=function(e){
      entry$openalex_status<-"retrieval_error"
      entry$note<-paste("OpenAlex:",conditionMessage(e))
    })
    if(!file.exists(target)) {
      key<-Sys.getenv("SCOPUS_API_TOKEN")
      inst<-Sys.getenv("SCOPUS_INSTTOKEN")
      if(!nzchar(doi)) {
        entry$sciencedirect_status<-"no_doi"
      } else if(!nzchar(key)) {
        entry$sciencedirect_status<-"no_api_key"
      } else {
        tryCatch({
          # Elsevier full-text retrieval requires separate institutional/API rights.
          base<-paste0("https://api.elsevier.com/content/article/doi/",
                       utils::URLencode(doi,reserved=TRUE))
          tmp<-tempfile(fileext=".pdf")
          on.exit(unlink(tmp),add=TRUE)
          req<-httr2::request(base)|>
            httr2::req_url_query(view="FULL",httpAccept="application/pdf")|>
            httr2::req_headers("X-ELS-APIKey"=key,Accept="application/pdf")|>
            httr2::req_timeout(90)
          if(nzchar(inst))req<-httr2::req_headers(req,"X-ELS-Insttoken"=inst)
          httr2::req_perform(req,path=tmp)
          con<-file(tmp,"rb");header<-readBin(con,"raw",n=5);close(con)
          if(identical(header,charToRaw("%PDF-"))&&file.info(tmp)$size>100L) {
            if(file.exists(target))stop("PDF appeared during download; not overwriting.")
            if(!file.copy(tmp,target,overwrite=FALSE))stop("PDF write failed.")
            entry$sciencedirect_status<-"downloaded_unverified"
            entry$status<-"downloaded_unverified"
            entry$source_url<-base
            entry$pdf_sha256<-sha(target)
          } else entry$sciencedirect_status<-"no_pdf_or_not_entitled"
          unlink(tmp)
        },error=function(e){
          entry$sciencedirect_status<-"request_failed_or_not_entitled"
          entry$note<-paste(entry$note,conditionMessage(e))
        })
      }
    }
    cat(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null"),"\n",
        file=log,append=TRUE,sep="")
    message("[W09] ",id,": ",entry$openalex_status,
            " -> ScienceDirect ",entry$sciencedirect_status)
  }
  invisible(log)
}
