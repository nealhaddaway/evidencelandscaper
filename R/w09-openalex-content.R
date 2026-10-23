#' W09 full-text retrieval: OpenAlex-hosted content, then ScienceDirect
#'
#' DOI lookup and strict DOI/title verification. OpenAlex content API only,
#' never OpenAlex external location URLs. Prefer GROBID TEI XML. ScienceDirect
#' is tried only when no full text was acquired from OpenAlex. Every record
#' has append-only status logging and the final report is CSV.
#' @param w09 Prepared W09 directory, normally the case-only 47-record queue.
#' @return Path to the final CSV report.
#' @export
retrieve_w09_openalex_content <- function(w09) {
  w09 <- normalizePath(w09, mustWork=TRUE)
  q <- utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                       stringsAsFactors=FALSE)
  if(anyDuplicated(q$record_id)) stop("Duplicate record IDs.")
  root <- file.path(w09,"openalex_content")
  dir.create(root,recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(w09,"structured"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(w09,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  log <- file.path(w09,"w09_openalex_content_retrieval.jsonl")
  report <- file.path(w09,"w09_fulltext_retrieval_report.csv")
  readjl <- function(f) {
    if(!file.exists(f)) return(list())
    v <- readLines(f,warn=FALSE,encoding="UTF-8")
    lapply(v[nzchar(trimws(v))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  prev <- readjl(log)
  if(length(prev) && anyDuplicated(vapply(prev,function(z)z$record_id,character(1))))
    stop("Duplicate retrieval log records; investigate before resuming.")
  done <- vapply(prev,function(z)z$record_id,character(1))
  key <- Sys.getenv("OPENALEX_API_KEY")
  elsevier <- Sys.getenv("SCOPUS_API_TOKEN")
  inst <- Sys.getenv("SCOPUS_INSTTOKEN")
  clean <- function(z) {
    if(!length(z)||is.na(z[[1L]])) return("")
    v <- trimws(as.character(z[[1L]]))
    v <- sub("^https?://(dx\\.)?doi\\.org/","",v,ignore.case=TRUE)
    sub("^doi:\\s*","",v,ignore.case=TRUE)
  }
  norm <- function(z) trimws(gsub("[^a-z0-9]+"," ",tolower(as.character(z))))
  sha <- function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  extok <- function(f,ext) {
    if(!file.exists(f)||is.na(file.info(f)$size)||file.info(f)$size<100)return(FALSE)
    if(ext=="pdf") {
      cn<-file(f,"rb");on.exit(close(cn))
      return(identical(readBin(cn,"raw",n=5),charToRaw("%PDF-")))
    }
    if(ext=="xml") {
      x<-tryCatch(xml2::read_xml(f),error=function(e)NULL)
      if(is.null(x))return(FALSE)
      body<-tolower(paste(xml2::xml_name(xml2::xml_find_all(x,"//*")),collapse=" "))
      return(grepl("body|div|section",body)&&nchar(xml2::xml_text(x))>1200)
    }
    if(ext=="json") {
      x<-tryCatch(jsonlite::fromJSON(f,simplifyVector=FALSE),error=function(e)NULL)
      return(is.list(x)&&!is.null(x[["full-text-retrieval-response"]]))
    }
    FALSE
  }
  download <- function(url,target,ext,headers=list()) {
    if(!is.character(url)||length(url)!=1L||!grepl("^https://",url))
      return(list(ok=FALSE,reason="invalid_url"))
    tmp<-tempfile(fileext=paste0(".",ext));on.exit(unlink(tmp),add=TRUE)
    out<-tryCatch({
      req<-httr2::request(url)|>httr2::req_timeout(90)
      if(length(headers))req<-do.call(httr2::req_headers,c(list(req),headers))
      httr2::req_perform(req,path=tmp)
    },error=function(e)e)
    if(inherits(out,"error"))return(list(ok=FALSE,reason=conditionMessage(out)))
    if(!extok(tmp,ext))return(list(ok=FALSE,reason="not_substantive_fulltext"))
    if(!file.exists(target)&&!file.copy(tmp,target,overwrite=FALSE))
      stop("Unable to store document: ",target)
    list(ok=TRUE,reason="saved",path=target,sha256=sha(target))
  }
  # First use already downloaded candidate-matched files without overwriting.
  # Files from earlier W09 runs are copied only if title and DOI match.
  olddirs <- setdiff(list.dirs(dirname(w09),recursive=FALSE,full.names=TRUE),w09)
  pastq <- lapply(olddirs,function(d) {
    f<-file.path(d,"retrieval_queue.csv")
    if(file.exists(f))tryCatch(utils::read.csv(f,stringsAsFactors=FALSE),
                                error=function(e)NULL) else NULL
  })
  remaining<-q[!q$record_id%in%done,,drop=FALSE]
  for(i in seq_len(nrow(remaining))) {
    id<-remaining$record_id[[i]]
    doi<-clean(remaining$doi[[i]])
    title<-as.character(remaining$title[[i]])
    entry<-list(record_id=id,doi=doi,openalex_match="not_checked",
      openalex_has_content_xml=FALSE,openalex_has_content_pdf=FALSE,
      openalex_status="not_attempted",sciencedirect_status="not_attempted",
      outcome="unavailable",format=NULL,file=NULL,sha256=NULL,
      attempts=list())
    targets<-c(xml=file.path(w09,"structured",paste0(id,".xml")),
      json=file.path(w09,"structured",paste0(id,".json")),
      pdf=file.path(w09,"pdfs",paste0(id,".pdf")))
    setfile<-function(f,origin) {
      ext<-tolower(tools::file_ext(f))
      entry$outcome<-"downloaded_unverified_identity"
      entry$format<-ext;entry$file<-f
      entry$sha256<-sha(f)
      entry$source<-origin
      entry
    }
    # Always check hosted-content availability, including for reused files.
    work<-NULL
    if(nzchar(doi)) {
      url<-paste0("https://api.openalex.org/works/https://doi.org/",
                  utils::URLencode(doi,reserved=TRUE))
      req<-httr2::request(url)|>httr2::req_timeout(40)
      if(nzchar(key))req<-httr2::req_url_query(req,api_key=key)
      candidate<-tryCatch(httr2::resp_body_json(httr2::req_perform(req),
                      simplifyVector=FALSE),error=function(e)e)
      if(!inherits(candidate,"error") &&
         identical(tolower(clean(candidate$doi)),tolower(doi))) {
        work<-candidate
        entry$openalex_match<-"verified_doi"
        entry$openalex_has_content_xml<-isTRUE(work$has_content$grobid_xml)
        entry$openalex_has_content_pdf<-isTRUE(work$has_content$pdf)
        entry$openalex_status<-"availability_checked"
      } else {
        entry$openalex_match<-"unverified_or_lookup_error"
        entry$openalex_status<-"not_retrieved"
      }
    } else entry$openalex_status<-"no_doi"
    existing<-names(targets)[vapply(names(targets),
                                  function(ex)extok(targets[[ex]],ex),logical(1))]
    if(length(existing)) {
      entry<-setfile(targets[[existing[[1L]]]],"existing")
      entry$openalex_status<-"existing_file_reused"
    } else {
      for(j in seq_along(olddirs)) {
        tab<-pastq[[j]]
        if(is.null(tab))next
        ix<-which(tab$record_id==id)
        if(length(ix)!=1L||!identical(norm(tab$title[ix]),norm(title))||
           !identical(tolower(clean(tab$doi[ix])),tolower(doi)))next
        for(ex in names(targets)) {
          f<-file.path(olddirs[[j]],if(ex=="pdf")"pdfs" else "structured",
                       paste0(id,".",ex))
          if(extok(f,ex)&&!file.exists(targets[[ex]])) {
            if(!file.copy(f,targets[[ex]],overwrite=FALSE))
              stop("Failed copying archived full text.")
            entry<-setfile(targets[[ex]],"existing_verified_queue_metadata")
            break
          }
        }
        if(!is.null(entry$file))break
      }
    }
    if(is.null(entry$file)) {
      # Reuse verified Work metadata from the availability lookup above.
      if(!is.null(work)) {
        has<-work$has_content
          wid<-sub("^.*/","",work$id)
          # Official content API. No publisher/repository location URLs.
          base<-paste0("https://content.openalex.org/works/",wid)
          for(ex in c("xml","pdf")) {
            available<-if(ex=="xml")isTRUE(has$grobid_xml) else isTRUE(has$pdf)
            if(!available)next
            uri<-paste0(base,if(ex=="xml")".grobid-xml" else ".pdf")
            if(nzchar(key))uri<-paste0(uri,"?api_key=",
                                      utils::URLencode(key,reserved=TRUE))
            res<-download(uri,targets[[ex]],ex)
            entry$attempts[[length(entry$attempts)+1L]]<-
              list(provider="OpenAlex",format=ex,
                   status=if(res$ok)"saved" else res$reason)
            if(res$ok) {
              entry<-setfile(res$path,"OpenAlex_hosted_content")
              entry$openalex_status<-"saved"
              break
            }
          }
          if(!identical(entry$openalex_status,"saved"))
            entry$openalex_status<-"no_usable_hosted_content"
      }
    }
    if(is.null(entry$file)) {
      # Only unsuccessful OpenAlex candidates fall through to ScienceDirect.
      if(!nzchar(doi)) entry$sciencedirect_status<-"no_verified_doi"
      else if(!nzchar(elsevier))entry$sciencedirect_status<-"no_api_key"
      else {
        base<-paste0("https://api.elsevier.com/content/article/doi/",
                     utils::URLencode(doi,reserved=TRUE))
        for(fmt in c("text/xml","application/json","application/pdf")) {
          ex<-switch(fmt,"text/xml"="xml","application/json"="json",
                     "application/pdf"="pdf")
          req<-httr2::request(base)|>
            httr2::req_url_query(view="FULL",httpAccept=fmt)|>
            httr2::req_headers("X-ELS-APIKey"=elsevier,Accept=fmt)|>
            httr2::req_timeout(90)
          if(nzchar(inst))req<-httr2::req_headers(req,"X-ELS-Insttoken"=inst)
          tmp<-tempfile(fileext=paste0(".",ex))
          response<-tryCatch(httr2::req_perform(req,path=tmp),
                              error=function(e)e)
          good<-!inherits(response,"error")&&extok(tmp,ex)
          entry$attempts[[length(entry$attempts)+1L]]<-
            list(provider="ScienceDirect",format=ex,
                 status=if(good)"saved" else if(inherits(response,"error"))
                   conditionMessage(response) else "not_substantive_fulltext")
          if(good) {
            if(!file.exists(targets[[ex]])&&!file.copy(tmp,targets[[ex]],
                                                       overwrite=FALSE))
              stop("Unable to save ScienceDirect document.")
            entry<-setfile(targets[[ex]],"ScienceDirect")
            entry$sciencedirect_status<-"saved"
          }
          unlink(tmp)
          if(good)break
        }
        if(identical(entry$sciencedirect_status,"not_attempted"))
          entry$sciencedirect_status<-"no_usable_fulltext"
      }
    } else entry$sciencedirect_status<-"not_needed"
    cat(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null"),"\n",
        file=log,append=TRUE,sep="")
    message(sprintf("[W09] %d/%d %s | OpenAlex: %s | ScienceDirect: %s | final: %s",
                    i,nrow(remaining),id,entry$openalex_status,
                    entry$sciencedirect_status,entry$outcome))
  }
  all<-readjl(log)
  rows<-lapply(all,function(z)data.frame(
    record_id=z$record_id,doi=if(is.null(z$doi))"" else z$doi,
    openalex_match=z$openalex_match,
    openalex_xml_available=isTRUE(z$openalex_has_content_xml),
    openalex_document_available=isTRUE(z$openalex_has_content_pdf),
    openalex_status=z$openalex_status,
    sciencedirect_status=z$sciencedirect_status,
    outcome=z$outcome,source=if(is.null(z$source))"" else z$source,
    format=if(is.null(z$format))"" else z$format,
    file=if(is.null(z$file))"" else z$file,
    sha256=if(is.null(z$sha256))"" else z$sha256,
    stringsAsFactors=FALSE))
  df<-do.call(rbind,rows)
  df<-df[match(q$record_id,df$record_id),,drop=FALSE]
  utils::write.csv(df,report,row.names=FALSE)
  message(sprintf("[W09 complete] OpenAlex hosted content availability: %d/%d (GROBID XML: %d, document: %d).",
    sum(df$openalex_xml_available|df$openalex_document_available),nrow(df),
    sum(df$openalex_xml_available),sum(df$openalex_document_available)))
  message(sprintf("[W09 complete] %d/%d retrieved; OpenAlex-hosted XML: %d; OpenAlex-hosted document: %d; ScienceDirect: %d; unavailable: %d.",
    sum(df$outcome!="unavailable"),nrow(df),
    sum(df$source=="OpenAlex_hosted_content"&df$format=="xml"),
    sum(df$source=="OpenAlex_hosted_content"&df$format=="pdf"),
    sum(df$source=="ScienceDirect"),sum(df$outcome=="unavailable")))
  invisible(report)
}
