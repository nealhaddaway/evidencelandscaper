#' Recover missing OpenAlex-hosted GROBID XML in W09
#'
#' Only the records marked as hosted-XML-available but lacking selected local
#' XML are attempted. Existing files and previous logs are never overwritten.
#' The content API is used directly; no external locations are followed.
#' @param w09 Existing W09 directory.
#' @return CSV path with one row per candidate and attempt outcome.
#' @export
recover_w09_openalex_xml <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 f<-file.path(w09,"w09_fulltext_validation.csv")
 retrieval<-file.path(w09,"w09_fulltext_retrieval_report.csv")
 if(!file.exists(f)||!file.exists(retrieval))
   stop("Run W09 retrieval and validation first.")
 val<-utils::read.csv(f,stringsAsFactors=FALSE)
 ret<-utils::read.csv(retrieval,stringsAsFactors=FALSE)
 wanted<-val$record_id[val$openalex_xml_reconciliation=="xml_missing_or_not_preferred"]
 if(anyDuplicated(wanted))stop("Duplicate recovery candidates.")
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
 log<-file.path(w09,"w09_openalex_xml_recovery.jsonl")
 readjl<-function(p) {
   if(!file.exists(p))return(list())
   txt<-readLines(p,warn=FALSE)
   lapply(txt[nzchar(trimws(txt))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 past<-readjl(log)
 completed<-vapply(past,function(x)x$record_id,character(1))
 if(anyDuplicated(completed))stop("Duplicated recovery history.")
 key<-Sys.getenv("OPENALEX_API_KEY")
 if(!nzchar(key))stop("OPENALEX_API_KEY must be set for hosted content.")
 validxml<-function(path) {
   if(!file.exists(path)||file.info(path)$size<200)return(FALSE)
   d<-tryCatch(xml2::read_xml(path),error=function(e)NULL)
   if(is.null(d))return(FALSE)
   ps<-xml2::xml_find_all(d,"//*[local-name()='body']//*[local-name()='p']")
   length(ps)>=3&&nchar(paste(xml2::xml_text(ps),collapse=" "))>1200
 }
 for(i in seq_along(wanted)) {
   id<-wanted[[i]]
   if(id%in%completed) {
     message("[W09 XML recovery] ",i,"/",length(wanted)," ",id,
             " already attempted; preserving log")
     next
   }
   destination<-file.path(w09,"structured",paste0(id,".xml"))
   entry<-list(record_id=id,status="not_attempted",sha256=NULL,
               error=NULL,hosted_xml=TRUE)
   if(validxml(destination))entry$status<-"existing_substantive_xml"
   else if(file.exists(destination))entry$status<-"existing_file_not_overwritten"
   else {
     j<-match(id,q$record_id)
     doi<-if(is.na(j))"" else as.character(q$doi[[j]])
     doi<-sub("^https?://(dx\\.)?doi\\.org/","",doi,ignore.case=TRUE)
     tryCatch({
       if(!nzchar(doi))stop("No DOI in queue.")
       request<-httr2::request(paste0(
          "https://api.openalex.org/works/https://doi.org/",
          utils::URLencode(doi,reserved=TRUE)))|>
          httr2::req_url_query(api_key=key)|>
          httr2::req_timeout(45)
       work<-httr2::resp_body_json(httr2::req_perform(request),
                                   simplifyVector=FALSE)
       remote_doi<-sub("^https?://(dx\\.)?doi\\.org/","",
                       as.character(work$doi),ignore.case=TRUE)
       if(!identical(tolower(doi),tolower(remote_doi)))
         stop("OpenAlex DOI identity mismatch.")
       if(!isTRUE(work$has_content$grobid_xml))
         stop("OpenAlex no longer flags hosted XML.")
       wid<-sub("^.*/","",as.character(work$id))
       uri<-paste0("https://content.openalex.org/works/",wid,
                    ".grobid-xml")
       tmp<-tempfile(fileext=".xml");on.exit(unlink(tmp),add=TRUE)
       resp<-httr2::request(uri)|>
             httr2::req_url_query(api_key=key)|>
             httr2::req_timeout(90)|>
             httr2::req_perform(path=tmp)
       if(!validxml(tmp))stop("XML absent or insufficient article body.")
       if(!file.copy(tmp,destination,overwrite=FALSE))
         stop("Could not save recovered XML.")
       entry$status<-"recovered_xml"
       entry$sha256<-digest::digest(file=destination,algo="sha256",
                                    serialize=FALSE)
       unlink(tmp)
     },error=function(e) {
       entry$status<-"failed"
       entry$error<-conditionMessage(e)
     })
   }
   if(identical(entry$status,"existing_substantive_xml"))
     entry$sha256<-digest::digest(file=destination,algo="sha256",
                                 serialize=FALSE)
   cat(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null"),
       "\n",file=log,append=TRUE,sep="")
   message("[W09 XML recovery] ",i,"/",length(wanted)," ",id,
           " : ",entry$status)
 }
 all<-readjl(log)
 out<-do.call(rbind,lapply(all,function(x)data.frame(
   record_id=x$record_id,status=x$status,
   sha256=if(is.null(x$sha256))"" else x$sha256,
   error=if(is.null(x$error))"" else x$error,
   stringsAsFactors=FALSE)))
 dest<-file.path(w09,"w09_openalex_xml_recovery_report.csv")
 utils::write.csv(out,dest,row.names=FALSE)
 message("[W09 XML recovery] ",sum(out$status=="recovered_xml"),
         " recovered; ",sum(out$status=="failed")," failed; ",
         sum(out$status=="existing_file_not_overwritten"),
         " protected existing files.")
 invisible(dest)
}
