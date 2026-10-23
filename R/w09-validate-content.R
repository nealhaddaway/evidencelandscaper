#' Validate W09 retrievals and reconcile OpenAlex-hosted availability
#'
#' Read-only inspection of the saved retrievals. No API requests or models.
#' Identity is confirmed only where the document contains the expected DOI.
#' @param w09 Prepared W09 case-only directory.
#' @return Path to validation CSV.
#' @export
validate_w09_fulltexts <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                    stringsAsFactors=FALSE)
 f<-file.path(w09,"w09_fulltext_retrieval_report.csv")
 if(!file.exists(f))stop("Run retrieve_w09_openalex_content() first.")
 r<-utils::read.csv(f,stringsAsFactors=FALSE)
 if(anyDuplicated(r$record_id)||!setequal(q$record_id,r$record_id))
   stop("W09 retrieval report and queue differ.")
 r<-r[match(q$record_id,r$record_id),,drop=FALSE]
 scalar<-function(x)if(!length(x)||is.na(x[[1L]]))"" else as.character(x[[1L]])
 normdoi<-function(x)tolower(sub("^https?://(dx\\.)?doi\\.org/","",
                                 trimws(scalar(x)),ignore.case=TRUE))
 normtitle<-function(x)gsub("[^a-z0-9]+"," ",tolower(scalar(x)))
 inspect<-function(file,format,doi,title) {
   result<-list(valid_format=FALSE,substantive=FALSE,
                doi_in_text=FALSE,title_in_text=FALSE,
                identity="unverified",extractable=FALSE,
                text_characters=0L,notes="")
   if(!nzchar(file)||!file.exists(file)) {
     result$notes<-"no_local_file";return(result)
   }
   if(format=="xml") {
     doc<-tryCatch(xml2::read_xml(file),error=function(e)e)
     if(inherits(doc,"error")) {
       result$notes<-"invalid_xml";return(result)
     }
     result$valid_format<-TRUE
     nodes<-xml2::xml_find_all(doc,
       "//*[local-name()='body']//*[local-name()='p'] | //*[local-name()='para']")
     txt<-paste(xml2::xml_text(nodes),collapse=" ")
     if(!nzchar(txt))txt<-xml2::xml_text(doc)
     result$substantive<-nchar(txt)>1200 && length(nodes)>=3
     if(!result$substantive) result$notes<-"insufficient_article_body"
     searchtxt<-paste(xml2::xml_text(doc),collapse=" ")
   } else if(format=="json") {
     z<-tryCatch(jsonlite::fromJSON(file,simplifyVector=FALSE),
                 error=function(e)e)
     if(inherits(z,"error")) {
       result$notes<-"invalid_json";return(result)
     }
     result$valid_format<-TRUE
     txt<-paste(readLines(file,warn=FALSE),collapse=" ")
     result$substantive<-nchar(txt)>5000 &&
       !is.null(z[["full-text-retrieval-response"]])
     searchtxt<-txt
     if(!result$substantive)result$notes<-"json_fulltext_not_verified"
   } else if(format=="pdf") {
     con<-base::file(file,"rb");magic<-readBin(con,"raw",n=5);close(con)
     result$valid_format<-identical(magic,charToRaw("%PDF-"))
     if(!result$valid_format) {
       result$notes<-"invalid_document";return(result)
     }
     result$notes<-"requires_grobid_conversion"
     return(result)
   } else {
     result$notes<-"unsupported_format";return(result)
   }
   result$text_characters<-nchar(txt)
   doi<-normdoi(doi)
   result$doi_in_text<-nzchar(doi)&&grepl(doi,tolower(searchtxt),fixed=TRUE)
   t<-trimws(normtitle(title))
   result$title_in_text<-nzchar(t)&&
       grepl(t,normtitle(searchtxt),fixed=TRUE)
   result$identity<-if(result$doi_in_text)"doi_confirmed" else
                     if(result$title_in_text)"title_confirmed" else "unverified"
   result$extractable<-result$substantive &&
      result$identity!="unverified"
   if(result$substantive&&!result$extractable)
      result$notes<-"substantive_text_identity_unverified"
   result
 }
 out<-vector("list",nrow(q))
 for(i in seq_len(nrow(q))) {
   id<-q$record_id[[i]]
   file<-scalar(r$file[i])
   format<-tolower(scalar(r$format[i]))
   assessment<-inspect(file,format,q$doi[i],q$title[i])
   xml_available<-isTRUE(r$openalex_xml_available[[i]])
   status<-if(assessment$extractable)"extraction_ready" else
     if(assessment$valid_format&&format=="pdf")"requires_grobid" else
     if(assessment$valid_format)"requires_review" else "unavailable_or_invalid"
   out[[i]]<-data.frame(record_id=id,doi=scalar(q$doi[i]),
      openalex_grobid_available=xml_available,
      local_format=format,source=scalar(r$source[i]),
      file=file,format_valid=assessment$valid_format,
      substantive_text=assessment$substantive,
      text_characters=assessment$text_characters,
      identity=assessment$identity,
      status=status,notes=assessment$notes,
      openalex_xml_reconciliation=if(!xml_available)"not_hosted" else
         if(format=="xml")"xml_present" else "xml_missing_or_not_preferred",
      stringsAsFactors=FALSE)
   message("[W09 validation] ",i,"/",nrow(q)," ",id,": ",status)
 }
 df<-do.call(rbind,out)
 dest<-file.path(w09,"w09_fulltext_validation.csv")
 if(file.exists(dest)) {
   old<-file.path(w09,paste0("w09_fulltext_validation-",
      substr(digest::digest(file=dest,algo="sha256",serialize=FALSE),1,12),".csv"))
   if(!file.exists(old)&&!file.copy(dest,old))stop("Cannot archive earlier report.")
 }
 utils::write.csv(df,dest,row.names=FALSE)
 message("[W09 validation] ",sum(df$status=="extraction_ready")," ready; ",
         sum(df$status=="requires_grobid")," require GROBID; ",
         sum(df$status=="requires_review")," require review; ",
         sum(df$status=="unavailable_or_invalid")," unavailable/invalid.")
 message("[W09 validation] OpenAlex GROBID XML: ",
         sum(df$openalex_grobid_available)," flagged; ",
         sum(df$openalex_xml_reconciliation=="xml_present")," local XML; ",
         sum(df$openalex_xml_reconciliation=="xml_missing_or_not_preferred"),
         " no selected XML.")
 invisible(dest)
}
