#' Prepare GROBID conversion and XML bibliographic review for W09
#'
#' Does not modify source files. If a GROBID endpoint is configured, converts
#' only documents still requiring conversion, saving to a separate directory.
#' Original documents and publisher/OpenAlex XML are never overwritten.
#' @param w09 Existing W09 retrieval directory.
#' @param grobid_url Optional GROBID service root, e.g. http://localhost:8070.
#' @return Path to an audit CSV.
#' @export
process_w09_remaining <- function(w09,grobid_url=Sys.getenv("GROBID_URL")) {
 w09<-normalizePath(w09,mustWork=TRUE)
 v<-file.path(w09,"w09_fulltext_validation.csv")
 if(!file.exists(v))stop("Run validate_w09_fulltexts() first.")
 tab<-utils::read.csv(v,stringsAsFactors=FALSE)
 selected<-tab[tab$status%in%c("requires_grobid","requires_review"),,drop=FALSE]
 outdir<-file.path(w09,"grobid_tei")
 dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
 log<-file.path(w09,"w09_remaining_processing.jsonl")
 existing<-if(file.exists(log))lapply(readLines(log,warn=FALSE),
   jsonlite::fromJSON,simplifyVector=FALSE) else list()
 previous<-vapply(existing,function(z)z$record_id,character(1))
 if(anyDuplicated(previous))stop("Duplicate processing log IDs.")
 for(i in seq_len(nrow(selected))) {
   z<-selected[i,,drop=FALSE];id<-z$record_id[[1]]
   if(id%in%previous) {
     message("[W09 processing] ",i,"/",nrow(selected)," ",id," already assessed")
     next
   }
   entry<-list(record_id=id,previous_status=z$status[[1]],
               status="not_processed",output=NULL,error=NULL,
               doi_in_header=FALSE,title_in_header=FALSE)
   if(identical(z$status[[1]],"requires_review")) {
     source<-file.path(w09,"structured",paste0(id,".xml"))
     doc<-tryCatch(xml2::read_xml(source),error=function(e)NULL)
     if(is.null(doc))entry$status<-"invalid_xml"
     else {
       queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                              stringsAsFactors=FALSE)
       q<-queue[queue$record_id==id,,drop=FALSE]
       h<-xml2::xml_find_first(doc,"//*[local-name()='teiHeader' or local-name()='front']")
       header<-if(inherits(h,"xml_missing"))"" else tolower(xml2::xml_text(h))
       doi<-tolower(sub("^https?://doi.org/","",as.character(q$doi[[1]])))
       normalize<-function(x)trimws(gsub("[^a-z0-9]+"," ",tolower(x)))
       entry$doi_in_header<-nzchar(doi)&&grepl(doi,header,fixed=TRUE)
       entry$title_in_header<-nzchar(normalize(q$title[[1]]))&&
         grepl(normalize(q$title[[1]]),normalize(header),fixed=TRUE)
       entry$status<-if(entry$doi_in_header||entry$title_in_header)
         "identity_supported_by_header" else "needs_bibliographic_review"
     }
   } else {
     target<-file.path(outdir,paste0(id,".xml"))
     if(file.exists(target))entry$status<-"existing_conversion_preserved"
     else if(!nzchar(grobid_url))entry$status<-"grobid_not_configured"
     else {
       source<-file.path(w09,"pdfs",paste0(id,".pdf"))
       tryCatch({
         if(!file.exists(source))stop("Source document missing.")
         url<-paste0(sub("/+$","",grobid_url),"/api/processFulltextDocument")
         response<-httr2::request(url)|>
           httr2::req_body_multipart(input=httr2::upload_file(source))|>
           httr2::req_timeout(180)|>httr2::req_perform()
         raw<-httr2::resp_body_raw(response)
         tmp<-tempfile(fileext=".xml")
         writeBin(raw,tmp)
         d<-xml2::read_xml(tmp)
         ps<-xml2::xml_find_all(d,
              "//*[local-name()='text']//*[local-name()='body']//*[local-name()='p']")
         if(length(ps)<3||nchar(paste(xml2::xml_text(ps),collapse=" "))<1200)
           stop("GROBID result lacks substantive article paragraphs.")
         if(!file.copy(tmp,target,overwrite=FALSE))stop("Cannot store TEI.")
         unlink(tmp)
         entry$status<-"converted_tei_unverified_identity"
         entry$output<-target
       },error=function(e){
         entry$status<-"conversion_failed"
         entry$error<-conditionMessage(e)
       })
     }
   }
   cat(jsonlite::toJSON(entry,auto_unbox=TRUE,null="null"),"\n",
       file=log,append=TRUE,sep="")
   message("[W09 processing] ",i,"/",nrow(selected)," ",id," : ",entry$status)
 }
 all<-lapply(readLines(log,warn=FALSE),jsonlite::fromJSON,
              simplifyVector=FALSE)
 rows<-do.call(rbind,lapply(all,function(z)data.frame(
   record_id=z$record_id,previous_status=z$previous_status,
   status=z$status,output=if(is.null(z$output))"" else z$output,
   error=if(is.null(z$error))"" else z$error,
   stringsAsFactors=FALSE)))
 path<-file.path(w09,"w09_remaining_processing_report.csv")
 utils::write.csv(rows,path,row.names=FALSE)
 message("[W09 processing] ",sum(rows$status=="converted_tei_unverified_identity"),
         " converted; ",sum(rows$status=="identity_supported_by_header"),
         " header-supported; ",sum(rows$status=="grobid_not_configured"),
         " awaiting GROBID configuration.")
 invisible(path)
}
