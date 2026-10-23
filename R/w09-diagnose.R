#' Diagnose why ScienceDirect was or was not attempted in W09
#'
#' Reads local retrieval logs and PDF presence only. Makes no network/API
#' requests and never alters existing source records, downloads or logs.
#' @param w09 Existing W09 pilot directory.
#' @return A data.frame of evidence for each queued publication.
#' @export
diagnose_w09_sciencedirect <- function(w09) {
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                    stringsAsFactors=FALSE,check.names=FALSE)
 path<-file.path(w09,"w09_retrieval_v2.jsonl")
 if(!file.exists(path))stop("W09 v2 retrieval log not found.")
 lines<-readLines(path,warn=FALSE,encoding="UTF-8")
 logs<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
              simplifyVector=FALSE)
 get1<-function(x,k) {
   z<-x[[k]]
   if(is.null(z)||!length(z)||all(is.na(z)))"" else
     paste(as.character(unlist(z,use.names=FALSE)),collapse="; ")
 }
 out<-lapply(seq_len(nrow(q)),function(i) {
   id<-as.character(q$record_id[[i]])
   entries<-Filter(function(z)identical(get1(z,"record_id"),id),logs)
   entry<-if(length(entries))entries[[length(entries)]] else list()
   pdf<-file.path(w09,"pdfs",paste0(id,".pdf"))
   exists_pdf<-file.exists(pdf)
   size<-if(exists_pdf)file.info(pdf)$size else NA_real_
   valid_header<-FALSE
   if(exists_pdf && !is.na(size) && size>=5L) {
     con<-file(pdf,"rb")
     signature<-readBin(con,what="raw",n=5L)
     close(con)
     valid_header<-identical(signature,charToRaw("%PDF-"))
   }
   sc<-get1(entry,"sciencedirect_status")
   observed<-if(sc=="no_doi")"Skipped: queue DOI missing or invalid" else
      if(sc=="not_attempted" && exists_pdf)
        "PDF currently exists: prior OpenAlex/post-download exception or concurrent write possible; examine log notes" else
      if(sc=="not_attempted")
        "Unexpected skip: no current PDF; inspect original v2 execution and logs" else
      if(sc=="no_api_key")"Skipped: Elsevier API credential unavailable" else
      if(sc=="request_failed_or_not_entitled")
        "ScienceDirect request attempted; see log note for HTTP error" else
      if(sc=="no_pdf_or_not_entitled")
        "ScienceDirect response not recognised as a PDF" else
      if(sc=="downloaded_unverified")
        "ScienceDirect PDF downloaded; verify publication identity" else
        if(!length(entries))"No v2 retrieval attempt logged" else
          paste("Other status:",sc)
   data.frame(record_id=id,queued_doi=as.character(q$doi[[i]]),
      openalex_status=get1(entry,"openalex_status"),
      sciencedirect_status=sc,log_note=get1(entry,"note"),
      pdf_exists=exists_pdf,pdf_header_valid=valid_header,
      pdf_bytes=size,diagnosis=observed,stringsAsFactors=FALSE)
 })
 ans<-do.call(rbind,out)
 rownames(ans)<-NULL
 ans
}
