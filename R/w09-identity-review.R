#' Record independently checked W09 publication identities
#'
#' Creates an explicit file-hash-bound review file for the two publications
#' whose GROBID headers were inaccurate. Does not overwrite existing reviews.
#' @param w09 W09 run directory.
#' @return Identity-review CSV path.
#' @export
record_w09_verified_identities <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 ids<-c("work-0e2b4d0f4cec66b0","work-9a9a378e38d2d629")
 bases<-c(
   "Manual comparison of thesis substantive text, Comunidad de Madrid and collective-action study with W02 title Estrategias para el fomento de los canales cortos de comercialización alimentaria; header extraction blank.",
   "Manual comparison of editorial body heading Transforming African food systems with W02 title Editorial: Transforming African food systems; GROBID misidentified title as EDITED AND REVIEWED BY."
 )
 queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
 if(!all(ids%in%queue$record_id))stop("Expected reviewed IDs absent from queue.")
 targets<-file.path(w09,"structured",paste0(ids,".xml"))
 if(any(!file.exists(targets)))stop("Reviewed full-text XML missing.")
 reviews<-data.frame(record_id=ids,decision="accept",
   sha256=vapply(targets,function(f)digest::digest(file=f,algo="sha256",
                          serialize=FALSE),character(1)),basis=bases,
   stringsAsFactors=FALSE)
 dest<-file.path(w09,"w09_identity_reviews.csv")
 if(file.exists(dest)) {
   old<-utils::read.csv(dest,stringsAsFactors=FALSE)
   if(!identical(old,reviews))stop("Existing identity reviews differ; refusing overwrite.")
 } else utils::write.csv(reviews,dest,row.names=FALSE)
 message("[W09] Recorded two hash-bound human-verified identities.")
 invisible(dest)
}


#' Append the verified Ethiopian barley thesis identity to W09
#'
#' The reviewed source is the locally converted GROBID TEI for the thesis
#' Foreign Investment, Organizational Innovation and Transformation in Food
#' Supply Chains: Evidence from the Ethiopian Barley Sector. The TEI header
#' incorrectly identifies a chapter title as the thesis title.
#' @param w09 Prepared W09 directory.
#' @return Path to the append-only-by-record reviewed identity CSV.
#' @export
record_w09_barley_thesis_identity <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 id<-"work-17db08a0a06fcff7"
 q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                    stringsAsFactors=FALSE)
 row<-q[q$record_id==id,,drop=FALSE]
 if(nrow(row)!=1L)stop("Expected W09 record is missing.")
 expected<-"Foreign Investment, Organizational Innovation and Transformation in Food Supply Chains: Evidence from the Ethiopian Barley Sector"
 if(!identical(as.character(row$title[[1L]]),expected))
   stop("W02 title differs from the manually checked thesis title.")
 source<-file.path(w09,"grobid_tei",paste0(id,".xml"))
 if(!file.exists(source))stop("Converted thesis XML not found.")
 doc<-xml2::read_xml(source)
 body<-xml2::xml_find_first(doc,"//*[local-name()='body']")
 if(inherits(body,"xml_missing"))stop("Missing thesis body.")
 txt<-tolower(xml2::xml_text(body))
 needed<-c("ethiopia","barley","supply chain","cooperative")
 if(!all(vapply(needed,grepl,logical(1),x=txt,fixed=TRUE)))
   stop("Thesis does not contain the expected topical anchors.")
 rownew<-data.frame(record_id=id,decision="accept",
    sha256=digest::digest(file=source,algo="sha256",serialize=FALSE),
    basis=paste("Manual review of substantive Ethiopian malt-barley PhD",
      "thesis, chapter structure and study design compared against W02.",
      "GROBID extracted chapter title rather than thesis title."),
    stringsAsFactors=FALSE)
 dest<-file.path(w09,"w09_identity_reviews.csv")
 if(!file.exists(dest))stop("Existing two identity decisions missing.")
 reviews<-utils::read.csv(dest,stringsAsFactors=FALSE)
 if(id%in%reviews$record_id) {
   old<-reviews[reviews$record_id==id,names(rownew),drop=FALSE]
   if(!identical(old,rownew))stop("Existing identity decision differs.")
   message("[W09] Thesis decision already present.")
   return(invisible(dest))
 }
 if(anyDuplicated(reviews$record_id))stop("Duplicate identity reviews.")
 reviews<-rbind(reviews,rownew)
 utils::write.csv(reviews,dest,row.names=FALSE)
 message("[W09] Added hash-bound thesis identity review: ",id)
 invisible(dest)
}
