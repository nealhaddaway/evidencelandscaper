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
