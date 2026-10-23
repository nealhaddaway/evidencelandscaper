#' Finalise a hash-verified W09 extraction inventory
#'
#' Source files remain unchanged. Refuses unresolved identities and stale
#' file paths in the selected subset, archives existing inventory versions.
#' @param w09 Existing W09 run directory.
#' @return Path to final extraction-ready CSV inventory.
#' @export
finalise_w09_inventory <- function(w09) {
 w09<-normalizePath(w09,mustWork=TRUE)
 validation<-file.path(w09,"w09_fulltext_validation.csv")
 if(!file.exists(validation))stop("Run validate_w09_fulltexts first.")
 df<-utils::read.csv(validation,stringsAsFactors=FALSE)
 queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                         stringsAsFactors=FALSE)
 if(anyDuplicated(df$record_id)||!setequal(df$record_id,queue$record_id))
   stop("Validation and queue record sets differ.")
 df<-df[df$status=="extraction_ready",,drop=FALSE]
 if(!nrow(df))stop("No extraction-ready full texts.")
 if(anyNA(df$file)||any(!file.exists(df$file)))
   stop("Extraction-ready file missing; rerun validation.")
 df$sha256<-vapply(df$file,function(x)digest::digest(file=x,
      algo="sha256",serialize=FALSE),character(1))
 df$title<-queue$title[match(df$record_id,queue$record_id)]
 df$doi<-queue$doi[match(df$record_id,queue$record_id)]
 df<-df[,c("record_id","title","doi","local_format","file",
            "sha256","identity","text_characters","status")]
 dest<-file.path(w09,"w09_extraction_ready_inventory.csv")
 if(file.exists(dest)) {
   old<-utils::read.csv(dest,stringsAsFactors=FALSE)
   if(identical(old,df)) {
     message("[W09] Reusing unchanged final inventory: ",nrow(df)," records.")
     return(invisible(dest))
   }
   archive<-file.path(w09,paste0("w09_extraction_ready_inventory-",
     substr(digest::digest(file=dest,algo="sha256",serialize=FALSE),1,12),".csv"))
   if(!file.exists(archive)&&!file.copy(dest,archive,overwrite=FALSE))
     stop("Unable to archive existing inventory.")
 }
 utils::write.csv(df,dest,row.names=FALSE,na="")
 message("[W09 final] ",nrow(df),"/",nrow(queue),
         " publications extraction-ready; ",
         nrow(queue)-nrow(df)," not extraction-ready.")
 invisible(dest)
}
