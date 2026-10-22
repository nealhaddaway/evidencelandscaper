#' Place legacy W07 local trial outputs into the dedicated W07 run directory
#'
#' Copies rather than deletes historical runs and verifies byte-for-byte SHA-256
#' checksums of every copied file. No model calls are made.
#' @param map_path Existing evidence-map project. Defaults to Ostara.
#' @return New W07 trial folder paths invisibly.
#' @export
organise_w07_trials <- function(map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  parent<-file.path(map_path,"runs","W01")
  roots<-if(dir.exists(parent))list.dirs(parent,recursive=FALSE,full.names=TRUE) else character()
  old<-unlist(lapply(roots,function(x){
    p<-file.path(x,"W07_local")
    if(dir.exists(p))list.dirs(p,recursive=FALSE,full.names=TRUE) else character()
  }),use.names=FALSE)
  if(!length(old)) {
    message("[W07] No legacy W07 trial folders found.")
    return(invisible(character()))
  }
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  target<-file.path(map_path,"runs","W07")
  dir.create(target,recursive=TRUE,showWarnings=FALSE)
  result<-character()
  for(src in old) {
    manifest<-file.path(src,"manifest.json")
    if(!file.exists(manifest))stop("Legacy W07 run lacks manifest: ",src)
    info<-jsonlite::fromJSON(manifest,simplifyVector=FALSE)
    if(!identical(info$schema,"local-w07-abstract-trial-v1"))
      stop("Unexpected legacy W07 trial schema: ",src)
    dst<-file.path(target,basename(src))
    files<-list.files(src,all.files=TRUE,recursive=TRUE,
                      include.dirs=FALSE,no..=TRUE)
    if(!length(files))stop("Empty legacy W07 trial: ",src)
    for(f in files) {
      from<-file.path(src,f)
      to<-file.path(dst,f)
      dir.create(dirname(to),recursive=TRUE,showWarnings=FALSE)
      if(file.exists(to)) {
        if(!identical(sha(from),sha(to)))
          stop("W07 destination file differs; refusing overwrite: ",to)
      } else {
        if(!file.copy(from,to))stop("Could not copy W07 artefact: ",f)
        if(!identical(sha(from),sha(to)))
          stop("Copied W07 artefact failed checksum verification: ",f)
      }
    }
    # Confirm the destination is a complete mirror.
    for(f in files)
      if(!identical(sha(file.path(src,f)),sha(file.path(dst,f))))
        stop("W07 relocation failed final checksum verification: ",f)
    result<-c(result,dst)
    message("[W07] Preserved ",length(files)," files in ",dst)
  }
  invisible(result)
}
