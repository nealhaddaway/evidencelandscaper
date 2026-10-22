#' Archive an editable prompt in a map's data directory
#'
#' Each distinct prompt is retained with its checksum. Existing versions are
#' never overwritten; source files and previous run-specific copies remain intact.
#' @param prompt_file Existing UTF-8 plain-text prompt.
#' @param stage W04 or W07.
#' @param map_path Map directory; defaults to Ostara project.
#' @return Path to the archived prompt.
#' @export
archive_map_prompt <- function(prompt_file, stage=c("W04","W07"),map_path=NULL) {
  stage<-match.arg(stage)
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  if(!grepl("\\.txt$",prompt_file,ignore.case=TRUE))
    stop("Prompt must be a UTF-8 .txt file.")
  lines<-readLines(prompt_file,warn=FALSE,encoding="UTF-8")
  if(!length(lines)||!nzchar(trimws(paste(lines,collapse="\n"))))
    stop("Prompt file is empty.")
  hash<-digest::digest(file=prompt_file,algo="sha256",serialize=FALSE)
  destdir<-file.path(map_path,"data","prompts",stage)
  dir.create(destdir,recursive=TRUE,showWarnings=FALSE)
  stem<-sub("\\.[Tt][Xx][Tt]$","",basename(prompt_file))
  stem<-gsub("[^A-Za-z0-9._-]","_",stem)
  dest<-file.path(destdir,paste0(stem,"-",substr(hash,1L,12L),".txt"))
  if(file.exists(dest)) {
    if(!identical(digest::digest(file=dest,algo="sha256",serialize=FALSE),hash))
      stop("Archived prompt has changed: ",dest)
  } else {
    if(!file.copy(prompt_file,dest,overwrite=FALSE))
      stop("Unable to archive prompt.")
    if(!identical(digest::digest(file=dest,algo="sha256",serialize=FALSE),hash))
      stop("Archived prompt copy failed checksum verification.")
  }
  message("[",stage,"] Archived prompt: ",dest)
  invisible(dest)
}
