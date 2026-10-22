#' Run W04 screening on all currently W03-eligible publications
#' @param prompt_file Local W04 TXT prompt.
#' @param map_path Existing local map, defaults to Ostara.
#' @param model OpenAI model.
#' @param open_app Open human review UI if needed.
#' @return W04 local run path, invisibly.
#' @export
run_w04_full <- function(prompt_file=NULL, map_path=NULL, model="gpt-5.6",open_app=TRUE) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(is.null(prompt_file)) {
    folder<-file.path(map_path,"data","prompts","W04")
    choices<-if(dir.exists(folder))list.files(folder,pattern="\\.txt$",full.names=TRUE) else character()
    if(!length(choices))stop("No archived W04 prompt in data/prompts/W04. Supply prompt_file once.")
    if(length(choices)>1L) {
      # Resolve exactly the same prompt as the successful earlier W04 trial, rather than guessing.
      known<-list.files(file.path(map_path,"runs","W01"),pattern="screening_prompt.txt",
                         full.names=TRUE,recursive=TRUE)
      known<-known[grepl("/W04_local/trial-",known,fixed=TRUE)]
      hashes<-vapply(choices,function(x)digest::digest(file=x,algo="sha256",serialize=FALSE),character(1))
      used<-unique(vapply(known,function(x)digest::digest(file=x,algo="sha256",serialize=FALSE),character(1)))
      choices<-choices[hashes%in%used]
      if(length(choices)!=1L)
        stop("Multiple archived W04 prompts; cannot safely select the approved version. Supply prompt_file.")
    }
    prompt_file<-choices[[1L]]
    message("[W04] Using archived prompt: ",prompt_file)
  }
  parent<-file.path(map_path,"runs","W01")
  if(!dir.exists(parent))stop("No W01 runs.")
  roots<-list.dirs(parent,recursive=FALSE,full.names=TRUE)
  roots<-roots[vapply(roots,function(x)
     file.exists(file.path(x,"W03_publication_status","full","local_manifest.json")),
     logical(1))]
  if(length(roots)!=1L)
    stop("Expected one complete W03 source root; found ",length(roots),
         ". Explicitly use run_w04(scored=...,max_records=0).")
  scored<-file.path(roots[[1L]],"full_scoring")
  if(!dir.exists(scored))stop("Existing W01 full scoring folder missing.")
  prepare_w04(scored,prompt_file)
  run_w04(scored,prompt_file,max_records=0L,model=model,open_app=open_app)
}
