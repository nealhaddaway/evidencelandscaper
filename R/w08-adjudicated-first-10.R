#' Repeat W08 case detection on all human-adjudicated first-10% retains
#'
#' A revised case detection prompt can be selected with a local file chooser.
#' Refuses to use incomplete human adjudications or a different W04 cohort.
#' W08 saves an independent, fingerprinted run and can safely resume it.
#'
#' @param prompt_file W08 case-identification UTF-8 .txt prompt, or NULL to choose.
#' @param sample_w04 Original selected 10% W04 run; NULL discovers the unique
#'   completed human-authoritative selected cohort.
#' @param map_path Ostara project directory, defaults to open_map().
#' @return W08 output directory, invisibly.
#' @export
run_w08_adjudicated_first_10 <- function(prompt_file=NULL,
                                         sample_w04=NULL,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(is.null(prompt_file)) {
    message("[W08 first 10%] Choose the CASE-DETECTION PROMPT (.txt), ",
            "not the W04 screening or W07 mechanism prompt.")
    if(!interactive())stop("Provide prompt_file for non-interactive use.")
    prompt_file<-file.choose()
  }
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  if(!grepl("[.]txt$",prompt_file,ignore.case=TRUE))
    stop("W08 case-detection prompt must be a .txt file.")
  if(!length(readLines(prompt_file,warn=FALSE,encoding="UTF-8")))
    stop("W08 case-detection prompt is empty.")
  message("[W08 first 10%] Using prompt: ",prompt_file)
  if(is.null(sample_w04)) {
    roots<-list.dirs(file.path(map_path,"runs","W01"),
                     recursive=FALSE,full.names=TRUE)
    runs<-unlist(lapply(roots,function(root) {
      p<-file.path(root,"W04_local")
      if(!dir.exists(p))return(character())
      list.dirs(p,recursive=FALSE,full.names=TRUE)
    }),use.names=FALSE)
    runs<-runs[startsWith(basename(runs),"selected-") &
      file.exists(file.path(runs,"local_manifest.json"))]
    ready<-runs[vapply(runs,function(p) {
      a<-tryCatch(.w04_latest_authoritative(p),error=function(e)e)
      if(inherits(a,"error"))
        stop("[W08] Invalid W04 authoritative layer in ",p,
             ": ",conditionMessage(a))
      !is.null(a) && isTRUE(a$complete)
    },logical(1))]
    if(length(ready)!=1L)
      stop("Expected one completed human-adjudicated selected W04 cohort; ",
           "found ",length(ready),". Supply sample_w04 explicitly.")
    sample_w04<-ready[[1L]]
  }
  sample_w04<-normalizePath(sample_w04,winslash="/",mustWork=TRUE)
  if(!startsWith(sample_w04,paste0(map_path,"/runs/W01/"))||
     !startsWith(basename(sample_w04),"selected-"))
    stop("sample_w04 must be a selected 10% W04 run within the Ostara map.")
  human<-.w04_latest_authoritative(sample_w04)
  if(is.null(human)||!isTRUE(human$complete))
    stop("W04 human adjudications are incomplete. Do not use model-only retains.")
  layer<-human$layer
  rows<-readLines(layer,warn=FALSE,encoding="UTF-8")
  rows<-lapply(rows[nzchar(trimws(rows))],jsonlite::fromJSON,
               simplifyVector=FALSE)
  d<-vapply(rows,function(x)as.character(x$screening$decision),character(1))
  if(any(!d%in%c("retain","exclude"))||!sum(d=="retain"))
    stop("Human-adjudicated W04 layer contains unresolved or no retained records.")
  message("[W08 first 10%] Authoritative W04 layer: ",layer)
  message("[W08 first 10%] ",length(rows)," screened; ",
          sum(d=="retain")," retained; ",sum(d=="exclude"),
          " excluded. Running case detection on ALL retained abstracts.")
  trial_w08_cases(n=0L,seed=20261023L,
                  model="gpt-5.6-luna",map_path=map_path,
                  prompt_file=prompt_file,w04_run=sample_w04,
                  provisional_retains=FALSE)
}
