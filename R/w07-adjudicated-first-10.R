#' Code included abstracts from the adjudicated first W04 cohort
#'
#' Select the revised W07 prompt and ontology interactively or supply paths.
#' Uses the checksum-verified human-authoritative W04 screening layer, not
#' the model-only consensus and not the remaining-90-percent screening run.
#' Existing W04 and W07 outputs are not overwritten.
#'
#' @param prompt_file Revised W07 mechanism prompt TXT, or NULL to choose.
#' @param ontology_file Revised mechanism ontology CSV, or NULL to choose.
#' @param sample_w04 Exact first-cohort W04 run, or NULL to discover uniquely.
#' @param map_path Existing Ostara map; defaults to Ostara_collective_action_map.
#' @return W07 coding run directory, invisibly.
#' @export
run_w07_adjudicated_first_10 <- function(prompt_file=NULL,
                                         ontology_file=NULL,
                                         sample_w04=NULL,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  if(is.null(prompt_file)) {
    message("[W07 first 10%] Select the REVISED MECHANISM CODING PROMPT (.txt).")
    if(!interactive())stop("Supply prompt_file in non-interactive sessions.")
    prompt_file<-file.choose()
  }
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  if(!grepl("[.]txt$",prompt_file,ignore.case=TRUE))
    stop("The W07 coding prompt must be a .txt file.")
  message("[W07 first 10%] Prompt: ",prompt_file)
  if(is.null(ontology_file)) {
    message("[W07 first 10%] Select the REVISED MECHANISM ONTOLOGY (.csv).")
    if(!interactive())stop("Supply ontology_file in non-interactive sessions.")
    ontology_file<-file.choose()
  }
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  if(!grepl("[.]csv$",ontology_file,ignore.case=TRUE))
    stop("The W07 mechanism ontology must be a .csv file.")
  message("[W07 first 10%] Ontology: ",ontology_file)
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
      if(inherits(a,"error"))stop("Invalid W04 human-authoritative snapshot ",
                                 "in ",p,": ",conditionMessage(a))
      !is.null(a) && isTRUE(a$complete)
    },logical(1))]
    if(length(ready)!=1L)
      stop("Expected exactly one selected W04 cohort with a COMPLETE ",
           "human-authoritative screening layer; found ",length(ready),
           ". Supply sample_w04 explicitly if multiple cohorts exist.")
    sample_w04<-ready[[1L]]
  }
  sample_w04<-normalizePath(sample_w04,winslash="/",mustWork=TRUE)
  if(!startsWith(sample_w04,paste0(map_path,"/runs/W01/")) ||
     !startsWith(basename(sample_w04),"selected-"))
    stop("sample_w04 must be an original selected-cohort W04 run in this map.")
  human<-.w04_latest_authoritative(sample_w04)
  if(is.null(human)||!isTRUE(human$complete))
    stop("First-cohort human adjudications are not fully final: ",
         "import all remaining uncertain decisions before W07.")
  rows<-readLines(human$layer,warn=FALSE,encoding="UTF-8")
  rows<-lapply(rows[nzchar(trimws(rows))],jsonlite::fromJSON,
               simplifyVector=FALSE)
  decisions<-vapply(rows,function(r)as.character(r$screening$decision),
                    character(1))
  if(any(!decisions%in%c("retain","exclude")))
    stop("First-cohort effective W04 layer has unresolved decisions.")
  message("[W07 first 10%] Using human-authoritative W04: ",human$layer)
  message("[W07 first 10%] Included: ",sum(decisions=="retain"),
          "; excluded: ",sum(decisions=="exclude"),
          "; records in first cohort: ",length(rows))
  if(!any(decisions=="retain"))
    stop("No included abstracts in the human-authoritative first cohort.")
  message("[W07 first 10%] Starting Luna coding on INCLUDED records only.")
  trial_w07(ontology_file=ontology_file,prompt_file=prompt_file,
            n=0L,model="gpt-5.6-luna",map_path=map_path,
            w04_run=sample_w04,provisional_retains=FALSE)
}
