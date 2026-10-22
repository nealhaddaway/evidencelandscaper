#' Open Shiny to enter missing W04 abstracts without screening decisions
#' @param w04 Optional W04 directory; finds latest full run when omitted.
#' @param map_path Local map path.
#' @return W04 directory, invisibly.
#' @export
edit_w04_missing_abstracts <- function(w04=NULL,map_path=NULL) {
 if(is.null(w04)) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  parent<-file.path(map_path,"runs","W01")
  roots<-list.dirs(parent,recursive=FALSE,full.names=TRUE)
  runs<-unlist(lapply(roots,function(z){
   d<-file.path(z,"W04_local")
   if(!dir.exists(d))return(character())
   list.dirs(d,recursive=FALSE,full.names=TRUE)
  }),use.names=FALSE)
  runs<-runs[grepl("^full-",basename(runs)) &
    file.exists(file.path(runs,"local_manifest.json"))]
  if(!length(runs))stop("No completed full W04 run.")
  runs<-runs[order(file.info(file.path(runs,"local_manifest.json"))$mtime,decreasing=TRUE)]
  w04<-runs[[1L]]
 }
 w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
 queue<-prepare_w04_review(w04,missing_abstracts=TRUE)
 if(is.null(queue))return(invisible(w04))
 state<-file.path(w04,"missing_abstract_reviewer_state")
 dir.create(state,recursive=TRUE,showWarnings=FALSE)
 script<-system.file("w04","missing_abstract_app.R",package="evidencelandscaper")
 if(!nzchar(script))stop("Abstract editor was not installed.")
 status<-system2(file.path(R.home("bin"),"Rscript"),
   args=vapply(c(script,queue,state),shQuote,character(1)),wait=TRUE)
 if(as.integer(status)!=0L)stop("Shiny abstract editor exited with an error.")
 invisible(w04)
}

#' Selectively rescreen only W04 records edited in the Shiny abstract editor
#' @param w04 W04 directory returned from edit_w04_missing_abstracts().
#' @param model OpenAI model.
#' @return Isolated rescreen output.
#' @export
rescreen_w04_shiny_abstracts <- function(w04=NULL,model="gpt-5.6") {
 if(is.null(w04)) {
   map_path<-open_map("Ostara_collective_action_map")
   roots<-list.dirs(file.path(map_path,"runs","W01"),recursive=FALSE,full.names=TRUE)
   runs<-unlist(lapply(roots,function(z){
     p<-file.path(z,"W04_local")
     if(dir.exists(p))list.dirs(p,recursive=FALSE,full.names=TRUE) else character()
   }),use.names=FALSE)
   runs<-runs[grepl("^full-",basename(runs)) &
     file.exists(file.path(runs,"missing_abstract_reviewer_state","abstract_corrections.jsonl"))]
   if(length(runs)!=1L)stop("Expected one full W04 run with saved Shiny abstracts; found ",length(runs),". Supply w04 explicitly.")
   w04<-runs[[1L]]
 }
 w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
 events_path<-file.path(w04,"missing_abstract_reviewer_state","abstract_corrections.jsonl")
 if(!file.exists(events_path))stop("No Shiny abstract edits found.")
 lines<-readLines(events_path,warn=FALSE,encoding="UTF-8")
 events<-lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
 final<-list()
 for(e in events) {
   if(!identical(e$schema,"local-w04-abstract-correction-v1") ||
      is.null(e$record_id)||is.null(e$abstract)||
      !nzchar(trimws(e$abstract)))stop("Invalid Shiny abstract correction.")
   final[[as.character(e$record_id)]]<-as.character(e$abstract)
 }
 if(!length(final))stop("No completed abstract corrections.")
 csv<-file.path(w04,"missing_abstract_reviewer_state","corrected_abstracts.csv")
 data<-data.frame(record_id=names(final),abstract=unlist(final,use.names=FALSE),
                  stringsAsFactors=FALSE)
 if(file.exists(csv)) {
   previous<-utils::read.csv(csv,stringsAsFactors=FALSE,check.names=FALSE)
   if(!identical(previous,data))stop("Existing rescreen input CSV differs; refusing overwrite.")
 } else utils::write.csv(data,csv,row.names=FALSE,na="")
 rescreen_w04_abstracts(w04,abstracts_csv=csv,model=model)
}
