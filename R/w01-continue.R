#' Continue local W01 classification through LLM and human review
#'
#' Reuses previously scored candidate pairs, prepares review cases using the
#' original W01-main identifier safeguards, and optionally runs the existing
#' LLM adjudicator. Does NOT finalise deduplicated clusters. The LLM requires
#' an API key and incurs API charges.
#' @param scored Full scoring folder returned by score_w01_candidates().
#' @param run_llm Set TRUE to explicitly authorise OpenAI API calls.
#' @param model Model name; defaults to OPENAI_DUPLICATE_MODEL or gpt-5.6.
#' @param open_app Open the local Shiny reviewer for LLM-uncertain cases.
#' @return A list of paths and counts, invisibly.
#' @export
continue_w01 <- function(scored,run_llm=FALSE,
                         model=Sys.getenv("OPENAI_DUPLICATE_MODEL","gpt-5.6"),
                         open_app=TRUE) {
  if(!dir.exists(scored)) stop("Full scoring folder not found.")
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  review_dir<-file.path(dirname(scored),"review_preparation")
  prep_manifest<-file.path(review_dir,"manifest.json")
  if(file.exists(prep_manifest)) {
    prep<-jsonlite::fromJSON(prep_manifest,simplifyVector=FALSE)
    queue<-file.path(review_dir,"w01_review_cases.jsonl")
    if(!file.exists(queue) ||
       !identical(prep$queue_sha256,
         digest::digest(file=queue,algo="sha256",serialize=FALSE)))
      stop("Previously prepared W01 queue failed checksum validation.")
    message("[W01] Reusing verified review-case preparation.")
  } else if(dir.exists(review_dir)) {
    existing <- list.files(review_dir, recursive=TRUE, all.files=TRUE,
                           no..=TRUE, full.names=FALSE)
    stop("W01 review preparation stopped before writing its completion manifest. ",
         "The files have been preserved. Existing paths: ",
         if(length(existing)) paste(head(existing, 25L),collapse=", ") else "(empty directory)",
         ". Please share this error and the paths shown; do not delete or overwrite them.",
         call.=FALSE)
  } else {
    queue<-prepare_w01_review(scored)
  }
  original_cases<-readLines(queue,warn=FALSE)
  if(!run_llm) {
    message("[W01] Review case preparation complete; LLM not authorised.")
    return(invisible(list(review_queue=queue,llm_dir=NULL,human_queue=NULL)))
  }
  llm_dir<-file.path(review_dir,"llm_adjudication")
  summary_path<-file.path(llm_dir,"summary.json")
  if(file.exists(summary_path)) {
    x<-jsonlite::fromJSON(summary_path,simplifyVector=FALSE)
    if(!identical(x$input_sha256,
      digest::digest(file=queue,algo="sha256",serialize=FALSE)))
      stop("LLM summary belongs to a different W01 queue.")
    message("[W01] Reusing existing LLM output.")
  } else {
    llm_dir<-run_w01_llm(queue,model=model)
  }
  human_queue<-file.path(llm_dir,"human_review_cases.jsonl")
  if(!file.exists(human_queue)) stop("LLM human queue missing.")
  n<-length(readLines(human_queue,warn=FALSE))
  message("[W01] ",n," cases require human review.")
  if(n>0L && isTRUE(open_app)) adjudicate_w01(human_queue)
  invisible(list(review_queue=queue,llm_dir=llm_dir,
                 human_queue=human_queue,human_cases=n))
}
