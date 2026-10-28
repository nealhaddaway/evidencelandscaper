#' Finalise human-reviewed Ostara mechanisms and create FT website inputs
#'
#' Explicitly fails if a publication mechanism review has not been completed.
#' Leaves W07, W09 and W10 original files unchanged.
#' @param w07_run Full completed W07 run directory.
#' @param mechanism_review_csv Completed mechanism review worksheet.
#' @param novelty_review_csv Completed novelty review worksheet.
#' @param output_dir New destination directory.
#' @return Paths to reconciled data and FT harmonisation inputs.
#' @export
finalise_ostara_mechanism_review <- function(w07_run,mechanism_review_csv,
                                               novelty_review_csv,output_dir) {
  w07_run<-normalizePath(w07_run,mustWork=TRUE)
  ontology<-file.path(w07_run,"ontology.csv")
  coding<-file.path(w07_run,"coding.jsonl")
  if(!all(file.exists(c(ontology,coding,mechanism_review_csv,novelty_review_csv))))
    stop("W07 archive and both completed review worksheets are required")
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Refusing to overwrite previous feedback outputs")
  m<-utils::read.csv(mechanism_review_csv,stringsAsFactors=FALSE)
  n<-utils::read.csv(novelty_review_csv,stringsAsFactors=FALSE)
  if(nrow(m)){
    if(!all(c("review_complete","human_decision","reviewer")%in%names(m)))
      stop("Mechanism review fields missing")
    done<-tolower(as.character(m$review_complete))=="true"
    if(anyNA(done)||!all(done)||any(!nzchar(trimws(m$human_decision)))||
       any(!nzchar(trimws(m$reviewer))))
      stop("All case-eligible publications need explicit human mechanism review")
  }
  if(nrow(n)){
    if(!all(c("human_novelty_decision","reviewer")%in%names(n)))
      stop("Novelty review fields missing")
    if(any(!nzchar(trimws(n$reviewer)))||
       any(!nzchar(trimws(n$human_novelty_decision))))
      stop("All novelty proposals require a human decision, including uncertain")
  }
  outputs<-list(
    mechanisms=reconcile_ostara_mechanisms(coding,mechanism_review_csv,
      ontology,file.path(output_dir,"mechanism_reconciliation")),
    novelty=reconcile_ostara_novelty(novelty_review_csv,ontology,
      file.path(output_dir,"novelty_reconciliation")))
  outputs$website_inputs<-prepare_ostara_fulltext_candidates(
    outputs$novelty$csv,file.path(output_dir,"website_harmonisation"))
  invisible(outputs)
}
