#' Finalise the reviewed Ostara mechanism worksheets using their provenance
#'
#' @param map Existing map path returned by open_map().
#' @param review_dir Directory containing reviewed W09/W10 worksheets.
#' @return Reconciliation outputs.
#' @export
finalise_ostara_mechanism_review_for_map <- function(map,review_dir) {
  map<-normalizePath(map,mustWork=TRUE)
  review_dir<-normalizePath(review_dir,mustWork=TRUE)
  manifest<-file.path(review_dir,"manifest.json")
  if(!file.exists(manifest))stop("Review provenance manifest missing")
  m<-jsonlite::fromJSON(manifest,simplifyVector=FALSE)
  w07<-m$w07_run
  if(is.null(w07)||!dir.exists(w07))
    stop("Exact W07 source path not recorded or unavailable. Supply an explicit W07 run.")
  output<-file.path(review_dir,"finalised_mechanism_feedback")
  finalise_ostara_mechanism_review(w07,
    mechanism_review_csv=file.path(review_dir,"mechanism_review.csv"),
    novelty_review_csv=file.path(review_dir,"novelty_review.csv"),
    output_dir=output)
}

#' Publish both abstract and case-FT mechanism candidate tabs from reviewed data
#'
#' Requires a manually harmonised and approved-for-display FT proposal CSV.
#' Does not silently accept the unreviewed automatic harmonisation template.
#' @param map Map directory returned by open_map().
#' @param abstract_harmonised_csv Reviewed abstract candidate harmonisation CSV.
#' @param review_dir Completed W09/W10 mechanism review directory.
#' @param fulltext_harmonised_csv Reviewed FT harmonisation CSV, or NULL if
#'   there are no verified novel FT proposals.
#' @return Standalone website path.
#' @export
publish_ostara_mechanism_feedback <- function(map,abstract_harmonised_csv,
                                               review_dir,
                                               fulltext_harmonised_csv=NULL) {
  map<-normalizePath(map,mustWork=TRUE)
  review_dir<-normalizePath(review_dir,mustWork=TRUE)
  root<-file.path(map,"runs","W07")
  targeted<-file.path(root,"full-170-5e43f57800-23f44de7f9-model-gpt-6-luna")
  expanded<-file.path(root,"full-631-5e43f57800-23f44de7f9-model-gpt-6-luna")
  if(!all(dir.exists(c(targeted,expanded))))
    stop("Expected original W07 run folders not present under: ",root)
  sources<-file.path(review_dir,"finalised_mechanism_feedback",
    "website_harmonisation","fulltext_candidates.csv")
  ft_csv<-NULL
  if(file.exists(sources)) {
    src<-utils::read.csv(sources,stringsAsFactors=FALSE)
    if(nrow(src)) {
      if(is.null(fulltext_harmonised_csv))
        stop("Verified FT candidates require a harmonised review CSV")
      ft_csv<-sources
    }
  }
  outdir<-file.path(map,"reports","mechanism_typology")
  dir.create(outdir,showWarnings=FALSE,recursive=TRUE)
  out<-file.path(outdir,paste0("ostara_mechanisms_feedback_",
                    format(Sys.time(),"%Y%m%d_%H%M%S"),".html"))
  publish_ostara_mechanisms(
    targeted_w07=targeted,expanded_w07=expanded,output_file=out,
    harmonised_csv=abstract_harmonised_csv,
    fulltext_candidates_csv=ft_csv,
    fulltext_harmonised_csv=if(is.null(ft_csv))NULL else fulltext_harmonised_csv)
}
