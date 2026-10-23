#' Republish the Ostara evidence atlas and reports
#'
#' Resolves the map, latest complete W09 inventory and the V2.5 extraction
#' explicitly. Earlier outputs remain untouched. No model/API calls.
#' @param map_path Optional Ostara map directory; defaults to open_map().
#' @param coordinates_csv Optional reviewed geocoding CSV. For exact continuity
#'   specify the same audited file used at the previous publication.
#' @param extraction_dir Optional explicitly selected completed W09 extraction.
#' @return Named paths to the HTML atlas, W09 report, full-review HTML and
#'   validated export.
#' @export
publish_w09_evidence <- function(map_path=NULL,coordinates_csv=NULL,extraction_dir=NULL) {
  if(is.null(map_path)) map_path <- open_map("Ostara_collective_action_map")
  map_path <- normalizePath(map_path,winslash="/",mustWork=TRUE)
  root <- file.path(map_path,"runs","W09")
  pilots <- if(dir.exists(root))list.dirs(root,recursive=FALSE,full.names=TRUE) else character()
  pilots <- pilots[file.exists(file.path(pilots,"w09_extraction_ready_inventory.csv"))]
  if(length(pilots)!=1L)
    stop("Expected one verified W09 inventory; specify/resolve W09 pilot uniquely.")
  w09 <- pilots[[1L]]
  if(is.null(extraction_dir)) {
    candidates <- list.dirs(w09,recursive=TRUE,full.names=TRUE)
    candidates <- candidates[file.exists(file.path(candidates,"manifest.json")) &
      file.exists(file.path(candidates,"fulltext_case_registry.jsonl"))]
    target <- "4d4457560ca19dcc3dcc52d6c6ed9637978536964c0e16690f3d3809f9835fb9"
    candidates <- candidates[vapply(candidates,function(d) {
      m <- tryCatch(jsonlite::fromJSON(file.path(d,"manifest.json")),error=function(e)NULL)
      !is.null(m) && identical(m$prompt_sha256,target) &&
        identical(m$model,"gpt-6-luna")
    },logical(1))]
    if(length(candidates)!=1L)stop("Cannot uniquely locate approved V2.5 Luna W09 extraction.")
    extraction_dir <- candidates[[1L]]
  }
  if(is.null(coordinates_csv)) {
    saved <- file.path(map_path,"report_inputs","ostara_case_coordinates.csv")
    bundled <- system.file("extdata","ostara_w09_geocoding_draft.csv",
                           package="evidencelandscaper")
    if(file.exists(saved)) {
      coordinates_csv <- saved
      message("[Ostara publication] Reusing saved reviewed coordinate registry: ",saved)
    } else if(nzchar(bundled) && file.exists(bundled)) {
      coordinates_csv <- bundled
      message("[Ostara publication] Using bundled 31-point UNVERIFIED draft; review coordinates before definitive publication.")
    } else stop("No saved or bundled geocoding provenance available.")
  }
  message("[Ostara publication] 0/3 resolving provenance-linked extraction")
  atlas <- republish_w09_atlas(w09,extraction_dir,coordinates_csv)
  message("[Ostara publication] 1/3 HTML atlas complete")
  report <- republish_w09_report(w09,extraction_dir,coordinates_csv)
  message("[Ostara publication] 2/3 W09 PDF report complete")
  full_report <- NULL
  template <- system.file("reports","Ostara_evidence_review.Rmd",package="evidencelandscaper")
  if(!nzchar(template))stop("Installed full-review report template missing.")
  if(!requireNamespace("rmarkdown",quietly=TRUE))
    stop("Install rmarkdown to render the full-review report.")
  output <- file.path(dirname(atlas),"Ostara_full_review.html")
  if(!file.exists(output)) {
    rmarkdown::render(template,output_file=basename(output),
      output_dir=dirname(output),
      params=list(map_path=map_path,w09_run=w09,w09_extract_run=extraction_dir),
      envir=new.env(parent=globalenv()),quiet=TRUE)
  }
  full_report <- output
  message("[Ostara publication] 3/3 full-review HTML rendered: ",full_report)
  invisible(list(atlas=atlas,report=report,full_report=full_report,export=dirname(atlas)))
}
