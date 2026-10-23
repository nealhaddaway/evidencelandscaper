#' Republish both W09 evidence products from one validated export
#'
#' Rebuilds the linked case data, then publishes the PDF report and HTML atlas.
#' All existing extraction and source files remain immutable.
#' @inheritParams republish_w09_atlas
#' @return A named list with the atlas, report and verified export directory.
#' @export
publish_w09_evidence <- function(w09,extraction_dir,coordinates_csv=NULL) {
  atlas <- republish_w09_atlas(w09,extraction_dir,coordinates_csv)
  report <- republish_w09_report(w09,extraction_dir,coordinates_csv)
  export <- dirname(atlas)
  message("[W09 publication] 2/2 outputs ready from verified export: ",export)
  invisible(list(atlas=atlas,report=report,export=export))
}
