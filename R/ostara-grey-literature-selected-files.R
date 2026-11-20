#' Extract OSTARA grey-literature cases from selected individual files
#'
#' Accepts an explicit character vector of full paths; unlike folder mode,
#' does not scan neighbouring files. Supports PDF, XML, HTML, HTM and ASPX
#' across different directories. Runs through the existing staging, SHA256
#' provenance, exact prompt/schema versioning and resumable W09 checkpoints.
#'
#' @param map_path Existing OSTARA map root.
#' @param files Character vector of individual file paths (one or more).
#' @param batch_id New batch label; reuse unchanged to resume this exact batch.
#' @param prompt_file Exact revised grey-literature extraction prompt TXT.
#' @param schema_file Authoritative 41-field grey-literature CSV.
#' @param run_extraction TRUE uses Luna; FALSE stages and reports without API.
#' @return Existing grey literature result paths and counts, invisibly.
#' @export
run_ostara_grey_literature_selected_files <- function(
    map_path,files,batch_id="grey_selected_files_01",
    prompt_file=NULL,schema_file=NULL,run_extraction=TRUE) {
  if(missing(files)||!is.character(files)||!length(files))
    stop("Provide a vector of selected paths in files.")
  run_ostara_grey_literature_fulltexts(
    map_path=map_path,
    input_files=files,
    batch_id=batch_id,
    prompt_file=prompt_file,
    schema_file=schema_file,
    run_extraction=run_extraction
  )
}
