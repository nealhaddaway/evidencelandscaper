#' Run original W02-main Europe PMC and Scopus enrichment locally
#'
#' Produces separate original W02-main enriched and audit files. The
#' ScienceDirect cached fallback is applied subsequently, never over W01.
#' @param scored W01 full_scoring output.
#' @param use_scopus Whether to use the existing Scopus fallback.
#' @return W02-main output directory, invisibly.
#' @export
run_w02_main <- function(scored,use_scopus=TRUE) {
  input<-prepare_w02(scored)
  folder<-dirname(input)
  result<-file.path(folder,"enriched.jsonl")
  audit<-file.path(folder,"audit.jsonl")
  report<-file.path(folder,"report.json")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(file.exists(report)) {
    meta<-jsonlite::fromJSON(report)
    if(!file.exists(result)||!file.exists(audit)||
       !identical(meta$input_sha256,sha(input))||
       !identical(meta$output_sha256,sha(result))||
       !identical(meta$audit_sha256,sha(audit)))
      stop("Existing W02-main results failed checksum validation.")
    message("[W02] Reusing verified Europe PMC/Scopus enrichment.")
    return(invisible(folder))
  }
  if(any(file.exists(c(result,audit))))
    stop("Incomplete W02-main output present; preserve files for inspection.")
  script<-system.file("w02","workflow_02_metadata_enrichment.R",
                      package="evidencelandscaper")
  if(!nzchar(script))stop("Original W02-main enrichment script not installed.")
  args<-c(script,"--input",input,"--output",result,"--audit",audit,
          "--report",report,"--skip-scopus",if(use_scopus)"false" else "true",
          "--enrich-author-keywords","false")
  out<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
    args=vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
  status<-attr(out,"status")
  if(!is.null(status)&&status!=0L)
    stop("Original W02-main enrichment failed:\n",
         paste(tail(out,35L),collapse="\n"),call.=FALSE)
  if(!file.exists(report)||!file.exists(result))
    stop("W02-main outputs missing.")
  message("[W02] Original Europe PMC and Scopus enrichment completed.")
  invisible(folder)
}
