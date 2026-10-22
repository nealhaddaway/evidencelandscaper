#' Run upstream W03 publication-status surveillance against final W02
#'
#' Uses the original OpenAlex DOI-batch and editorial-title screening script.
#' Does not modify finalised W02 or dispatch W04.
#' @param scored Finalised W01 run's full_scoring folder.
#' @param limit 0 for whole corpus, positive for a diagnostic subset.
#' @param batch_size OpenAlex DOI batch size, at most 100.
#' @return W03 output directory invisibly.
#' @export
run_w03 <- function(scored,limit=0L,batch_size=100L) {
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  root<-dirname(scored)
  w02<-file.path(root,"W02_main","finalised")
  canonical<-file.path(w02,"canonical.jsonl")
  summary_path<-file.path(w02,"summary.json")
  if(!file.exists(canonical)||!file.exists(summary_path))
    stop("Finalised W02 canonical input missing.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  w02_summary<-jsonlite::fromJSON(summary_path,simplifyVector=FALSE)
  if(!identical(w02_summary$status,"final"))
    stop("W02 status is not final.")
  expected<-unlist(w02_summary$checksums,use.names=FALSE)
  if(!identical(tail(expected,1L),sha(file.path(w02,"application_report.json"))))
    stop("W02 finalisation checksums inconsistent.")
  if(!identical(expected[[length(expected)-1L]],sha(canonical)))
    stop("W02 canonical input checksum mismatch.")
  limit<-as.integer(limit);batch_size<-as.integer(batch_size)
  if(is.na(limit)||limit<0L||is.na(batch_size)||batch_size<1L||batch_size>100L)
    stop("limit must be >= 0 and batch_size must be 1..100.")
  dest<-file.path(root,"W03_publication_status",
                  if(limit==0L)"full" else paste0("trial-",limit))
  paths<-list(output=file.path(dest,"publication_status.jsonl"),
              audit=file.path(dest,"api_audit.jsonl"),
              report=file.path(dest,"report.json"),
              manifest=file.path(dest,"local_manifest.json"))
  if(file.exists(paths$manifest)) {
    state<-jsonlite::fromJSON(paths$manifest)
    if(!identical(state$w02_canonical_sha256,sha(canonical)) ||
       !identical(state$status_sha256,sha(paths$output)) ||
       !identical(state$report_sha256,sha(paths$report)) ||
       !identical(state$audit_sha256,sha(paths$audit)))
      stop("Cached W03 report or input changed; refusing to overwrite.")
    message("[W03] Reusing verified completed scan.")
    return(invisible(dest))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Partial W03 output exists. Inspect it before re-execution.")
  dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  script<-system.file("w03","workflow_03_publication_status.R",
                      package="evidencelandscaper")
  if(!nzchar(script))stop("Upstream W03 script missing.")
  args<-c(script,"--input",canonical,"--output",paths$output,
          "--audit",paths$audit,"--report",paths$report,
          "--limit",as.character(limit),"--batch-size",as.character(batch_size))
  logs<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
              args=vapply(args,shQuote,character(1)),stdout=TRUE,stderr=TRUE))
  status<-attr(logs,"status")
  if(!is.null(status)&&status!=0L)
    stop("W03 upstream script failed:\n",paste(tail(logs,35L),collapse="\n"))
  if(!all(file.exists(unlist(paths[c("output","audit","report")]))))
    stop("W03 output files missing.")
  rep<-jsonlite::fromJSON(paths$report)
  if(!identical(rep$status,"PASS") ||
     !identical(rep$input_sha256,sha(canonical)) ||
     !identical(rep$output_sha256,sha(paths$output)))
    stop("W03 provider/report validation failed.")
  rows<-readLines(paths$output,warn=FALSE,encoding="UTF-8")
  rows<-rows[nzchar(trimws(rows))]
  expected_count<-if(limit==0L) as.integer(w02_summary$publication_clusters) else
    min(limit,as.integer(w02_summary$publication_clusters))
  if(length(rows)!=expected_count ||
     !identical(as.integer(rep$records_scanned),expected_count))
    stop("W03 output count does not reconcile with W02.")
  jsonlite::write_json(list(schema="local-w03-handover-v1",
    status="final",full_corpus=(limit==0L),
    publication_clusters=length(rows),
    w02_canonical_sha256=sha(canonical),status_sha256=sha(paths$output),
    report_sha256=sha(paths$report),audit_sha256=sha(paths$audit)),
    paths$manifest,auto_unbox=TRUE,pretty=TRUE)
  message("[W03] PASS: ",length(rows)," publication statuses; ",
          rep$exclude_from_workflow04," excluded from W04.")
  invisible(dest)
}
