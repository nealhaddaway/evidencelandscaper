#' Finalise the completed W02 Europe PMC/Scopus enrichment
#'
#' Reuses original upstream W02 patch construction and application. No
#' new provider queries are performed. ScienceDirect suggestions remain
#' a separate, unaccepted review layer.
#' @param scored W01 full-scoring directory.
#' @return Directory containing validated final W02 files.
#' @export
finalize_w02 <- function(scored) {
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  main<-file.path(dirname(scored),"W02_main")
  input<-file.path(main,"canonical_input.jsonl")
  enriched<-file.path(main,"enriched.jsonl")
  audit<-file.path(main,"audit.jsonl")
  enrichment_report<-file.path(main,"report.json")
  source_files<-c(input,enriched,audit,enrichment_report)
  if(!all(file.exists(source_files))) stop("Completed W02-main enrichment is required.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  rep<-jsonlite::fromJSON(enrichment_report)
  if(!identical(rep$input_sha256,sha(input)) ||
     !identical(rep$output_sha256,sha(enriched)) ||
     !identical(rep$audit_sha256,sha(audit)))
    stop("W02-main input or output checksums do not match.")
  out<-file.path(main,"finalised")
  paths<-list(
    patch=file.path(out,"patch.jsonl"),
    retry=file.path(out,"retry_queue.jsonl"),
    patch_report=file.path(out,"patch_report.json"),
    canonical=file.path(out,"canonical.jsonl"),
    application_report=file.path(out,"application_report.json"),
    summary=file.path(out,"summary.json"))
  if(file.exists(paths$summary)) {
    old<-jsonlite::fromJSON(paths$summary)
    expected<-old$checksums
    files<-c(source_files,unlist(paths[1:5],use.names=FALSE))
    if(length(expected)!=length(files) ||
       !identical(unname(unlist(expected)),unname(vapply(files,sha,character(1)))))
      stop("Existing W02 finalisation changed; refusing to overwrite.")
    message("[W02] Reusing verified finalised corpus.")
    return(invisible(out))
  }
  if(dir.exists(out)&&length(list.files(out,all.files=TRUE,no..=TRUE)))
    stop("Partial W02 finalisation exists. Inspect it before restarting.")
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  run_original<-function(name,args) {
    script<-system.file("w02",name,package="evidencelandscaper")
    if(!nzchar(script)) stop("Missing original W02 script: ",name)
    result<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
       args=vapply(c(script,args),shQuote,character(1)),
       stdout=TRUE,stderr=TRUE))
    status<-attr(result,"status")
    if(!is.null(status)&&status!=0L)
      stop("W02 original script failed:\n",paste(tail(result,35L),collapse="\n"))
  }
  run_original("workflow_02_build_patch.R",c(
    "--input",input,"--enriched",enriched,"--audit",audit,
    "--enrichment-report",enrichment_report,"--patch",paths$patch,
    "--retry-queue",paths$retry,"--report",paths$patch_report))
  patch_meta<-jsonlite::fromJSON(paths$patch_report)
  if(!identical(patch_meta$status,"PASS") ||
     !identical(patch_meta$input_sha256,sha(input)) ||
     !identical(patch_meta$enriched_sha256,sha(enriched)) ||
     !identical(patch_meta$patch_sha256,sha(paths$patch)))
    stop("W02 patch build failed verification.")
  run_original("workflow_02_apply_patch.R",c(
    "--input",input,"--patch",paths$patch,"--output",paths$canonical,
    "--report",paths$application_report,"--mode","exact"))
  applied<-jsonlite::fromJSON(paths$application_report,simplifyVector=FALSE)
  if(!identical(applied$status,"PASS") ||
     !identical(applied$input_sha256,sha(input)) ||
     !identical(applied$patch_sha256,sha(paths$patch)) ||
     !identical(applied$output_sha256,sha(paths$canonical)))
    stop("W02 patch application reported unresolved conflicts or failed checksums.")
  read_rows<-function(file) {
    z<-readLines(file,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  rows<-read_rows(paths$canonical)
  initial<-read_rows(input)
  if(length(rows)!=length(initial))stop("W02 publication count changed.")
  rid<-function(x)x$identity$record_id
  if(!identical(lapply(rows,rid),lapply(initial,rid)))
    stop("W02 changed publication identities.")
  text_present<-function(x) {
    if(is.null(x)||!length(x))return(FALSE)
    any(nzchar(trimws(as.character(unlist(x,use.names=FALSE)))))
  }
  has_abs<-function(x)text_present(x$canonical$abstract)
  before<-sum(vapply(initial,has_abs,logical(1)))
  after<-sum(vapply(rows,has_abs,logical(1)))
  if(after<before)stop("W02 abstract coverage decreased.")
  audit_rows<-read_rows(audit)
  quarantined<-unlist(lapply(audit_rows,function(x)x$quarantined),
                      recursive=FALSE)
  files<-c(source_files,unlist(paths[1:5],use.names=FALSE))
  jsonlite::write_json(list(schema="local-w02-finalisation-v1",
    status="final",publication_clusters=length(rows),
    abstracts_before=before,abstracts_after=after,
    remaining_without_abstract=length(rows)-after,
    quarantined_provider_decisions=length(quarantined),
    sciencedirect_suggestions_not_applied=TRUE,
    checksums=unname(as.list(vapply(files,sha,character(1))))),
    paths$summary,auto_unbox=TRUE,pretty=TRUE)
  message("[W02] Finalised ",length(rows)," publications; ",
    after," with abstracts; ",length(rows)-after," still missing. ",
    length(quarantined)," provider decisions quarantined.")
  invisible(out)
}
