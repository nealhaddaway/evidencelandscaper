#' Export a portable Shiny ZIP for full-text-only OSTARA case review
#'
#' Uses the independently verified W08-versus-full-text queue. The ZIP is
#' self-contained and runs offline with shiny/jsonlite/digest; no API key or
#' local OSTARA folder is needed on the colleague's computer.
#' @param destination_zip New ZIP path.
#' @param map_path OSTARA project; NULL uses the default named map.
#' @param mechanism_runs Optional named explicit first10/remaining90 mechanism
#'   inventory directories, to resolve multiple fingerprinted runs.
#' @return ZIP path, invisibly.
#' @export
export_ostara_fulltext_case_review <- function(
    destination_zip,map_path=NULL,mechanism_runs=NULL) {
  if(!is.character(destination_zip)||length(destination_zip)!=1L||
     is.na(destination_zip)||!grepl("\\.zip$",destination_zip,ignore.case=TRUE))
    stop("destination_zip must be one .zip path.")
  destination_zip<-file.path(normalizePath(dirname(destination_zip),
    winslash="/",mustWork=TRUE),basename(destination_zip))
  if(file.exists(destination_zip))stop("Review ZIP exists; refusing overwrite.")
  prepared<-review_ostara_fulltext_case_discrepancies(
    map_path=map_path,mechanism_runs=mechanism_runs,launch=FALSE)
  queue_file<-prepared$queue_file
  app<-system.file("ostara","case_discrepancy_app.R",
                   package="evidencelandscaper")
  if(!nzchar(app)||!file.exists(app))stop("Packaged Shiny reviewer missing.")
  work<-tempfile("ostara-case-review-")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  root<-file.path(work,"ostara_case_review_bundle")
  dir.create(file.path(root,"data"),recursive=TRUE)
  if(!file.copy(queue_file,file.path(root,"data","review_queue.jsonl"))||
     !file.copy(app,file.path(root,"app.R")))
    stop("Failed to stage portable case-review files.")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  entries<-readLines(file.path(root,"data","review_queue.jsonl"),warn=FALSE)
  manifest<-list(schema="ostara-ft-case-review-bundle-v1",
    queue_sha256=sha(file.path(root,"data","review_queue.jsonl")),
    records=prepared$records,
    case_selection="W08 not case positive, full-text case_present",
    adjudication_format="ostara-ft-case-discrepancy-adjudications-v1",
    model_results_modified=FALSE)
  jsonlite::write_json(manifest,file.path(root,"manifest.json"),
                       pretty=TRUE,auto_unbox=TRUE)
  writeLines(c(
    "OSTARA | HUMAN REVIEW OF FULL-TEXT-ONLY COLLECTIVE-ACTION CASES",
    "",
    "1. Unzip the archive into a writable folder.",
    "2. In RStudio, install once: install.packages(c('shiny','jsonlite','digest'))",
    "3. Open ostara_case_review_bundle/app.R and click Run App.",
    "   Or run shiny::runApp('/path/to/ostara_case_review_bundle').",
    "4. Enter your reviewer name. Review each publication and candidate-case evidence.",
    "5. Select Accept, Reject or Unclear and click Save decision.",
    "6. Select 'Return adjudication JSON' and send that JSON file back.",
    "",
    "No OSTARA project folder, credentials, GitHub installation or internet",
    "connection is required for review. The model's inputs are not changed.",
    "The app stores your local progress inside data/reviewer_state.",
    "Do not alter data/review_queue.jsonl or manifest.json."
  ),file.path(root,"README.txt"),useBytes=TRUE)
  old<-getwd()
  on.exit(setwd(old),add=TRUE)
  setwd(work)
  files<-c("ostara_case_review_bundle/app.R",
           "ostara_case_review_bundle/README.txt",
           "ostara_case_review_bundle/manifest.json",
           "ostara_case_review_bundle/data/review_queue.jsonl")
  utils::zip(zipfile=destination_zip,files=files,flags="-q")
  if(!file.exists(destination_zip)||is.na(file.info(destination_zip)$size)||
     file.info(destination_zip)$size<=0L)stop("Could not create ZIP.")
  message("[OSTARA case review] Portable ZIP (",prepared$records,
          " candidate publications): ",destination_zip)
  invisible(destination_zip)
}

#' Import a colleague's JSON adjudications for full-text-only cases
#'
#' Verifies the JSON's queue SHA against the exact local exported review queue.
#' Preserves previous imports, writes validated rows to a separate directory,
#' and creates approved_w09_case_ids.csv for a later W09 extraction. No model
#' results, abstract adjudications or case extraction ledgers are modified.
#'
#' @param adjudications_json JSON exported with Return adjudication JSON.
#' @param map_path OSTARA map; NULL uses the default named map.
#' @return Directory containing the validated import and accepted IDs.
#' @export
import_ostara_fulltext_case_adjudications <- function(
    adjudications_json,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  adjudications_json<-normalizePath(adjudications_json,
                                     winslash="/",mustWork=TRUE)
  root<-file.path(map_path,"runs","W09",
                  "fulltext_case_discrepancy_human_review")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  x<-jsonlite::fromJSON(adjudications_json,simplifyVector=FALSE)
  if(!identical(x$schema,"ostara-ft-case-discrepancy-adjudications-v1")||
     !is.character(x$queue_sha256)||length(x$queue_sha256)!=1L||
     !is.list(x$records))
    stop("Not an OSTARA full-text case adjudication JSON.")
  queues<-list.files(root,pattern="^review_queue-.*\\.jsonl$",
                     full.names=TRUE)
  matches<-queues[vapply(queues,function(p)identical(sha(p),x$queue_sha256),
                         logical(1))]
  if(length(matches)!=1L)
    stop("Could not verify exactly one original review queue for this JSON.")
  qlines<-readLines(matches[[1L]],warn=FALSE,encoding="UTF-8")
  queue<-lapply(qlines[nzchar(trimws(qlines))],
                jsonlite::fromJSON,simplifyVector=FALSE)
  key<-function(z)paste(z$cohort,z$record_id,sep="|")
  known<-setNames(queue,vapply(queue,key,character(1)))
  if(anyDuplicated(names(known)))stop("Review queue has duplicate identities.")
  rows<-list()
  for(e in x$records) {
    if(!is.list(e)||!all(c("record_id","cohort",
       "mechanism_manifest_sha256","decision","notes",
       "reviewer","timestamp_utc")%in%names(e)))
      stop("Incomplete human adjudication entry.")
    id<-key(e)
    q<-known[[id]]
    if(is.null(q))stop("Unknown review publication ",id)
    if(!identical(e$mechanism_manifest_sha256,q$mechanism_manifest_sha256))
      stop("Full-text provenance mismatch for ",id)
    if(!is.character(e$decision)||length(e$decision)!=1L||
       !e$decision%in%c("accept","reject","unclear"))
      stop("Invalid human decision: ",id)
    if(!is.character(e$reviewer)||length(e$reviewer)!=1L||
       !nzchar(trimws(e$reviewer)))
      stop("Missing reviewer: ",id)
    rows[[length(rows)+1L]]<-data.frame(
      record_id=e$record_id,cohort=e$cohort,
      decision=e$decision,reviewer=e$reviewer,
      notes=as.character(e$notes),timestamp_utc=e$timestamp_utc,
      mechanism_manifest_sha256=e$mechanism_manifest_sha256,
      source_w09=q$source_w09,
      stringsAsFactors=FALSE)
  }
  decisions<-if(length(rows))do.call(rbind,rows) else data.frame(
    record_id=character(),cohort=character(),decision=character(),
    reviewer=character(),notes=character(),timestamp_utc=character(),
    mechanism_manifest_sha256=character(),source_w09=character())
  if(anyDuplicated(paste(decisions$cohort,decisions$record_id)))
    stop("Duplicated human adjudication entries.")
  out<-file.path(root,"imports",paste0("import-",
     substr(sha(adjudications_json),1L,16L)))
  dir.create(out,recursive=TRUE,showWarnings=FALSE)
  copied<-file.path(out,"adjudications.json")
  if(file.exists(copied)&&!identical(sha(copied),sha(adjudications_json)))
    stop("Prior import directory contains a different JSON.")
  if(!file.exists(copied)&&!file.copy(adjudications_json,copied))
    stop("Could not archive original adjudication JSON.")
  utils::write.csv(decisions,file.path(out,"all_adjudications.csv"),
                   row.names=FALSE,na="")
  accepted<-decisions[decisions$decision=="accept",,drop=FALSE]
  utils::write.csv(accepted,
       file.path(out,"approved_w09_case_ids.csv"),row.names=FALSE,na="")
  jsonlite::write_json(list(schema="ostara-ft-case-adjudication-import-v1",
    source_json_sha256=sha(adjudications_json),
    queue_sha256=x$queue_sha256,
    imported_records=nrow(decisions),
    accepted_records=nrow(accepted),
    review_queue=matches[[1L]]),
    file.path(out,"import_manifest.json"),pretty=TRUE,auto_unbox=TRUE)
  message("[OSTARA case review] Imported ",nrow(decisions),
          " decisions; ",nrow(accepted)," accepted for W09 extraction.")
  message("[OSTARA case review] Approved records: ",
          file.path(out,"approved_w09_case_ids.csv"))
  invisible(out)
}
