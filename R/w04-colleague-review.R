#' Export a portable, complete W04 screening validation bundle
#'
#' Packages ALL records from a completed W04 model-consensus run, including
#' retained, excluded and uncertain classifications, for independent human
#' validation. Bundles Shiny application source and a checksum-pinned queue;
#' no live map, API credentials or GitHub checkout is required by the reviewer.
#' Human judgments remain separate from model and finalised W04 outputs.
#'
#' @param w04 Exact completed W04 run directory returned by run_w04().
#' @param destination_zip New .zip path to create (must not exist).
#' @return Absolute path to the portable ZIP, invisibly.
#' @export
export_w04_colleague_review <- function(w04,destination_zip) {
  w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
  consensus<-file.path(w04,"consensus","summary.json")
  if(!file.exists(consensus))stop("Missing completed W04 model consensus")
  summary<-jsonlite::fromJSON(consensus,simplifyVector=FALSE)
  if(!summary$status%in%c("PASS","HUMAN_REVIEW_REQUIRED"))
    stop("W04 consensus is not complete")
  if(is.null(destination_zip)||length(destination_zip)!=1L||
     !grepl("\\.zip$",destination_zip,ignore.case=TRUE))
    stop("destination_zip must be a single .zip filename")
  destination_zip<-file.path(normalizePath(dirname(destination_zip),
                                  winslash="/",mustWork=TRUE),
                             basename(destination_zip))
  if(file.exists(destination_zip))stop("Bundle ZIP exists; refusing to overwrite")
  queue<-prepare_w04_review(w04,include_all=TRUE)
  if(is.null(queue)||!file.exists(queue))
    stop("W04 full-validation queue unavailable")
  readjl<-function(path) {
    rows<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(rows[nzchar(trimws(rows))],
           jsonlite::fromJSON,simplifyVector=FALSE)
  }
  cases<-readjl(queue)
  ids<-vapply(cases,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(ids)||length(ids)!=as.integer(summary$records_screened))
    stop("Validation queue does not cover the complete W04 screening")
  if(any(!vapply(cases,function(z)
     z$model$decision%in%c("retain","exclude","uncertain"),logical(1))))
    stop("Validation queue includes unknown model decisions")
  app<-system.file("w04","validation_bundle_app.R",
                   package="evidencelandscaper")
  if(!nzchar(app))stop("Portable W04 Shiny app not installed")
  work<-tempfile("w04-colleague-")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  bundle<-file.path(work,"w04_review_bundle")
  dir.create(file.path(bundle,"data"),recursive=TRUE)
  file.copy(queue,file.path(bundle,"data","review_queue.jsonl"))
  file.copy(app,file.path(bundle,"app.R"))
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  manifest<-list(schema="ostara-w04-colleague-review-v1",
   source_w04=basename(w04),
   model=summary$model,
   prompt_sha256=summary$prompt_sha256,
   model_counts=list(retain=summary$final_retain,
                     exclude=summary$final_exclude,
                     uncertain=summary$final_unresolved),
   records=length(cases),
   review_queue_sha256=sha(file.path(bundle,"data","review_queue.jsonl")),
   source_consensus_sha256=sha(file.path(w04,"consensus",
                                           "workflow04_consensus_layer.jsonl")))
  jsonlite::write_json(manifest,file.path(bundle,"manifest.json"),
                       auto_unbox=TRUE,pretty=TRUE)
  writeLines(c(
    "OSTARA W04: INDEPENDENT ABSTRACT SCREENING VALIDATION",
    "",
    "This package contains ALL model-screened abstracts: retained, excluded",
    "and uncertain. No original evidence-map files or API credentials are",
    "needed; review work is local to this unzipped directory.",
    "",
    "1. Unzip the archive to a writable local folder.",
    "2. Install R packages once: install.packages(c('shiny','jsonlite','digest'))",
    "3. From RStudio: shiny::runApp('/path/to/w04_review_bundle')",
    "   Alternatively open app.R in RStudio and click Run App.",
    "4. Enter your name. Filter by model decision, review status or search.",
    "5. Review titles/abstracts, save an independent decision and optional",
    "   justification. All changes append to decisions.jsonl (audit trail).",
    "6. Download the complete CSV, including records not yet reviewed, and",
    "   the raw JSONL event history. Return both files to the project lead.",
    "",
    "IMPORTANT: This is independent validation, not W04 adjudication import.",
    "Nothing here edits the original W04 model consensus, full-text data",
    "or canonical evidence map. Do not merge decisions automatically.",
    "Only one reviewer should edit this same bundle folder at a time.",
    "For multiple reviewers, each gets a separate extracted copy.",
    "The abstract and model decision queue is checksum-verified at startup.",
    "Do not edit data/review_queue.jsonl or manifest.json."
  ),file.path(bundle,"README.txt"),useBytes=TRUE)
  # utils::zip uses a standard ZIP utility available on macOS, Windows and
  # typical Linux R environments. Build a relative-path portable archive.
  cwd<-getwd()
  on.exit(setwd(cwd),add=TRUE)
  setwd(work)
  utils::zip(zipfile=destination_zip,
             files=c("w04_review_bundle/app.R",
                     "w04_review_bundle/README.txt",
                     "w04_review_bundle/manifest.json",
                     "w04_review_bundle/data/review_queue.jsonl"),
             flags="-q")
  if(!file.exists(destination_zip)||file.info(destination_zip)$size<=0)
    stop("Could not create review ZIP")
  message("Portable review bundle created: ",destination_zip,
          " (",length(cases)," records).")
  invisible(destination_zip)
}
