#' Export a portable Shiny adjudication bundle for unresolved W04 records
#'
#' Uses the identical W04 human-review queue and decision schema expected by
#' import_w04_review(). Only unresolved consensus cases are packaged; model
#' agreement records are not re-reviewed. Source outputs remain untouched.
#'
#' @param w04 Exact completed W04 run directory.
#' @param destination_zip A new ZIP path that does not already exist.
#' @return ZIP file path, invisibly.
#' @export
export_w04_adjudication_bundle <- function(w04,destination_zip) {
  w04<-normalizePath(w04,winslash="/",mustWork=TRUE)
  summary_file<-file.path(w04,"consensus","summary.json")
  layer<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  if(!all(file.exists(c(summary_file,layer))))
    stop("Completed W04 consensus summary and layer are required.")
  summary<-jsonlite::fromJSON(summary_file,simplifyVector=FALSE)
  if(!identical(summary$status,"HUMAN_REVIEW_REQUIRED"))
    stop("There are no unresolved W04 consensus decisions to adjudicate.")
  if(length(destination_zip)!=1L||is.na(destination_zip)||
     !grepl("\\.zip$",destination_zip,ignore.case=TRUE))
    stop("destination_zip must be a single ZIP pathname.")
  destination_zip<-file.path(normalizePath(dirname(destination_zip),
     winslash="/",mustWork=TRUE),basename(destination_zip))
  if(file.exists(destination_zip))
    stop("Adjudication ZIP already exists; refusing to overwrite.")
  queue<-prepare_w04_review(w04,include_all=FALSE)
  if(is.null(queue)||!file.exists(queue))
    stop("No unresolved W04 cases to export.")
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  readjl<-function(path){
    lines<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,
           simplifyVector=FALSE)
  }
  cases<-readjl(queue)
  ids<-vapply(cases,function(z)as.character(z$record_id),character(1))
  if(!length(ids)||anyDuplicated(ids)||
     !identical(length(ids),as.integer(summary$final_unresolved))||
     any(!vapply(cases,function(z)
       identical(z$schema,"local-w04-review-case-v1") &&
       identical(z$model$decision,"uncertain"),logical(1))))
    stop("Adjudication queue does not match W04 unresolved consensus cases.")
  original_app<-system.file("w04","review_app.R",package="evidencelandscaper")
  if(!nzchar(original_app))stop("Installed W04 native Shiny reviewer missing.")
  source<-paste(readLines(original_app,warn=FALSE,encoding="UTF-8"),
                collapse="\n")
  before_args<-"args<-commandArgs(trailingOnly=TRUE)"
  after_args<-"args<-c(file.path(getwd(),\"data\",\"human_review_queue.jsonl\"),getwd())"
  before_app<-"shiny::runApp(shinyApp(ui,server),launch.browser=TRUE)"
  after_app<-"shinyApp(ui,server)"
  if(!grepl(before_args,source,fixed=TRUE)||
     !grepl(before_app,source,fixed=TRUE)||
     length(gregexpr(before_args,source,fixed=TRUE)[[1]])!=1L||
     length(gregexpr(before_app,source,fixed=TRUE)[[1]])!=1L)
    stop("Native W04 app changed; cannot safely adapt portable version.")
  source<-sub(before_args,after_args,source,fixed=TRUE)
  source<-sub(before_app,after_app,source,fixed=TRUE)
  work<-tempfile("w04-adjudication-")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  bundle<-file.path(work,"w04_adjudication_bundle")
  dir.create(file.path(bundle,"data"),recursive=TRUE)
  staged_queue<-file.path(bundle,"data","human_review_queue.jsonl")
  if(!file.copy(queue,staged_queue))
    stop("Could not stage immutable W04 adjudication queue.")
  if(!identical(sha(queue),sha(staged_queue)))
    stop("Staged adjudication queue checksum mismatch.")
  appfile<-file.path(bundle,"app.R")
  writeLines(source,appfile,useBytes=TRUE)
  tryCatch(parse(file=appfile),error=function(e)
    stop("Adapted portable W04 Shiny app cannot be parsed: ",conditionMessage(e)))
  manifest<-list(schema="ostara-w04-adjudication-bundle-v1",
    source_w04=basename(w04),
    source_consensus_sha256=sha(layer),
    source_queue_sha256=sha(queue),
    prompt_sha256=summary$prompt_sha256,
    model=summary$model,
    unresolved_records=length(cases),
    decision_schema="local-w04-review-decision-v1")
  jsonlite::write_json(manifest,file.path(bundle,"manifest.json"),
                       auto_unbox=TRUE,pretty=TRUE)
  writeLines(c(
    "OSTARA W04 | PORTABLE UNRESOLVED-SCREENING ADJUDICATION",
    "",
    "1. Extract this ZIP into a writable directory.",
    "2. Install R dependencies once: install.packages(c('shiny','jsonlite','digest'))",
    "3. Open w04_adjudication_bundle/app.R in RStudio and click Run App;",
    "   or run shiny::runApp('/path/to/w04_adjudication_bundle').",
    "4. Enter your name, read each abstract and the model consensus.",
    "5. Decide retain or exclude, or mark uncertain if truly unresolved.",
    "   Add a free-text reason and save each decision.",
    "6. When every record is reviewed, download w04_human_decisions.jsonl.",
    "   Send this exact JSONL file to the project lead for import.",
    "",
    "This is W04 adjudication, not all-record independent validation.",
    "The review is saved locally as decisions.jsonl in the extracted folder.",
    "The source screening run is not changed by the app.",
    "Do not edit data/human_review_queue.jsonl or manifest.json.",
    "Use separate extracted folders for different reviewers."
  ),file.path(bundle,"README.txt"),useBytes=TRUE)
  oldwd<-getwd()
  on.exit(setwd(oldwd),add=TRUE)
  setwd(work)
  utils::zip(destination_zip,c(
    "w04_adjudication_bundle/app.R",
    "w04_adjudication_bundle/README.txt",
    "w04_adjudication_bundle/manifest.json",
    "w04_adjudication_bundle/data/human_review_queue.jsonl"),flags="-q")
  if(!file.exists(destination_zip)||file.info(destination_zip)$size<=0)
    stop("Could not create W04 adjudication ZIP.")
  message("[W04] Exported ",length(cases),
          " unresolved records for portable adjudication: ",destination_zip)
  invisible(destination_zip)
}
