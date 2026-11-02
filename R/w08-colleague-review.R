#' Export portable W08 case detection review for a colleague
#'
#' Includes the complete W08 model assessments, their originating abstracts,
#' and the exact prompt, without modifying W04, W07 or W08 results.
#' Human decisions are recorded separately by the offline Shiny application.
#' @param w08 Directory returned by trial_w08_cases().
#' @param destination_zip A new ZIP path, not an existing file.
#' @param allow_partial Explicitly permit PILOT_PARTIAL runs. Only validated
#'   responses enter the Shiny queue; missing IDs are reported separately.
#' @return ZIP path, invisibly.
#' @export
export_w08_colleague_review <- function(w08, destination_zip, allow_partial=FALSE) {
  if(!is.logical(allow_partial)||length(allow_partial)!=1L||is.na(allow_partial))
    stop('allow_partial must be TRUE or FALSE.')
  w08 <- normalizePath(w08,winslash="/",mustWork=TRUE)
  required <- file.path(w08,c("manifest.json","summary.json",
                              "case_assessments.jsonl","prompt.txt","fields.csv"))
  if(!all(file.exists(required)))
    stop("W08 assessment, manifest, prompt, fields and summary are required.")
  sha <- function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  manifest <- jsonlite::fromJSON(file.path(w08,"manifest.json"),simplifyVector=FALSE)
  summary <- jsonlite::fromJSON(file.path(w08,"summary.json"),simplifyVector=FALSE)
  partial<-identical(summary$status,"PILOT_PARTIAL")
  if(!identical(summary$status,"PILOT_COMPLETE")&&
     !(partial&&allow_partial))
    stop("W08 is incomplete; to export validated responses only, set allow_partial=TRUE.")
  if(!identical(sha(file.path(w08,"case_assessments.jsonl")),
                summary$responses_sha256)||
     !identical(sha(file.path(w08,"prompt.txt")),manifest$prompt_sha256)||
     !identical(sha(file.path(w08,"fields.csv")),manifest$schema_sha256))
    stop("W08 archived data checksum mismatch.")
  if(length(destination_zip)!=1L||is.na(destination_zip)||
     !grepl("\\.zip$",destination_zip,ignore.case=TRUE))
    stop("destination_zip must be a single ZIP path.")
  destination_zip<-file.path(normalizePath(dirname(destination_zip),
                       winslash="/",mustWork=TRUE),basename(destination_zip))
  if(file.exists(destination_zip))stop("ZIP already exists; refusing overwrite.")
  readjl<-function(p) {
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  assessments<-readjl(file.path(w08,"case_assessments.jsonl"))
  ids<-vapply(assessments,function(x)as.character(x$record_id),character(1))
  chosen<-unlist(manifest$selected_record_ids,use.names=FALSE)
  missing<-setdiff(chosen,ids)
  if(!length(ids)||anyDuplicated(ids)||anyDuplicated(chosen)||
     any(!ids%in%chosen)||
     (length(ids)!=length(chosen)-length(missing))||
     (!partial&&length(missing)))
    stop("W08 validated records do not match the exact selection.")
  if(partial) {
    unresolved<-unlist(summary$unresolved_record_ids,use.names=FALSE)
    if(length(missing)<1L||!setequal(missing,unresolved)||
       !identical(as.integer(summary$validated_records),as.integer(length(ids)))||
       !identical(length(chosen),as.integer(manifest$sample_size)))
      stop("W08 partial output and unresolved IDs mismatch; refusing export.")
  }
  w04<-normalizePath(manifest$source_w04,mustWork=TRUE)
  provisional<-isTRUE(manifest$provisional_retains)
  layer_dir<-if(provisional)"consensus" else "finalised"
  source_layer<-file.path(w04,layer_dir,
    if(provisional)"workflow04_consensus_layer.jsonl"
    else "workflow04_final_screening_layer.jsonl")
  if(!file.exists(source_layer)||
     !identical(sha(source_layer),manifest$source_w04_sha256))
    stop("Source W04 screening layer changed or is missing.")
  if(provisional&&!identical(manifest$source_w04_layer,"consensus"))
    stop("Provisional W08 manifest has incorrect W04 layer provenance.")
  retained<-Filter(function(z)identical(z$screening$decision,"retain"),
                   readjl(source_layer))
  retained_ids<-vapply(retained,function(z)as.character(z$record_id),character(1))
  if(any(!chosen%in%retained_ids))stop("W08 selected non-retained W04 publications.")
  canonical<-file.path(dirname(dirname(w04)),"W02_main","finalised","canonical.jsonl")
  if(!file.exists(canonical)||!identical(sha(canonical),manifest$canonical_w02_sha256))
    stop("Canonical W02 source does not match W08 provenance.")
  original<-readjl(canonical)
  original_ids<-vapply(original,function(z)as.character(z$identity$record_id),character(1))
  if(anyDuplicated(original_ids)||any(!chosen%in%original_ids))
    stop("Could not match W08 publication IDs uniquely to source abstracts.")
  source<-setNames(original,original_ids)
  w04_summary<-jsonlite::fromJSON(file.path(w04,layer_dir,"summary.json"),
                                   simplifyVector=FALSE)
  corrections<-list()
  if(!provisional&&isTRUE(w04_summary$reconciled_abstracts)) {
    dirs<-list.dirs(file.path(w04,"abstract_rescreens"),
                    recursive=FALSE,full.names=TRUE)
    dirs<-dirs[file.exists(file.path(dirs,"manifest.json"))]
    if(length(dirs)!=1L)stop("Cannot identify the corrected W04 abstracts.")
    csv<-file.path(dirs[[1L]],"inputs","submitted_abstracts.csv")
    if(!file.exists(csv))stop("Missing corrected W04 abstracts.")
    corrected<-utils::read.csv(csv,stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
    if(anyDuplicated(corrected$record_id))
      stop("Duplicate corrected W04 abstract IDs.")
    corrections<-setNames(as.list(corrected$abstract),corrected$record_id)
  }
  scalar<-function(z)if(is.null(z)||!length(z))"" else
    paste(as.character(unlist(z,use.names=FALSE)),collapse="; ")
  rows<-lapply(assessments,function(x) {
    id<-as.character(x$record_id)
    z<-source[[id]]$canonical
    list(record_id=id,
      bibliographic=list(
        title=scalar(z$title),
        authors=scalar(if(!is.null(z$authors))z$authors else z$author),
        year=scalar(z$year),
        journal=scalar(if(!is.null(z$source_title))z$source_title else z$journal),
        doi=scalar(z$doi),
        keywords=scalar(if(!is.null(z$author_keywords))
                          z$author_keywords else z$keywords),
        abstract=scalar(if(!is.null(corrections[[id]]))
                          corrections[[id]] else z$abstract)),
      detection=x)
  })
  app<-system.file("w08","review_bundle_app.R",package="evidencelandscaper")
  if(!nzchar(app))stop("Installed W08 Shiny reviewer is missing.")
  work<-tempfile("w08-review-")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  root<-file.path(work,"w08_review_bundle")
  dir.create(file.path(root,"data"),recursive=TRUE)
  queue<-file.path(root,"data","review_queue.jsonl")
  writeLines(vapply(rows,function(z)as.character(jsonlite::toJSON(
    z,auto_unbox=TRUE,null="null")),character(1)),queue,useBytes=TRUE)
  if(partial) {
    # Report missing publication separately, without manufacturing a model
    # assessment or changing the immutable 249-record validation queue.
    excluded<-data.frame(record_id=missing,
      title=vapply(missing,function(id)scalar(source[[id]]$canonical$title),
                   character(1)),
      abstract=vapply(missing,function(id)scalar(source[[id]]$canonical$abstract),
                      character(1)),stringsAsFactors=FALSE)
    utils::write.csv(excluded,file.path(root,"data","unresolved_publications.csv"),
                     row.names=FALSE,na="")
  }
  if(!file.copy(app,file.path(root,"app.R"))||
     !file.copy(file.path(w08,"prompt.txt"),file.path(root,"data","prompt.txt")))
    stop("Could not stage the portable review app and prompt.")
  metadata<-list(schema="ostara-w08-colleague-review-v1",
    source_w08=basename(w08),source_w04=basename(w04),
    source_w04_sha256=manifest$source_w04_sha256,
    provisional_retains=provisional,
    source_w08_sha256=sha(file.path(w08,"case_assessments.jsonl")),
    prompt_sha256=sha(file.path(root,"data","prompt.txt")),
    queue_sha256=sha(queue),records=length(rows),
    partial_run=partial,selected_records=length(chosen),
    unresolved_record_ids=as.list(missing),
    candidate_cases=sum(vapply(assessments,function(x)length(x$cases),integer(1))))
  jsonlite::write_json(metadata,file.path(root,"manifest.json"),
                       pretty=TRUE,auto_unbox=TRUE)
  writeLines(c(
    "OSTARA W08 | COLLEAGUE REVIEW OF ABSTRACT-LEVEL CASE DETECTION",
    "",
    "1. Extract the ZIP to a writable directory.",
    "2. Install R packages once: install.packages(c('shiny','jsonlite','digest'))",
    "3. Open w08_review_bundle/app.R and click Run App.",
    "   Alternatively run shiny::runApp('/path/to/w08_review_bundle').",
    "4. Enter reviewer name, examine each abstract and its detected case candidates.",
    "5. Accept/reject the publication-level assessment and optionally accept/reject",
    "   each candidate individually. Record corrections and alternate cases in text boxes.",
    "6. Save before navigating. Saved judgments append to decisions.jsonl.",
    "7. Export complete CSV and JSONL audit history and return both.",
    "",
    if(partial)paste0("IMPORTANT: PARTIAL RUN. Shiny contains ",length(ids),
      " validated publications of ",length(chosen),
      ". Missing ID(s): ",paste(missing,collapse=", "),
      ". See data/unresolved_publications.csv. No model assessment was fabricated.")
    else "COMPLETE RUN: All selected publications have validated assessments.",
    "A rejection does not edit the model's assessment.",
    "The review ZIP is offline and requires no API key, map folder or GitHub access.",
    "For multiple reviewers, give each a separate extracted ZIP."
  ),file.path(root,"README.txt"),useBytes=TRUE)
  oldwd<-getwd()
  on.exit(setwd(oldwd),add=TRUE)
  setwd(work)
  utils::zip(destination_zip,
    c("w08_review_bundle/app.R","w08_review_bundle/README.txt",
      "w08_review_bundle/manifest.json",
      "w08_review_bundle/data/prompt.txt",
      "w08_review_bundle/data/review_queue.jsonl",
      if(partial)"w08_review_bundle/data/unresolved_publications.csv"),flags="-q")
  if(!file.exists(destination_zip)||file.info(destination_zip)$size<=0)
    stop("W08 review ZIP not created.")
  message("[W08] Exported ",length(rows),
          if(partial)paste0(" of ",length(chosen)," (partial)") else "",
          " publications and ",
          metadata$candidate_cases," candidate cases: ",destination_zip)
  invisible(destination_zip)
}
