#' Export an offline Shiny review bundle for W07 mechanism coding
#'
#' Packages all completed W07 publication-level mechanism assignments with
#' abstracts, ontology definitions and model evidence. Reviewer accept/reject
#' decisions and suggested alternatives are saved independently from W07.
#'
#' @param w07 Completed trial_w07() directory (contains coding.jsonl).
#' @param destination_zip New ZIP path; never overwrites an existing archive.
#' @return ZIP path, invisibly.
#' @export
export_w07_colleague_review <- function(w07,destination_zip) {
  w07<-normalizePath(w07,winslash="/",mustWork=TRUE)
  coding_path<-file.path(w07,"coding.jsonl")
  manifest_path<-file.path(w07,"manifest.json")
  ontology_path<-file.path(w07,"ontology.csv")
  summary_path<-file.path(w07,"summary.json")
  if(!all(file.exists(c(coding_path,manifest_path,ontology_path,summary_path))))
    stop("W07 coding, manifest, ontology and completion summary are required.")
  source_manifest<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
  summary<-jsonlite::fromJSON(summary_path,simplifyVector=FALSE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  if(!identical(sha(coding_path),summary$coding_sha256)||
     !identical(sha(ontology_path),source_manifest$ontology_sha256)||
     !identical(summary$status,"TRIAL_COMPLETE"))
    stop("W07 data integrity or completion validation failed.")
  if(length(destination_zip)!=1L||is.na(destination_zip)||
     !grepl("\\.zip$",destination_zip,ignore.case=TRUE))
    stop("Supply one destination ZIP filename.")
  destination_zip<-file.path(normalizePath(dirname(destination_zip),
                                   winslash="/",mustWork=TRUE),
                              basename(destination_zip))
  if(file.exists(destination_zip))
    stop("Output ZIP already exists; refusing overwrite.")
  readjl<-function(p) {
    z<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(z[nzchar(trimws(z))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  coding<-readjl(coding_path)
  selected<-unlist(source_manifest$selected_record_ids,use.names=FALSE)
  ids<-vapply(coding,function(x)as.character(x$record_id),character(1))
  if(!length(ids)||anyDuplicated(ids)||length(ids)!=length(selected)||
     !setequal(ids,selected))
    stop("W07 coding does not cover all selected publications.")
  w04<-normalizePath(source_manifest$source_w04,mustWork=TRUE)
  canonical_path<-file.path(dirname(dirname(w04)),"W02_main","finalised","canonical.jsonl")
  if(!file.exists(canonical_path)||
     !identical(sha(canonical_path),source_manifest$canonical_w02_sha256))
    stop("Canonical W02 source does not match archived W07 provenance.")
  source<-readjl(canonical_path)
  source_ids<-vapply(source,function(z)as.character(z$identity$record_id),character(1))
  if(anyDuplicated(source_ids)||any(!ids%in%source_ids))
    stop("W07 record IDs not uniquely present in canonical bibliographic source.")
  source<-setNames(source,source_ids)
  # Use amended abstracts where W04 was finalised after a documented rescreen.
  w04_summary<-jsonlite::fromJSON(file.path(w04,"finalised","summary.json"),
                                  simplifyVector=FALSE)
  amended<-list()
  if(isTRUE(w04_summary$reconciled_abstracts)) {
    rescreens<-list.dirs(file.path(w04,"abstract_rescreens"),
                        recursive=FALSE,full.names=TRUE)
    rescreens<-rescreens[file.exists(file.path(rescreens,"manifest.json"))]
    if(length(rescreens)!=1L)
      stop("Cannot identify the single corrected-abstract set.")
    correction_file<-file.path(rescreens[[1]],"inputs","submitted_abstracts.csv")
    if(!file.exists(correction_file))stop("Missing corrected-abstract file.")
    corrections<-utils::read.csv(correction_file,stringsAsFactors=FALSE,
                                  fileEncoding="UTF-8-BOM")
    if(anyDuplicated(corrections$record_id))
      stop("Duplicated corrected abstract record IDs.")
    amended<-setNames(as.list(corrections$abstract),corrections$record_id)
  }
  field<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  ontology<-utils::read.csv(ontology_path,stringsAsFactors=FALSE,
                            fileEncoding="UTF-8-BOM",check.names=FALSE)
  if(!all(c("mechanism_id")%in%names(ontology))||
     anyDuplicated(ontology$mechanism_id))
    stop("Invalid archived ontology.")
  valid_codes<-as.character(ontology$mechanism_id)
  cases<-lapply(coding,function(x) {
    id<-as.character(x$record_id)
    record<-source[[id]]$canonical
    mechanisms<-x$mechanisms
    if(is.null(mechanisms))mechanisms<-list()
    assigned<-vapply(mechanisms,function(z)as.character(z$mechanism_id),character(1))
    if(any(!assigned%in%valid_codes)||anyDuplicated(assigned))
      stop("W07 assignments inconsistent with archived ontology: ",id)
    list(schema="ostara-w07-review-case-v1",record_id=id,
      bibliographic=list(
        title=field(record$title),
        authors=field(if(!is.null(record$authors))record$authors else record$author),
        year=field(record$year),
        source=field(if(!is.null(record$source_title))record$source_title else record$journal),
        doi=field(record$doi),
        keywords=field(if(!is.null(record$author_keywords))
                         record$author_keywords else record$keywords),
        abstract=field(if(!is.null(amended[[id]]))amended[[id]] else record$abstract)),
      coding=x)
  })
  app<-system.file("w07","review_bundle_app.R",package="evidencelandscaper")
  if(!nzchar(app))stop("W07 Shiny bundle app is not installed.")
  work<-tempfile("ostara-w07-bundle-")
  on.exit(unlink(work,recursive=TRUE,force=TRUE),add=TRUE)
  bundle<-file.path(work,"w07_review_bundle")
  dir.create(file.path(bundle,"data"),recursive=TRUE)
  queue<-file.path(bundle,"data","review_queue.jsonl")
  writeLines(vapply(cases,function(z)as.character(jsonlite::toJSON(
    z,auto_unbox=TRUE,null="null")),character(1)),queue,useBytes=TRUE)
  file.copy(ontology_path,file.path(bundle,"data","ontology.csv"))
  file.copy(app,file.path(bundle,"app.R"))
  metadata<-list(schema="ostara-w07-review-bundle-v1",
    source_w07=basename(w07),records=length(cases),
    source_coding_sha256=sha(coding_path),
    source_w04_sha256=source_manifest$source_w04_sha256,
    prompt_sha256=source_manifest$prompt_sha256,
    ontology_sha256=sha(file.path(bundle,"data","ontology.csv")),
    queue_sha256=sha(queue))
  jsonlite::write_json(metadata,file.path(bundle,"manifest.json"),
                       pretty=TRUE,auto_unbox=TRUE)
  writeLines(c(
    "OSTARA W07: CLIENT REVIEW OF MECHANISM CODING",
    "",
    "1. Unzip this package to a writable folder.",
    "2. In RStudio install packages (once):",
    "   install.packages(c('shiny','jsonlite','digest'))",
    "3. Open app.R in the extracted w07_review_bundle directory.",
    "   Select Run App, or run shiny::runApp('/path/to/w07_review_bundle').",
    "4. Enter your name; inspect each abstract and coded mechanisms.",
    "5. Accept or reject the publication's model mechanism coding and",
    "   optionally enter notes or different mechanism IDs/descriptions.",
    "6. Save each review; entries append to decisions.jsonl locally.",
    "7. Download complete CSV and full JSONL audit and send both back.",
    "",
    "Review decisions never alter W07 coding, ontology or original evidence map.",
    "A rejection records feedback, not an automatic recoding.",
    "For simultaneous reviewers, use separate unzipped copies.",
    "Do not modify data/ or manifest.json."
  ),file.path(bundle,"README.txt"),useBytes=TRUE)
  oldwd<-getwd()
  on.exit(setwd(oldwd),add=TRUE)
  setwd(work)
  utils::zip(destination_zip,
             files=c("w07_review_bundle/app.R",
                     "w07_review_bundle/README.txt",
                     "w07_review_bundle/manifest.json",
                     "w07_review_bundle/data/review_queue.jsonl",
                     "w07_review_bundle/data/ontology.csv"),flags="-q")
  if(!file.exists(destination_zip)||file.info(destination_zip)$size<=0L)
    stop("Could not create W07 review ZIP.")
  message("[W07] Created review package containing ",length(cases),
          " publications: ",destination_zip)
  invisible(destination_zip)
}
