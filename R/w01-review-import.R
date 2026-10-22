#' Validate and import a portable W01 reviewer decision layer
#'
#' Never merges reviewer decisions or changes the source corpus. Every submission
#' remains a separate attributed layer; reimports of an identical layer are
#' rejected, and each case is checked against the assigned case queue.
#' @param package_dir Reviewer package directory with its bundle manifest.
#' @param destination Local evidence-map directory for independent layers.
#' @return Imported decision-layer manifest path, invisibly.
#' @export
import_w01_review <- function(package_dir, destination) {
  meta_file <- file.path(package_dir,"bundle_manifest.json")
  queue_file <- file.path(package_dir,"assigned_cases.jsonl")
  decision_file <- file.path(package_dir,"reviewer_state","decisions.jsonl")
  reviewer_file <- file.path(package_dir,"reviewer_state","reviewer.json")
  corrections_file <- file.path(package_dir,"reviewer_state","abstract_corrections.jsonl")
  necessary <- c(meta_file,queue_file,decision_file,reviewer_file)
  if (any(!file.exists(necessary)))
    stop("Returned reviewer package missing manifest, queue, identity or decisions.")
  meta <- jsonlite::fromJSON(meta_file,simplifyVector=FALSE)
  if (!identical(meta$schema,"local-w01-review-bundle-v1"))
    stop("Unknown review bundle schema.")
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  if (!identical(sha(queue_file),meta$assigned_queue_sha256))
    stop("Assigned queue changed since packaging.")
  records <- readLines(queue_file,warn=FALSE,encoding="UTF-8")
  records <- records[nzchar(trimws(records))]
  ids <- vapply(records,function(x) jsonlite::fromJSON(x,simplifyVector=FALSE)$review_case_id,character(1))
  if (!setequal(ids,unlist(meta$assigned_case_ids,use.names=FALSE)))
    stop("Bundle case assignments altered.")
  who <- jsonlite::fromJSON(reviewer_file,simplifyVector=FALSE)$reviewer
  if (!is.character(who)||length(who)!=1L||!nzchar(trimws(who)))
    stop("Reviewer identity missing.")
  lines <- readLines(decision_file,warn=FALSE,encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) stop("No decisions provided.")
  events <- lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  for(e in events) {
    if (!identical(e$schema,"local-w01-decision-v1") ||
        !identical(e$queue_sha256,meta$assigned_queue_sha256) ||
        !identical(e$reviewer,who) ||
        !e$review_case_id %in% ids ||
        !e$decision %in% c("duplicate","not_duplicate","uncertain") ||
        !is.character(e$rationale)||!nzchar(trimws(e$rationale)))
      stop("Invalid or foreign reviewer decision event.")
  }
  last <- list()
  for(e in events) last[[e$review_case_id]] <- e
  correction_events <- if(file.exists(corrections_file)) {
    x <- readLines(corrections_file,warn=FALSE,encoding="UTF-8")
    lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
  } else list()
  for(e in correction_events) {
    if (!identical(e$schema,"local-abstract-correction-v1") ||
        !identical(e$reviewer,who) ||
        !identical(e$queue_sha256,meta$assigned_queue_sha256) ||
        !e$review_case_id %in% ids ||
        !e$action %in% c("edit","delete") ||
        !is.character(e$record_key) || length(e$record_key)!=1L ||
        !is.character(e$value) || length(e$value)!=1L) {
      stop("Invalid or foreign abstract correction event.")
    }
  }
  if (!dir.exists(destination)) dir.create(destination,recursive=TRUE)
  # Import digest names never depend on untrusted reviewer text.
  layer_id <- substr(sha(decision_file),1L,20L)
  target <- file.path(destination,paste0("review-layer-",layer_id))
  if (file.exists(target)||dir.exists(target))
    stop("Already imported this decision layer: ",target)
  dir.create(target)
  file.copy(decision_file,file.path(target,"decision_events.jsonl"))
  file.copy(reviewer_file,file.path(target,"reviewer.json"))
  if(file.exists(corrections_file))
    file.copy(corrections_file,file.path(target,"abstract_corrections.jsonl"))
  file.copy(meta_file,file.path(target,"bundle_manifest.json"))
  jsonlite::write_json(list(schema="local-w01-review-import-v1",
    reviewer=who,case_count=length(last),event_count=length(events),
    abstract_correction_events=length(correction_events),
    abstract_corrections_sha256=if(file.exists(corrections_file)) sha(corrections_file) else NULL,
    queue_sha256=meta$assigned_queue_sha256,
    decision_sha256=sha(decision_file),
    imported_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")),
    file.path(target,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(file.path(target,"manifest.json"))
}
