#' Launch local W01 reviewer application
#' @param queue JSONL W01 human-adjudication case queue.
#' @param output_dir Persistent local reviewer state directory.
#' @return Invisibly returns output directory when app exits.
#' @export
adjudicate_w01 <- function(queue, output_dir = NULL) {
  if (!is.character(queue) || length(queue) != 1L ||
      !file.exists(queue)) stop("Existing W01 adjudication queue required.")
  queue <- normalizePath(queue, winslash = "/", mustWork = TRUE)
  if (is.null(output_dir)) output_dir <- file.path(dirname(queue), "reviewer_state")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  script <- system.file("adjudication", "app.R", package = "evidencelandscaper")
  if (!nzchar(script)) stop("Packaged local adjudication app missing.")
  status <- system2(file.path(R.home("bin"), "Rscript"),
      args = vapply(c(script, queue, output_dir), shQuote, character(1)),
      wait = TRUE)
  if (!identical(as.integer(status), 0L))
    stop("Local adjudication app exited unsuccessfully.", call. = FALSE)
  invisible(output_dir)
}

#' Export a subset of W01 cases as a self-contained local reviewer folder
#'
#' Contains only the assigned cases, a copy of the Shiny app and a manifest.
#' Does not include credentials or any other review records. A reviewer needs
#' R and the shiny/jsonlite/digest packages available to run launch.R.
#' @param queue Original W01 adjudication JSONL.
#' @param case_ids Optional case IDs assigned to this reviewer. Defaults to all.
#' @param destination New folder in which to create reviewer bundle.
#' @return Destination directory, invisibly.
#' @export
package_w01_review <- function(queue, destination, case_ids = NULL) {
  if (!file.exists(queue)) stop("Review queue not found.")
  if (file.exists(destination) || dir.exists(destination))
    stop("Reviewer package destination already exists: ", destination)
  lines <- readLines(queue, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  cases <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  ids <- vapply(cases, function(x) as.character(x$review_case_id),
                character(1))
  if (anyDuplicated(ids) || any(!nzchar(ids)))
    stop("Invalid queue identifiers.")
  if (!is.null(case_ids)) {
    if (anyDuplicated(case_ids) || any(!case_ids %in% ids))
      stop("Assigned case IDs must be unique and present in queue.")
    lines <- lines[ids %in% case_ids]
  }
  if (!length(lines)) stop("Package requires at least one case.")
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(destination, "assigned_cases.jsonl")
  writeLines(lines, file, useBytes = TRUE)
  from <- system.file("adjudication", "app.R", package = "evidencelandscaper")
  if (!file.copy(from, file.path(destination, "app.R")))
    stop("Could not copy local adjudication app.")
  writeLines(c(
    'if (!requireNamespace("shiny", quietly = TRUE) ||',
    '    !requireNamespace("jsonlite", quietly = TRUE) ||',
    '    !requireNamespace("digest", quietly = TRUE))',
    '  stop("Install shiny, jsonlite and digest first.")',
    'script <- normalizePath("app.R", mustWork = TRUE)',
    'queue <- normalizePath("assigned_cases.jsonl", mustWork = TRUE)',
    'dir.create("reviewer_state", showWarnings = FALSE)',
    'system2(file.path(R.home("bin"), "Rscript"),',
    '  args = vapply(c(script, queue, normalizePath("reviewer_state")),',
    '    shQuote, character(1)), wait = TRUE)'
  ), file.path(destination, "launch.R"))
  jsonlite::write_json(list(schema = "local-w01-review-bundle-v1",
    created_at_utc = format(Sys.time(), tz="UTC",format="%Y-%m-%dT%H:%M:%SZ"),
    original_queue_sha256 = digest::digest(file=queue,algo="sha256",serialize=FALSE),
    assigned_queue_sha256 = digest::digest(file=file,algo="sha256",serialize=FALSE),
    assigned_case_ids = vapply(lines,function(x) jsonlite::fromJSON(
      x,simplifyVector=FALSE)$review_case_id,character(1))),
    file.path(destination,"bundle_manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  invisible(destination)
}
