#' Consolidate a local W00 sample and accepted RIS imports
#'
#' Produces a source-labelled record envelope (not yet W01-ready), with
#' verified source counts and immutable pointers to original source data.
#' No API calls, deduplication, Zenodo actions or W01 execution.
#' @param plan Existing plan_w00() result.
#' @return Path of consolidated W00 manifest.
#' @export
consolidate_w00 <- function(plan) {
  if (!is.list(plan) || length(plan$search_plan) != 1L ||
      !file.exists(plan$search_plan)) stop("Supply a valid W00 plan.", call. = FALSE)
  run_dir <- normalizePath(dirname(dirname(plan$search_plan)),
                           winslash = "/", mustWork = TRUE)
  output <- file.path(run_dir, "consolidated")
  if (file.exists(output) || dir.exists(output)) {
    stop("Consolidated output already exists; refusing overwrite.", call. = FALSE)
  }
  parent_file <- file.path(run_dir, "w00_retrieval_manifest.json")
  accepted_file <- file.path(run_dir, "accepted_manual_imports.json")
  if (!file.exists(parent_file) && !file.exists(accepted_file)) {
    stop("No completed automatic retrieval or accepted manual RIS imports.",
         call. = FALSE)
  }
  read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  auto <- if (file.exists(parent_file)) read_json(parent_file) else NULL
  manual <- if (file.exists(accepted_file)) read_json(accepted_file) else NULL
  if (!is.null(auto) && !identical(auto$status, "sample_success")) {
    stop("Automated W00 retrieval is not successfully completed.", call. = FALSE)
  }
  if (!is.null(manual) && !identical(manual$run_id, basename(run_dir))) {
    stop("Accepted RIS registry belongs to a different W00 run.", call. = FALSE)
  }
  source_names <- c(names(auto$sources), names(manual$imports))
  if (anyDuplicated(source_names)) stop("Automated and manual sources overlap.", call. = FALSE)
  if (!length(source_names)) stop("No sources selected.", call. = FALSE)
  records <- list()
  source_audit <- list()
  append_rows <- function(source, rows, origin, expected) {
    if (length(rows) != as.integer(expected)) {
      stop("W00 count mismatch for ", source, ": expected ", expected,
           ", found ", length(rows), call. = FALSE)
    }
    for (i in seq_along(rows)) {
      records[[length(records) + 1L]] <<- list(
        source = source, source_record_index = i,
        origin = origin, original_record = rows[[i]])
    }
    source_audit[[source]] <<- list(records = length(rows),
                                  input = origin, status = "validated")
  }
  jsonl <- function(path) {
    if (!file.exists(path)) stop("Missing JSONL: ", path, call. = FALSE)
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
  }
  if (!is.null(auto)) for (src in names(auto$sources)) {
    entry <- auto$sources[[src]]
    if (!identical(entry$status, "success")) stop("Unsuccessful source: ", src, call. = FALSE)
    dir <- file.path(run_dir, "sources", src)
    mf <- file.path(dir, "manifest.json")
    if (!file.exists(mf)) stop("Missing source manifest: ", src, call. = FALSE)
    m <- read_json(mf)
    if (!identical(m$status, "success") &&
        !(identical(src, "lens") && identical(m$status, "sample_success"))) {
      stop("Source manifest unsuccessful: ", src, call. = FALSE)
    }
    if (src == "openalex") {
      rows <- jsonl(file.path(dir, "unique_works.jsonl"))
      origin <- file.path("sources", src, "unique_works.jsonl")
    } else if (src == "lens" || startsWith(src, "ebsco_")) {
      rows <- jsonl(file.path(dir, "records.jsonl"))
      origin <- file.path("sources", src, "records.jsonl")
    } else {
      files <- sort(list.files(file.path(dir, "raw"),
        pattern = "^response_[0-9]{6}[.]json$", full.names = TRUE))
      if (!length(files) && as.integer(entry$records) > 0L) {
        stop("Missing Europe PMC raw pages: ", src, call. = FALSE)
      }
      rows <- unlist(lapply(files, function(f) {
        page <- read_json(f)
        if (is.null(page$resultList$result)) list() else page$resultList$result
      }), recursive = FALSE)
      origin <- file.path("sources", src, "raw/response_*.json")
    }
    append_rows(src, rows, origin, entry$records)
  }
  if (!is.null(manual)) for (src in names(manual$imports)) {
    entry <- manual$imports[[src]]
    path <- normalizePath(entry$import_directory, winslash = "/", mustWork = TRUE)
    map <- normalizePath(file.path(run_dir, "..", "..", ".."),
                         winslash = "/", mustWork = TRUE)
    root <- normalizePath(file.path(map, "runs", "W00", "manual"),
                          winslash = "/", mustWork = TRUE)
    if (!startsWith(paste0(path, "/"), paste0(root, "/"))) {
      stop("Accepted RIS path outside map: ", src, call. = FALSE)
    }
    mf <- file.path(path, "manifest.json")
    handoff <- file.path(path, "handoff", "records.jsonl")
    if (!identical(sha(mf), entry$manifest_sha256) ||
        !identical(sha(handoff), entry$handoff_sha256)) {
      stop("Accepted RIS content changed: ", src, call. = FALSE)
    }
    m <- read_json(mf)
    if (!identical(m$status, "success") || !identical(m$source, src)) {
      stop("Accepted RIS manifest invalid: ", src, call. = FALSE)
    }
    for (f in m$chunks$files) {
      raw <- file.path(path, f$path)
      if (!file.exists(raw) || !identical(sha(raw), f$sha256)) {
        stop("Original RIS file changed: ", src, call. = FALSE)
      }
    }
    append_rows(src, jsonl(handoff), handoff, entry$records)
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(output, "records.jsonl")
  con <- file(data_path, "wt", encoding = "UTF-8")
  on.exit(if (isOpen(con)) close(con), add = TRUE)
  for (record in records) writeLines(jsonlite::toJSON(record,
    auto_unbox = TRUE, null = "null", na = "null"), con, useBytes = TRUE)
  close(con)
  manifest <- list(schema = "evidencelandscaper-w00-consolidated-v1",
    status = "success", format = "source_labelled_record_envelopes",
    w01_ready = FALSE, run_id = basename(run_dir),
    record_count = length(records), source_count = length(source_names),
    sources = source_audit, records_file = "records.jsonl",
    records_sha256 = sha(data_path),
    automated_manifest_sha256 = if (file.exists(parent_file)) sha(parent_file) else NULL,
    manual_registry_sha256 = if (file.exists(accepted_file)) sha(accepted_file) else NULL,
    created_at_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"))
  path <- file.path(output, "manifest.json")
  jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  message("[W00] Consolidated ", length(records), " records from ",
          length(source_names), " sources. W01 handover not yet validated.")
  invisible(path)
}
