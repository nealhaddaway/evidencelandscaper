#' Consolidate a local W00 sample and accepted RIS imports
#'
#' Produces a source-labelled record envelope (not yet W01-ready), with
#' verified source counts and immutable pointers to original source data.
#' No API calls, deduplication, Zenodo actions or W01 execution.
#' @param plan An existing plan_w00() result, or a map name/path.\n#'   For a map, selects its latest eligible W00 run with accepted RIS imports;
#'   otherwise the latest successfully retrieved run.
#' @return Path of consolidated W00 manifest.
#' @export
consolidate_w00 <- function(plan) {
  if (is.character(plan) && length(plan) == 1L && !is.na(plan)) {
    map <- open_map(plan)
    root <- file.path(map, "runs", "W00")
    candidates <- if (dir.exists(root)) list.dirs(root, recursive = FALSE,
                                                  full.names = TRUE) else character()
    candidates <- candidates[file.exists(file.path(candidates, "plan",
                                                    "search_plan.json"))]
    has_manual <- file.exists(file.path(candidates, "accepted_manual_imports.json"))
    has_auto <- file.exists(file.path(candidates, "w00_retrieval_manifest.json"))
    has_full <- file.exists(file.path(candidates, "w00_full_retrieval_manifest.json"))
    eligible <- candidates[has_manual | has_auto | has_full]
    if (!length(eligible)) {
      stop("No W00 run with accepted RIS imports or retrieval results found.",
           call. = FALSE)
    }
    # Full harvest is preferred over old sample/manual-only tests. Never mix
    # independently planned runs without explicit acceptance of provenance.
    if (any(has_full)) {
      eligible <- candidates[has_full]
    } else if (any(has_manual)) {
      eligible <- candidates[has_manual]
    }
    chosen <- sort(eligible, decreasing = TRUE)[[1L]]
    message("[W00] Selected run: ", basename(chosen))
    message("[W00] Manual imports: ",
            if (file.exists(file.path(chosen, "accepted_manual_imports.json")))
              "accepted" else "none")
    plan <- list(search_plan = file.path(chosen, "plan", "search_plan.json"))
  }
  if (!is.list(plan) || !is.character(plan$search_plan) ||
      length(plan$search_plan) != 1L || !file.exists(plan$search_plan)) {
    stop("Supply a map name/path or a valid W00 plan.", call. = FALSE)
  }
  run_dir <- normalizePath(dirname(dirname(plan$search_plan)),
                           winslash = "/", mustWork = TRUE)
  output <- file.path(run_dir, "consolidated")
  # Preserve an already published consolidation. New manually accepted inputs
  # create a separate immutable revision instead of rewriting its records.
  accepted_now <- file.path(run_dir, "accepted_manual_imports.json")
  base_manifest <- file.path(output, "manifest.json")
  if (file.exists(accepted_now) && file.exists(base_manifest)) {
    earlier <- tryCatch(jsonlite::fromJSON(base_manifest,
      simplifyVector = FALSE), error = function(e) NULL)
    latest_sha <- digest::digest(file = accepted_now, algo = "sha256",
                                 serialize = FALSE)
    if (!is.null(earlier) &&
        !identical(earlier$manual_registry_sha256, latest_sha)) {
      output <- file.path(run_dir,
                         paste0("consolidated-manual-", substr(latest_sha, 1L, 12L)))
      message("[W00] New manual acceptance detected; retaining earlier",
              " consolidation and creating: ", basename(output))
    }
  }
  if (dir.exists(output)) {
    existing_manifest <- file.path(output, "manifest.json")
    existing_records <- file.path(output, "records.jsonl")
    if (file.exists(existing_manifest) && file.exists(existing_records)) {
      previous <- tryCatch(jsonlite::fromJSON(existing_manifest,
        simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(previous) && identical(previous$status, "success") &&
          identical(previous$run_id, basename(run_dir)) &&
          identical(previous$records_sha256,
                    digest::digest(file = existing_records, algo = "sha256",
                                   serialize = FALSE))) {
        message("[W00] Existing consolidation verified: ",
                previous$record_count, " records from ",
                previous$source_count, " sources.")
        return(invisible(existing_manifest))
      }
    }
    # An interrupted write has no validated publication manifest. Preserve
    # its bytes separately and recreate this exact deterministic consolidation.
    if(file.exists(existing_manifest)) {
      stop("Existing consolidated manifest is invalid or changed; refusing overwrite.",
           call.=FALSE)
    }
    recovery<-file.path(run_dir,"interrupted_consolidations")
    dir.create(recovery,recursive=TRUE,showWarnings=FALSE)
    target<-file.path(recovery,paste0(basename(output),"-",
      format(Sys.time(),"%Y%m%dT%H%M%S",tz="UTC")))
    if(file.exists(target)||!file.rename(output,target))
      stop("Could not archive incomplete consolidation: ",output,call.=FALSE)
    message("[W00] Archived incomplete consolidation without deletion: ",target)
  }
  full_file <- file.path(run_dir, "w00_full_retrieval_manifest.json")
  sample_file <- file.path(run_dir, "w00_retrieval_manifest.json")
  if (file.exists(full_file) && file.exists(sample_file))
    stop("Both full and sample retrieval manifests exist; ambiguous source.", call. = FALSE)
  parent_file <- if (file.exists(full_file)) full_file else sample_file
  accepted_file <- file.path(run_dir, "accepted_manual_imports.json")
  if (!file.exists(parent_file) && !file.exists(accepted_file)) {
    stop("No completed automatic retrieval or accepted manual RIS imports.",
         call. = FALSE)
  }
  read_json <- function(p) jsonlite::fromJSON(p, simplifyVector = FALSE)
  sha <- function(p) digest::digest(file = p, algo = "sha256", serialize = FALSE)
  auto <- if (file.exists(parent_file)) read_json(parent_file) else NULL
  manual <- if (file.exists(accepted_file)) read_json(accepted_file) else NULL
  approval_file <- file.path(run_dir,"approved_partial_retrieval.json")
  partial <- !is.null(auto) && identical(auto$status,"incomplete") &&
    identical(parent_file,full_file)
  missing_sources <- character()
  if(partial) {
    if(!file.exists(approval_file))
      stop("Partial W00 requires explicit approval before consolidation.")
    approval<-read_json(approval_file)
    missing_sources<-names(Filter(function(z)!identical(z$status,"success"),auto$sources))
    if(!identical(approval$run_id,basename(run_dir)) ||
       !identical(approval$original_manifest_sha256,sha(parent_file)) ||
       !setequal(unlist(approval$unavailable_sources),missing_sources) ||
       !all(vapply(auto$sources[missing_sources],
         function(z)identical(z$status,"unavailable"),logical(1))))
      stop("Partial W00 approval no longer matches the source manifest.")
  } else if (!is.null(auto) && !identical(auto$status,
      if (identical(parent_file, full_file)) "success" else "sample_success")) {
    stop("Automated W00 retrieval is not successfully completed.", call. = FALSE)
  }
  if (!is.null(manual) && !identical(manual$run_id, basename(run_dir))) {
    stop("Accepted RIS registry belongs to a different W00 run.", call. = FALSE)
  }
  source_names <- c(setdiff(names(auto$sources),missing_sources), names(manual$imports))
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
  `%||%` <- function(x, y) if (is.null(x)) y else x
  auto_names <- if(is.null(auto))character() else setdiff(names(auto$sources),missing_sources)
  message("[W00] Consolidating ",length(auto_names)," successful automated sources",
          " and ",if(is.null(manual))0L else length(manual$imports)," manual sources.")
  if (!is.null(auto)) for (src in auto_names) {
    message("[W00] Loading source ",match(src,auto_names),"/",length(auto_names),
            ": ",src," (expected ",auto$sources[[src]]$records," records)")
    entry <- auto$sources[[src]]
    if (!identical(entry$status, "success")) stop("Unsuccessful source: ", src, call. = FALSE)
    dir <- file.path(run_dir, "sources", src)
    mf <- file.path(dir, "manifest.json")
    if (!file.exists(mf)) stop("Missing source manifest: ", src, call. = FALSE)
    m <- read_json(mf)
    declared <- if (src == "lens") m$unique_records_written else
      if (src == "openalex") m$unique_records_exported else
      if (src == "scopus") m$entries_retrieved else m$records_retrieved
    if (is.null(declared) || length(declared) != 1L ||
        as.integer(declared) != as.integer(entry$records)) {
      stop("Parent/source manifest record count disagreement: ", src, call. = FALSE)
    }
    if (!identical(m$status, "success") &&
        !(identical(src, "lens") && identical(m$status, "sample_success"))) {
      stop("Source manifest unsuccessful: ", src, call. = FALSE)
    }
    if (src == "openalex") {
      rows <- jsonl(file.path(dir, "unique_works.jsonl"))
      origin <- file.path("sources", src, "unique_works.jsonl")
    } else if (src %in% c("scopus", "wos")) {
      files <- sort(list.files(file.path(dir, "raw"),
        pattern = "^response_[0-9]{6}[.]json$", full.names = TRUE))
      if (!length(files) && as.integer(entry$records) > 0L)
        stop("Missing raw retrieval pages: ", src, call. = FALSE)
      rows <- unlist(lapply(files, function(f) {
        page <- read_json(f)
        if (src == "scopus") {
          if (is.null(page[["search-results"]]))
            stop("Malformed Scopus raw response: ", f, call. = FALSE)
          page[["search-results"]][["entry"]] %||% list()
        } else {
          if (is.null(page$metadata))
            stop("Malformed Web of Science raw response: ", f, call. = FALSE)
          page$hits %||% list()
        }
      }), recursive = FALSE)
      origin <- file.path("sources", src, "raw/response_*.json")
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
    message("[W00] Source complete: ",src,"; cumulative records: ",
            format(length(records),big.mark=","))
  }
  if (!is.null(manual)) for (src in names(manual$imports)) {
    message("[W00] Loading manual source: ",src)
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
    message("[W00] Manual source complete: ",src,"; cumulative records: ",
            format(length(records),big.mark=","))
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  data_path <- file.path(output, "records.jsonl")
  con <- file(data_path, "wt", encoding = "UTF-8")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  message("[W00] Writing consolidated records: ",format(length(records),big.mark=","))
  for (i in seq_along(records)) {
    writeLines(jsonlite::toJSON(records[[i]],
      auto_unbox = TRUE, null = "null", na = "null"), con, useBytes = TRUE)
    if(i %% 5000L==0L || i==length(records))
      message("[W00] Written ",format(i,big.mark=","),"/",
              format(length(records),big.mark=",")," consolidated records")
  }
  close(con)
  manifest <- list(schema = "evidencelandscaper-w00-consolidated-v1",
    status = "success", format = "source_labelled_record_envelopes",
    w01_ready = FALSE, run_id = basename(run_dir),
    record_count = length(records), source_count = length(source_names),
    sources = source_audit, partial_retrieval=partial,
    unavailable_sources=as.list(missing_sources),
    partial_approval_sha256=if(partial)sha(approval_file) else NULL,
    records_file = "records.jsonl",
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
