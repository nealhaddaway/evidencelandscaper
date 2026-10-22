#' Import any RIS export into a local evidence-map project
#'
#' One universal RIS parser for CAB Abstracts, PQD&T, and other named sources.
#' Copies raw files, records SHA-256 checksums and search provenance, and writes
#' a normalised W00 handoff JSONL. Does not publish or invoke W01.
#'
#' @param map Map name or existing local map directory.
#' @param files One or more RIS file paths from the same source/search.
#' @param source Stable lowercase identifier, e.g. "cba" or "pqdt".
#' @param database Human-readable database name.
#' @param search_date Search date in YYYY-MM-DD format.
#' @param search_string Actual database-specific search expression.
#' @param description Short explanation of the search/export.
#' @param keyword_semantics Interpretation of RIS KW.
#' @param platform Optional exporting platform.
#' @param reported_results Optional reported result count.
#' @return Directory containing raw RIS, audit files and handoff/records.jsonl.
#' @export
import_ris <- function(map, files, source, database, search_date,
                       search_string, description = "Manual RIS export",
                       keyword_semantics = c("unknown", "author_keywords",
                                             "indexing_terms", "mixed"),
                       platform = NULL, reported_results = NULL) {
  map <- open_map(map)
  keyword_semantics <- match.arg(keyword_semantics)
  if (!is.character(files) || !length(files) || anyNA(files) ||
      any(!file.exists(files)) || anyDuplicated(basename(files))) {
    stop("Supply existing RIS files with unique filenames.", call. = FALSE)
  }
  if (any(!grepl("\\.ris$", files, ignore.case = TRUE))) {
    stop("Every input must have a .ris extension.", call. = FALSE)
  }
  files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  scalar <- function(x) is.character(x) && length(x) == 1L &&
    !is.na(x) && nzchar(trimws(x))
  if (!scalar(source) || !grepl("^[a-z0-9][a-z0-9_-]*$", source)) {
    stop("source must be a stable lowercase identifier.", call. = FALSE)
  }
  if (!scalar(database) || !scalar(description) || !scalar(search_string)) {
    stop("database, description and search_string must be nonempty.", call. = FALSE)
  }
  if (!scalar(search_date) || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", search_date) ||
      is.na(as.Date(search_date, format = "%Y-%m-%d"))) {
    stop("search_date must be a valid YYYY-MM-DD date.", call. = FALSE)
  }
  if (!is.null(platform) && !scalar(platform)) {
    stop("platform must be a nonempty string or NULL.", call. = FALSE)
  }
  if (!is.null(reported_results) &&
      (!is.numeric(reported_results) || length(reported_results) != 1L ||
       is.na(reported_results) || reported_results < 0L ||
       reported_results != as.integer(reported_results))) {
    stop("reported_results must be a nonnegative integer or NULL.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE)) {
    stop("Install jsonlite and digest.", call. = FALSE)
  }
  script <- system.file("w00", "workflow_00_ris_ingestion.R",
                        package = "evidencelandscaper")
  if (!nzchar(script)) stop("Packaged RIS ingestion script is missing.", call. = FALSE)
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
                   "-", substr(basename(tempfile()), 1L, 12L))
  dest <- file.path(map, "runs", "W00", "manual", paste0(source, "-", run_id))
  if (dir.exists(dest)) stop("RIS import directory already exists.", call. = FALSE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  registry <- list(
    schema_version = "workflow00-source-registry-v1",
    acquisition = list(method = "ris_upload", source_format = "RIS"),
    database = list(name = database, short_name = source, platform = platform),
    search = list(description = description, search_date = search_date,
                  search_string = search_string,
                  reported_results = if (is.null(reported_results)) NULL
                                     else as.integer(reported_results)),
    field_semantics = list(ris_KW = keyword_semantics),
    input = list(expected_chunk_count = length(files),
                 files = lapply(basename(files), function(f) list(filename = f))),
    staging = list(method = "local_files", publish_after_validation = FALSE)
  )
  registry_file <- file.path(dest, "source_registry.json")
  jsonlite::write_json(registry, registry_file, auto_unbox = TRUE,
                       pretty = TRUE, null = "null")
  args <- c(script, unlist(lapply(files, function(f) c("--ris", f)),
                           use.names = FALSE),
            "--registry", registry_file, "--output-dir", file.path(dest, "import"))
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                     args = vapply(args, shQuote, character(1)),
                     stdout = TRUE, stderr = TRUE))
  code <- attr(output, "status")
  if (!is.null(code) && code != 0L) {
    stop("RIS import failed; local audit retained in ", dest, ":\n",
         paste(output, collapse = "\n"), call. = FALSE)
  }
  manifest_file <- file.path(dest, "import", "manifest.json")
  if (!file.exists(manifest_file)) {
    stop("RIS import did not produce a manifest: ", dest, call. = FALSE)
  }
  manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)
  if (!identical(manifest$status, "success")) {
    stop("RIS import manifest reports failure: ", dest, call. = FALSE)
  }
  message("RIS imported: ", manifest$records$unique_records_for_handover,
          " unique records from ", source)
  invisible(file.path(dest, "import"))
}
