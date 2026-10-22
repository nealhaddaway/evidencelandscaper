#' Run a bounded EBSCOhost W00 database sample
#'
#' Uses the original EBSCO EIT R harvester with local outputs and archived
#' query verification. No Zenodo, W01, or GitHub Actions invocation.
#' @param plan Existing plan_w00() result.
#' @param source One configured EBSCO source ID.
#' @param max_records Integer 1..100.
#' @return Local source output directory, invisibly.
#' @export
run_w00_ebsco <- function(plan, source, max_records = 10L) {
  if (!is.character(source) || length(source) != 1L || is.na(source)) {
    stop("source must be one EBSCO source ID.", call. = FALSE)
  }
  cfg_file <- system.file("w00", "config/workflow00_ebsco_sources.json",
                          package = "evidencelandscaper")
  if (!nzchar(cfg_file)) stop("Missing EBSCO configuration.", call. = FALSE)
  cfg <- jsonlite::fromJSON(cfg_file, simplifyVector = FALSE)
  if (!source %in% names(cfg$sources)) {
    stop("Unknown configured EBSCO source: ", source, call. = FALSE)
  }
  if (!is.list(plan) || !is.character(plan$search_plan) ||
      length(plan$search_plan) != 1L || !file.exists(plan$search_plan) ||
      !is.character(plan$query_archive) ||
      length(plan$query_archive) != 1L || !dir.exists(plan$query_archive)) {
    stop("Supply an existing plan_w00() result.", call. = FALSE)
  }
  if (!is.numeric(max_records) || length(max_records) != 1L ||
      is.na(max_records) || max_records != as.integer(max_records) ||
      max_records < 1L || max_records > 100L) {
    stop("max_records must be between 1 and 100.", call. = FALSE)
  }
  if (!nzchar(Sys.getenv("EBSCO_EHOST_UID")) ||
      !nzchar(Sys.getenv("EBSCO_EHOST_PWD"))) {
    stop("EBSCO_EHOST_UID and EBSCO_EHOST_PWD are required.", call. = FALSE)
  }
  archive <- jsonlite::fromJSON(file.path(plan$query_archive, "manifest.json"),
                                 simplifyVector = FALSE)
  entry <- archive$queries[[source]]
  if (is.null(entry) || !identical(entry$file, paste0(source, ".txt"))) {
    stop("EBSCO source not present in archived query plan.", call. = FALSE)
  }
  query_file <- file.path(plan$query_archive, entry$file)
  if (!file.exists(query_file)) stop("Missing EBSCO archived query.", call. = FALSE)
  file_hash <- digest::digest(file = query_file, algo = "sha256", serialize = FALSE)
  if (!identical(tolower(file_hash), tolower(entry$sha256))) {
    stop("EBSCO archived query checksum mismatch.", call. = FALSE)
  }
  query <- paste(readLines(query_file, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
  if (!identical(tolower(digest::digest(query, algo = "sha256", serialize = FALSE)),
                 tolower(entry$query_string_sha256))) {
    stop("EBSCO archived query string checksum mismatch.", call. = FALSE)
  }
  original <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  if (!identical(query, original$source_queries[[source]])) {
    stop("EBSCO archived query differs from search plan.", call. = FALSE)
  }
  destination <- file.path(dirname(dirname(plan$search_plan)), "sources", source)
  if (file.exists(destination) || dir.exists(destination)) {
    stop("EBSCO source output already exists; refusing overwrite.", call. = FALSE)
  }
  script <- system.file("w00", "workflow_00g_ebsco_ingestion.R",
                        package = "evidencelandscaper")
  if (!nzchar(script)) stop("Missing EBSCO harvester script.", call. = FALSE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  args <- c(script, "--source", source, "--query", query,
            "--config", cfg_file, "--output-dir", destination,
            "--max-records", as.character(as.integer(max_records)),
            "--page-size", as.character(as.integer(max_records)))
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                   args = vapply(args, shQuote, character(1)),
                   stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("EBSCO ", source, " ingestion failed:\n",
         paste(out, collapse = "\n"), call. = FALSE)
  }
  mf <- file.path(destination, "manifest.json")
  if (!file.exists(mf)) stop("EBSCO manifest missing.", call. = FALSE)
  result <- jsonlite::fromJSON(mf, simplifyVector = FALSE)
  if (!identical(result$status, "success") ||
      !identical(result$source, source) ||
      is.null(result$records_retrieved) ||
      result$records_retrieved > max_records) {
    stop("EBSCO completion manifest invalid.", call. = FALSE)
  }
  invisible(destination)
}
