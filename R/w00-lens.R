#' Retrieve a bounded Lens W00 sample
#'
#' Verifies the immutable query archive from plan_w00(), generates the
#' Lens request configuration with the upstream adapter, and executes the
#' existing Lens harvester with a local sample limit. No W01 or Zenodo action.
#' @param plan Return value of plan_w00().
#' @param max_records Integer from 1 to 100. Defaults to 10.
#' @return Output directory, invisibly.
#' @export
run_w00_lens <- function(plan, max_records = 10L) {
  if (!is.list(plan) || length(plan$search_plan) != 1L ||
      !file.exists(plan$search_plan) || !dir.exists(plan$query_archive)) {
    stop("Supply an existing plan_w00() result.", call. = FALSE)
  }
  if (length(max_records) != 1L || !is.numeric(max_records) ||
      is.na(max_records) || max_records != as.integer(max_records) ||
      max_records < 1L || max_records > 100L) {
    stop("max_records must be an integer between 1 and 100.", call. = FALSE)
  }
  if (!nzchar(Sys.getenv("LENS_API_TOKEN"))) {
    stop("LENS_API_TOKEN is missing. Set it in your user .Renviron.", call. = FALSE)
  }
  archive <- jsonlite::fromJSON(
    file.path(plan$query_archive, "manifest.json"), simplifyVector = FALSE)
  entry <- archive$queries$lens
  if (is.null(entry) || !identical(entry$file, "lens.txt")) {
    stop("No Lens query in archived plan.", call. = FALSE)
  }
  path <- file.path(plan$query_archive, "lens.txt")
  if (!file.exists(path)) stop("Archived Lens query missing.", call. = FALSE)
  if (!identical(tolower(digest::digest(file = path, algo = "sha256", serialize = FALSE)),
                 tolower(entry$sha256))) {
    stop("Archived Lens query file checksum mismatch.", call. = FALSE)
  }
  query <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!identical(tolower(digest::digest(query, algo = "sha256", serialize = FALSE)),
                 tolower(entry$query_string_sha256))) {
    stop("Archived Lens query string checksum mismatch.", call. = FALSE)
  }
  original <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  if (!identical(query, original$source_queries$lens)) {
    stop("Archived Lens query differs from the plan.", call. = FALSE)
  }
  destination <- file.path(dirname(dirname(plan$search_plan)), "sources", "lens")
  if (file.exists(destination) || dir.exists(destination)) {
    stop("Lens source output already exists: refusing overwrite.", call. = FALSE)
  }
  asset <- function(name) {
    x <- system.file("w00", name, package = "evidencelandscaper")
    if (!nzchar(x)) stop("Missing package asset: ", name, call. = FALSE)
    x
  }
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  config <- file.path(destination, "lens_config.json")
  execute <- function(script, args) {
    cmd <- file.path(R.home("bin"), "Rscript")
    output <- suppressWarnings(system2(cmd,
      args = vapply(c(asset(script), args), shQuote, character(1)),
      stdout = TRUE, stderr = TRUE))
    code <- attr(output, "status")
    if (!is.null(code) && code != 0L) {
      stop("Lens ", script, " failed:\n", paste(output, collapse = "\n"),
           call. = FALSE)
    }
  }
  execute("workflow_00_build_lens_config.R",
          c("--query", query, "--output", config))
  execute("workflow_00_lens_orchestrated_search.R",
          c("--config", config, "--max-records", as.character(as.integer(max_records)),
            "--output", file.path(destination, "records.jsonl"),
            "--manifest", file.path(destination, "manifest.json"),
            "--audit-dir", file.path(destination, "audit")))
  manifest <- jsonlite::fromJSON(file.path(destination, "manifest.json"),
                                  simplifyVector = FALSE)
  if (!identical(manifest$status, "sample_success") ||
      manifest$unique_records_written > max_records) {
    stop("Lens sample completion manifest invalid.", call. = FALSE)
  }
  message("Lens sample saved to: ", destination)
  invisible(destination)
}
