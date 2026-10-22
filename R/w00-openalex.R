#' Retrieve a limited OpenAlex W00 sample
#'
#' Uses the unchanged OpenAlex ingestion R script from LivingEvidenceMap/main.
#' Requires OPENALEX_API_KEY or OPENALEX_API_TOKEN in the user's environment.
#' This function never uploads to Zenodo or triggers W01.
#'
#' @param plan Result of plan_w00(), including search_plan and query_archive.
#' @param max_records Maximum records to request (1 to 100; default 10).
#' @return Path of source-specific output directory, invisibly.
#' @export
run_w00_openalex <- function(plan, max_records = 10L) {
  if (!is.list(plan) || !is.character(plan$search_plan) ||
      length(plan$search_plan) != 1L ||
      !file.exists(plan$search_plan) ||
      !dir.exists(plan$query_archive)) {
    stop("Supply an existing plan_w00() result.", call. = FALSE)
  }
  if (length(max_records) != 1L || is.na(max_records) ||
      !is.numeric(max_records) || max_records != as.integer(max_records) ||
      max_records < 1L || max_records > 100L) {
    stop("max_records must be an integer between 1 and 100.", call. = FALSE)
  }
  if (!nzchar(Sys.getenv("OPENALEX_API_KEY")) &&
      !nzchar(Sys.getenv("OPENALEX_API_TOKEN"))) {
    stop("Set OPENALEX_API_KEY (or OPENALEX_API_TOKEN) in your user .Renviron.",
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE) ||
      !requireNamespace("httr2", quietly = TRUE)) {
    stop("The packages jsonlite, digest, and httr2 are required.", call. = FALSE)
  }

  manifest_path <- file.path(plan$query_archive, "manifest.json")
  if (!file.exists(manifest_path)) stop("Missing query archive manifest.", call. = FALSE)
  archive <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  entry <- archive$queries$openalex
  if (is.null(entry) || !identical(entry$file, "openalex.txt")) {
    stop("This search plan has no archived OpenAlex query.", call. = FALSE)
  }
  query_file <- file.path(plan$query_archive, entry$file)
  if (!file.exists(query_file)) stop("Missing archived OpenAlex query.", call. = FALSE)
  file_hash <- digest::digest(file = query_file, algo = "sha256", serialize = FALSE)
  if (!identical(tolower(file_hash), tolower(entry$sha256))) {
    stop("Archived OpenAlex query checksum mismatch.", call. = FALSE)
  }
  query <- paste(readLines(query_file, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
  query_hash <- digest::digest(query, algo = "sha256", serialize = FALSE)
  if (!identical(tolower(query_hash), tolower(entry$query_string_sha256))) {
    stop("Archived OpenAlex query string checksum mismatch.", call. = FALSE)
  }
  original_plan <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  if (!identical(query, original_plan$source_queries$openalex)) {
    stop("Archived query differs from search plan.", call. = FALSE)
  }

  source_dir <- file.path(dirname(plan$search_plan), "..", "sources", "openalex")
  source_dir <- normalizePath(source_dir, mustWork = FALSE)
  if (dir.exists(source_dir)) {
    stop("OpenAlex output already exists for this run; refusing to overwrite.",
         call. = FALSE)
  }
  script <- system.file("w00", "workflow_00c_openalex_ingestion.R",
                        package = "evidencelandscaper")
  if (!nzchar(script)) stop("Missing packaged OpenAlex ingestion script.", call. = FALSE)
  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  cmd <- file.path(R.home("bin"), "Rscript")
  args <- c(script, "--query", query, "--max-records",
            as.character(as.integer(max_records)), "--page-size", "100",
            "--output-dir", source_dir)
  output <- suppressWarnings(system2(cmd, args = vapply(args, shQuote, character(1)),
                                    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("OpenAlex retrieval failed; inspect output directory.\n",
         paste(output, collapse = "\n"), call. = FALSE)
  }
  if (!file.exists(file.path(source_dir, "manifest.json"))) {
    stop("OpenAlex ingestion returned without a manifest.", call. = FALSE)
  }
  message("OpenAlex W00 ingestion completed: ", source_dir)
  invisible(source_dir)
}
