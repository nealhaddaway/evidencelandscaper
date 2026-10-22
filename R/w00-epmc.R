#' Run a bounded Europe PMC-hosted W00 source sample
#'
#' Retains the upstream AGRICOLA and Europe PMC source ingestion scripts.
#' All results are local; no deduplication or W01 dispatch occurs.
#' @param plan Existing result of plan_w00().
#' @param source One of agricola, pubmed, ethos, cba, epmc_preprints.
#' @param max_records Maximum records, 1 to 100.
#' @return Source output directory, invisibly.
#' @export
run_w00_epmc <- function(plan, source, max_records = 10L) {
  specs <- list(
    agricola = c("AGR", "agricola"),
    pubmed = c("MED", "pubmed"),
    ethos = c("ETH", "ethos"),
    cba = c("CBA", "cba"),
    epmc_preprints = c("PPR", "epmc_preprints")
  )
  if (!is.character(source) || length(source) != 1L ||
      is.na(source) || !source %in% names(specs)) {
    stop("Unsupported Europe PMC-hosted source.", call. = FALSE)
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
    stop("max_records must be 1 to 100.", call. = FALSE)
  }
  archive <- jsonlite::fromJSON(file.path(plan$query_archive, "manifest.json"),
                                 simplifyVector = FALSE)
  entry <- archive$queries[[source]]
  if (is.null(entry) || !identical(entry$file, paste0(source, ".txt"))) {
    stop("Source not present in the archived query plan: ", source, call. = FALSE)
  }
  query_file <- file.path(plan$query_archive, entry$file)
  if (!file.exists(query_file)) stop("Missing archived source query.", call. = FALSE)
  file_hash <- digest::digest(file = query_file, algo = "sha256", serialize = FALSE)
  if (!identical(tolower(file_hash), tolower(entry$sha256))) {
    stop("Archived query file checksum mismatch.", call. = FALSE)
  }
  query <- paste(readLines(query_file, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
  query_hash <- digest::digest(query, algo = "sha256", serialize = FALSE)
  if (!identical(tolower(query_hash), tolower(entry$query_string_sha256))) {
    stop("Archived query string checksum mismatch.", call. = FALSE)
  }
  original <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  if (!identical(query, original$source_queries[[source]])) {
    stop("Archived source query differs from plan.", call. = FALSE)
  }
  run_dir <- dirname(dirname(plan$search_plan))
  destination <- file.path(run_dir, "sources", source)
  if (file.exists(destination) || dir.exists(destination)) {
    stop("Source output already exists; refusing overwrite: ", destination,
         call. = FALSE)
  }
  file_name <- if (source == "agricola") {
    "workflow_00d_agricola_ingestion.R"
  } else {
    "workflow_00f_europe_pmc_source_ingestion.R"
  }
  script <- system.file("w00", file_name, package = "evidencelandscaper")
  if (!nzchar(script)) stop("Missing source ingestion script: ", file_name,
                            call. = FALSE)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  args <- c(script, "--query", query, "--max-records",
            as.character(as.integer(max_records)), "--page-size", "100",
            "--output-dir", destination)
  if (source != "agricola") {
    args <- c(args, "--source-code", specs[[source]][[1L]],
              "--source-slug", specs[[source]][[2L]],
              "--source-collection", specs[[source]][[2L]])
  }
  result <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                    args = vapply(args, shQuote, character(1)),
                    stdout = TRUE, stderr = TRUE))
  exit_code <- attr(result, "status")
  if (!is.null(exit_code) && exit_code != 0L) {
    stop("W00 ", source, " ingestion failed:\n",
         paste(result, collapse = "\n"), call. = FALSE)
  }
  manifest_file <- file.path(destination, "manifest.json")
  if (!file.exists(manifest_file)) stop("Source ingestion manifest missing.",
                                        call. = FALSE)
  manifest <- jsonlite::fromJSON(manifest_file, simplifyVector = FALSE)
  if (!identical(manifest$status, "success") ||
      is.null(manifest$records_retrieved) ||
      manifest$records_retrieved > max_records) {
    stop("Source ingestion completion validation failed.", call. = FALSE)
  }
  invisible(destination)
}
