#' Execute a bounded local W00 retrieval plan
#'
#' Executes the supported source adapters sequentially and writes a summary
#' manifest inside the existing W00 run directory. Never launches W01,
#' publishes to Zenodo, or performs a full-corpus harvest.
#'
#' @param plan Result from [plan_w00()].
#' @param max_records Maximum records per source (1 to 100). Default 10.
#' @return A list with source results and the parent manifest path.
#' @export
run_w00 <- function(plan, max_records = 10L) {
  if (!is.list(plan) || !is.character(plan$search_plan) ||
      length(plan$search_plan) != 1L || !file.exists(plan$search_plan) ||
      !is.character(plan$query_archive) ||
      length(plan$query_archive) != 1L ||
      !dir.exists(plan$query_archive)) {
    stop("Supply a valid existing plan_w00() result.", call. = FALSE)
  }
  if (!is.numeric(max_records) || length(max_records) != 1L ||
      is.na(max_records) || max_records != as.integer(max_records) ||
      max_records < 1L || max_records > 100L) {
    stop("max_records must be an integer from 1 to 100.", call. = FALSE)
  }
  plan_data <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  sources <- names(plan_data$source_queries)
  if (!length(sources) || anyDuplicated(sources)) {
    stop("W00 plan must contain unique source queries.", call. = FALSE)
  }
  manual <- intersect(sources, c("cba", "pqdt"))
  if (length(manual)) {
    stop("Manual RIS upload required for: ", paste(manual, collapse = ", "),
         ". API retrieval is disabled. No retrieval started.", call. = FALSE)
  }
  config_path <- system.file("w00", "config/workflow00_ebsco_sources.json",
                             package = "evidencelandscaper")
  ebsco_sources <- names(jsonlite::fromJSON(config_path, simplifyVector = FALSE)$sources)
  supported <- c("openalex", "lens", "agricola", "pubmed", "ethos", "epmc_preprints", ebsco_sources)
  unknown <- setdiff(sources, supported)
  if (length(unknown)) {
    stop("W00 retrieval is not yet implemented for: ",
         paste(unknown, collapse = ", "), ". No retrieval started.",
         call. = FALSE)
  }
  if (!identical(plan_data$run_type, "full")) {
    stop("Only bounded full-run samples are currently supported.", call. = FALSE)
  }
  # Preflight every credential before the first request.
  if ("lens" %in% sources && !nzchar(Sys.getenv("LENS_API_TOKEN"))) {
    stop("LENS_API_TOKEN is missing; no retrieval started.", call. = FALSE)
  }
  if ("openalex" %in% sources &&
      !nzchar(Sys.getenv("OPENALEX_API_KEY")) &&
      !nzchar(Sys.getenv("OPENALEX_API_TOKEN"))) {
    stop("OPENALEX_API_KEY is missing; no retrieval started.", call. = FALSE)
  }
  if (any(sources %in% ebsco_sources) &&
      (!nzchar(Sys.getenv("EBSCO_EHOST_UID")) ||
       !nzchar(Sys.getenv("EBSCO_EHOST_PWD")))) {
    stop("EBSCO_EHOST_UID and EBSCO_EHOST_PWD required; no retrieval started.",
         call. = FALSE)
  }
  run_dir <- normalizePath(dirname(dirname(plan$search_plan)),
                           winslash = "/", mustWork = TRUE)
  summary_path <- file.path(run_dir, "w00_retrieval_manifest.json")
  if (file.exists(summary_path)) {
    stop("W00 manifest already exists; refusing to overwrite.", call. = FALSE)
  }
  existing <- sources[vapply(sources, function(src) {
    dir.exists(file.path(run_dir, "sources", src))
  }, logical(1))]
  if (length(existing)) {
    stop("Existing source output would be overwritten: ",
         paste(existing, collapse = ", "), ". Create a new W00 plan.",
         call. = FALSE)
  }
  results <- list()
  started <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  write_summary <- function(status) {
    out <- list(workflow = "00", status = status, execution = "local_bounded_sample",
                max_records_per_source = as.integer(max_records),
                started_at_utc = started,
                updated_at_utc = format(Sys.time(), tz = "UTC",
                                        format = "%Y-%m-%dT%H:%M:%SZ"),
                search_plan = "plan/search_plan.json",
                sources = results)
    jsonlite::write_json(out, summary_path, auto_unbox = TRUE,
                         pretty = TRUE, null = "null")
  }
  run_start <- Sys.time()
  display_name <- function(source) {
    if (source %in% ebsco_sources) {
      return(paste0("EBSCO: ", jsonlite::fromJSON(config_path,
        simplifyVector = FALSE)$sources[[source]]$display_name))
    }
    switch(source, openalex = "OpenAlex", lens = "Lens",
           agricola = "AGRICOLA", pubmed = "PubMed",
           ethos = "EThOS", epmc_preprints = "Europe PMC preprints", source)
  }
  log_step <- function(...) message("[W00] ", paste0(..., collapse = ""))
  log_step("Starting bounded retrieval | ", length(sources),
           " sources | up to ", max_records, " records per source")
  log_step("Run: ", basename(run_dir))
  write_summary("running")
  for (i in seq_along(sources)) {
    source <- sources[[i]]
    source_start <- Sys.time()
    log_step("[", i, "/", length(sources), "] Searching ", display_name(source),
             " (", source, ")...")
    outcome <- tryCatch({
      output_dir <- switch(source,
        openalex = run_w00_openalex(plan, max_records),
        lens = run_w00_lens(plan, max_records),
        agricola = run_w00_epmc(plan, source, max_records),
        pubmed = run_w00_epmc(plan, source, max_records),
        ethos = run_w00_epmc(plan, source, max_records),
        epmc_preprints = run_w00_epmc(plan, source, max_records),
        run_w00_ebsco(plan, source, max_records))
      source_manifest <- file.path(output_dir, "manifest.json")
      if (!file.exists(source_manifest)) stop("Missing source manifest.")
      manifest <- jsonlite::fromJSON(source_manifest, simplifyVector = FALSE)
      if (source == "lens" && !identical(manifest$status, "sample_success")) {
        stop("Lens sample status is not successful.")
      }
      if (source == "openalex" && !identical(manifest$status, "success")) {
        stop("OpenAlex sample status is not successful.")
      }
      count <- if (source == "lens") manifest$unique_records_written
               else if (source == "openalex") manifest$unique_records_exported
               else manifest$records_retrieved
      if (is.null(count) || as.integer(count) > max_records ||
          as.integer(count) < 0L) {
        stop("Invalid sample record count.")
      }
      list(status = "success", records = as.integer(count),
           manifest = file.path("sources", source, "manifest.json"))
    }, error = function(e) {
      list(status = "failed", error = conditionMessage(e))
    })
    elapsed <- as.numeric(difftime(Sys.time(), source_start, units = "secs"))
    if (identical(outcome$status, "success")) {
      log_step("[", i, "/", length(sources), "] Complete: ",
               outcome$records, " records | ", sprintf("%.1f", elapsed), " s")
    } else {
      log_step("[", i, "/", length(sources), "] FAILED after ",
               sprintf("%.1f", elapsed), " s: ", outcome$error)
    }
    results[[source]] <- outcome
    write_summary(if (identical(outcome$status, "failed")) "failed" else "running")
    if (identical(outcome$status, "failed")) {
      stop("W00 ", source, " failed: ", outcome$error,
           ". Existing outputs retained; see ", summary_path, call. = FALSE)
    }
  }
  write_summary("sample_success")
  total <- sum(vapply(results, function(x) x$records, integer(1)))
  elapsed_total <- as.numeric(difftime(Sys.time(), run_start, units = "secs"))
  log_step("Complete | ", length(results), " sources | ", total,
           " records | ", sprintf("%.1f", elapsed_total), " s")
  log_step("Manifest: ", summary_path)
  invisible(list(manifest = summary_path, sources = results))
}
