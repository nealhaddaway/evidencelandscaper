#' Run a complete local W00 search
#'
#' Runs the original source-specific R harvesters with no 100-record sample
#' limit. A fresh immutable W00 plan is created when given a map name.
#' The original source scripts preserve API responses and checkpoints; this
#' function does not submit downstream workflows or write to Zenodo.
#' Existing completed source outputs are never overwritten.
#' @param map A map name/path or a plan_w00() result.
#' @return Path to the W00 full-retrieval manifest (invisibly).
#' @export
run_w00_full <- function(map) {
  plan <- if (is.character(map) && length(map) == 1L && !is.na(map)) {
    plan_w00(map)
  } else map
  if (!is.list(plan) || !file.exists(plan$search_plan) ||
      !dir.exists(plan$query_archive)) {
    stop("Supply a valid map name or W00 plan.", call. = FALSE)
  }
  cfg <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  if (!identical(cfg$run_type, "full")) stop("A full W00 plan is required.", call. = FALSE)
  sources <- names(cfg$source_queries)
  if (!length(sources) || anyDuplicated(sources)) stop("Invalid source list.", call. = FALSE)
  eb_file <- system.file("w00", "config/workflow00_ebsco_sources.json",
                         package = "evidencelandscaper")
  eb <- jsonlite::fromJSON(eb_file, simplifyVector = FALSE)$sources
  supported <- c("lens", "openalex", "scopus", "wos", "agricola",
                 "pubmed", "ethos", "epmc_preprints", names(eb))
  unknown <- setdiff(sources, supported)
  if (length(unknown)) stop("Unsupported full retrieval sources: ",
                            paste(unknown, collapse = ", "), call. = FALSE)
  credentials <- list(
    lens = "LENS_API_TOKEN",
    openalex = c("OPENALEX_API_KEY", "OPENALEX_API_TOKEN"),
    scopus = c("SCOPUS_API_TOKEN", "SCOPUS_INSTTOKEN"),
    wos = "WOS_STARTER_API",
    ebsco = c("EBSCO_EHOST_UID", "EBSCO_EHOST_PWD")
  )
  missing <- character()
  if ("lens" %in% sources && !nzchar(Sys.getenv(credentials$lens)))
    missing <- c(missing, credentials$lens)
  if ("openalex" %in% sources &&
      !any(nzchar(Sys.getenv(credentials$openalex))))
    missing <- c(missing, "OPENALEX_API_KEY or OPENALEX_API_TOKEN")
  if ("scopus" %in% sources) {
    missing <- c(missing, credentials$scopus[!nzchar(Sys.getenv(credentials$scopus))])
  }
  if ("wos" %in% sources && !nzchar(Sys.getenv(credentials$wos)))
    missing <- c(missing, credentials$wos)
  if (any(sources %in% names(eb))) {
    missing <- c(missing, credentials$ebsco[!nzchar(Sys.getenv(credentials$ebsco))])
  }
  if (length(missing)) stop("Missing W00 credentials: ",
                            paste(unique(missing), collapse = ", "),
                            ". No retrieval started.", call. = FALSE)
  run_dir <- normalizePath(dirname(dirname(plan$search_plan)),
                           winslash = "/", mustWork = TRUE)
  manifest_file <- file.path(run_dir, "w00_full_retrieval_manifest.json")
  if (file.exists(manifest_file)) {
    stop("W00 full retrieval manifest exists, refusing to overwrite: ",
         manifest_file, call. = FALSE)
  }
  existing <- sources[vapply(sources, function(x)
    dir.exists(file.path(run_dir, "sources", x)), logical(1))]
  if (length(existing)) stop("Existing W00 source outputs for ",
                             paste(existing, collapse = ", "),
                             ". Use a fresh plan.", call. = FALSE)
  archive <- jsonlite::fromJSON(file.path(plan$query_archive, "manifest.json"),
                                simplifyVector = FALSE)
  asset <- function(name) {
    path <- system.file("w00", name, package = "evidencelandscaper")
    if (!nzchar(path)) stop("Missing W00 package asset: ", name, call. = FALSE)
    path
  }
  execute <- function(script, args, source) {
    output <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      args = vapply(c(asset(script), args), shQuote, character(1)),
      stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L)
      stop(source, " harvester failed:\n", paste(tail(output, 35L),
             collapse = "\n"), call. = FALSE)
    invisible(output)
  }
  results <- list()
  started <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  save_manifest <- function(status) {
    out <- list(schema = "evidencelandscaper-w00-full-retrieval-v1",
                status = status, mode = "full", run_id = basename(run_dir),
                started_at_utc = started,
                updated_at_utc = format(Sys.time(), tz = "UTC",
                                        format = "%Y-%m-%dT%H:%M:%SZ"),
                plan_file = "plan/search_plan.json",
                records_not_deduplicated = TRUE,
                sources = results)
    jsonlite::write_json(out, manifest_file, pretty = TRUE,
                         auto_unbox = TRUE, null = "null")
  }
  save_manifest("running")
  for (i in seq_along(sources)) {
    src <- sources[[i]]
    message("[W00 full] [", i, "/", length(sources), "] ", src)
    dest <- file.path(run_dir, "sources", src)
    dir.create(dest, showWarnings = FALSE, recursive = TRUE)
    result <- tryCatch({
      entry <- archive$queries[[src]]
      q_file <- file.path(plan$query_archive, paste0(src, ".txt"))
      if (is.null(entry) || !file.exists(q_file) ||
          !identical(tolower(digest::digest(file = q_file, algo = "sha256",
                                            serialize = FALSE)),
                     tolower(entry$sha256))) {
        stop("Archived source query checksum invalid.")
      }
      q <- paste(readLines(q_file, warn = FALSE, encoding = "UTF-8"),
                 collapse = "\n")
      if (!identical(q, cfg$source_queries[[src]]) ||
          !identical(tolower(digest::digest(q, algo = "sha256",
                                            serialize = FALSE)),
                     tolower(entry$query_string_sha256)))
        stop("Archived query does not match plan.")
      if (src == "lens") {
        config <- file.path(dest, "lens_config.json")
        execute("workflow_00_build_lens_config.R",
                c("--query", q, "--output", config), src)
        execute("workflow_00_lens_orchestrated_search.R",
                c("--config", config, "--max-records", "all",
                  "--output", file.path(dest, "records.jsonl"),
                  "--manifest", file.path(dest, "manifest.json"),
                  "--audit-dir", file.path(dest, "audit")), src)
      } else if (src == "openalex") {
        execute("workflow_00c_openalex_ingestion.R",
                c("--query", q, "--max-records", "all",
                  "--page-size", "100", "--output-dir", dest), src)
      } else if (src == "scopus") {
        execute("workflow_00b_scopus_ingestion.R",
                c("--query", q, "--page-size", "200",
                  "--output-dir", dest), src)
      } else if (src == "wos") {
        execute("workflow_00e_wos_starter_ingestion.R",
                c("--query", q, "--page-size", "50",
                  "--output-dir", dest), src)
      } else if (src %in% names(eb)) {
        execute("workflow_00g_ebsco_ingestion.R",
                c("--source", src, "--query", q, "--config", eb_file,
                  "--output-dir", dest, "--max-records", "all",
                  "--page-size", "100"), src)
      } else {
        script <- if (src == "agricola")
          "workflow_00d_agricola_ingestion.R" else
          "workflow_00f_europe_pmc_source_ingestion.R"
        args <- c("--query", q, "--max-records", "all",
                  "--page-size", "100", "--output-dir", dest)
        if (src != "agricola") {
          code <- c(pubmed = "MED", ethos = "ETH",
                    epmc_preprints = "PPR")[[src]]
          args <- c(args, "--source-code", code,
                    "--source-slug", src, "--source-collection", src)
        }
        execute(script, args, src)
      }
      mf <- file.path(dest, "manifest.json")
      if (!file.exists(mf)) stop("Harvester did not write manifest.")
      meta <- jsonlite::fromJSON(mf, simplifyVector = FALSE)
      if (!identical(meta$status, "success"))
        stop("Harvester manifest does not report success.")
      count <- if (src == "lens") meta$unique_records_written else
        if (src == "openalex") meta$unique_records_exported else
        if (src == "scopus") meta$entries_retrieved else
        meta$records_retrieved
      if (length(count) != 1L || is.null(count) || is.na(count) ||
          !is.numeric(count) || count < 0)
        stop("Invalid retrieved record count in source manifest.")
      list(status = "success", records = as.integer(count),
           manifest = paste0("sources/", src, "/manifest.json"))
    }, error = function(e) list(status = "failed",
                               error = conditionMessage(e)))
    results[[src]] <- result
    save_manifest(if (result$status == "success") "running" else "failed")
    if (result$status != "success") {
      stop("W00 full retrieval failed at ", src, ": ", result$error,
           ". Output retained for diagnosis; no subsequent databases run.",
           call. = FALSE)
    }
    message("[W00 full] ", src, ": ", result$records, " records")
  }
  save_manifest("success")
  message("[W00 full] All ", length(sources), " source harvests completed.")
  invisible(manifest_file)
}
