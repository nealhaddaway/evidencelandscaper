#' Prepare W01 main source inputs from completed local W00 consolidation
#'
#' Converts each authoritative W00 source via the original W01-main adapters,
#' with explicit count and identity validation. Never modifies W00 outputs,
#' submits API requests, or starts deduplication.
#' @param map Evidence map name/path.
#' @param w00_run Optional explicit completed W00 run directory. Prevents
#'   selecting a different run when several expansions share one map.
#' @return W01 source-input manifest path, invisibly.
#' @export
prepare_w01 <- function(map,w00_run=NULL) {
  map <- open_map(map)
  root <- file.path(map, "runs", "W00")
  runs <- if (dir.exists(root)) list.dirs(root, recursive = FALSE,
                                         full.names = TRUE) else character()
  if(!is.null(w00_run)) {
    run <- normalizePath(w00_run,winslash="/",mustWork=TRUE)
    if(!identical(dirname(run),normalizePath(root,winslash="/",mustWork=TRUE)))
      stop("Selected W00 run is not inside this map.")
    parent <- file.path(run,"w00_full_retrieval_manifest.json")
    parent_state<-if(file.exists(parent))
      jsonlite::fromJSON(parent,simplifyVector=FALSE) else NULL
    approved<-file.path(run,"approved_partial_retrieval.json")
    partial_ok<-!is.null(parent_state) &&
      identical(parent_state$status,"incomplete") &&
      file.exists(approved) &&
      identical(jsonlite::fromJSON(approved,simplifyVector=FALSE)$original_manifest_sha256,
                digest::digest(file=parent,algo="sha256",serialize=FALSE))
    if(is.null(parent_state) ||
       !(identical(parent_state$status,"success")||partial_ok))
      stop("Explicit W00 retrieval is neither complete nor verified as approved partial.")
  } else {
    candidates <- character()
    for (candidate in runs) {
      parent <- file.path(candidate,"w00_full_retrieval_manifest.json")
      if(!file.exists(parent))next
      obj <- tryCatch(jsonlite::fromJSON(parent,simplifyVector=FALSE),error=function(e)NULL)
      if(!is.null(obj)&&identical(obj$status,"success"))candidates<-c(candidates,candidate)
    }
    if(length(candidates)!=1L)
      stop("Multiple or no W00 full runs; pass w00_run explicitly.")
    run <- candidates[[1L]]
  }
  consolidated <- file.path(run, "consolidated")
  revision <- file.path(run, "accepted_manual_imports.json")
  if(file.exists(revision)) {
    revision_sha<-digest::digest(file=revision,algo="sha256",serialize=FALSE)
    base_mf<-file.path(consolidated,"manifest.json")
    base<-if(file.exists(base_mf))
      jsonlite::fromJSON(base_mf,simplifyVector=FALSE) else NULL
    if(is.null(base)||!identical(base$manual_registry_sha256,revision_sha))
      consolidated<-file.path(run,paste0("consolidated-manual-",substr(revision_sha,1L,12L)))
  }
  cm <- file.path(consolidated, "manifest.json")
  if (!file.exists(cm)) stop("Consolidate the full W00 run first.", call. = FALSE)
  meta <- jsonlite::fromJSON(cm, simplifyVector = FALSE)
  data_file <- file.path(consolidated, "records.jsonl")
  if (!identical(meta$status, "success") ||
      !identical(meta$records_sha256,
                 digest::digest(file = data_file, algo = "sha256",
                                serialize = FALSE))) {
    stop("Consolidated W00 checksum or status invalid.", call. = FALSE)
  }
  source_names <- names(meta$sources)
  if (!length(source_names) || anyDuplicated(source_names) ||
      sum(vapply(meta$sources, function(x) as.integer(x$records),
                 integer(1))) != meta$record_count)
    stop("W00 source-count reconciliation failed.", call. = FALSE)
  dest <- file.path(map, "runs", "W01", paste0("prepare-", meta$run_id,
                         "-", substr(meta$records_sha256, 1L, 12L)))
  if (file.exists(file.path(dest, "source_inputs.json"))) {
    stop("Completed W01 preparation already exists: ",
         file.path(dest, "source_inputs.json"), call. = FALSE)
  }
  asset <- function(name) {
    p <- system.file("w01", name, package = "evidencelandscaper")
    if (!nzchar(p)) stop("Missing W01 main adapter: ", name, call. = FALSE)
    p
  }
  run_script <- function(name, args) {
    res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      args = vapply(c(asset(name), args), shQuote, character(1)),
      stdout = TRUE, stderr = TRUE))
    code <- attr(res, "status")
    if (!is.null(code) && code != 0L)
      stop("W01 main adapter ", name, " failed: ",
           paste(tail(res, 20L), collapse = "\n"), call. = FALSE)
  }
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  paths <- list()
  validate_output <- function(output, src, expected) {
    lines <- readLines(output, warn = FALSE, encoding = "UTF-8")
    lines <- lines[nzchar(trimws(lines))]
    if (length(lines) != expected)
      stop("W01 source count mismatch: ", src, " expected ", expected,
           " got ", length(lines), call. = FALSE)
    ids <- vapply(lines, function(line) {
      obj <- jsonlite::fromJSON(line, simplifyVector = FALSE)
      if (src == "lens") obj$identity$lens_id else
        obj$sidecar_identity$sidecar_record_id
    }, character(1))
    if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))
      stop("W01 source IDs missing or duplicated: ", src, call. = FALSE)
    invisible(TRUE)
  }
  for (src in source_names) {
    expected <- as.integer(meta$sources[[src]]$records)
    input <- file.path(run, "sources", src)
    outdir <- file.path(dest, "adapters", src)
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    output <- file.path(dest, paste0(src, ".jsonl"))
    if (file.exists(output)) {
      validate_output(output, src, expected)
      paths[[src]] <- output
      message("[W01 prepare] Reused verified ", src, ": ", expected, " records")
      next
    }
    if (expected == 0L) {
      file.create(output)
    } else if (src == "lens" || startsWith(src, "ebsco_")) {
      original <- file.path(input, "records.jsonl")
      if (!file.copy(original, output))
        stop("Could not copy source records: ", src, call. = FALSE)
    } else if (src %in% c("scopus", "wos", "openalex")) {
      script <- switch(src,
        scopus = "workflow_01s_scopus_sidecar_adapter.R",
        wos = "workflow_01w_wos_starter_sidecar_adapter.R",
        openalex = "workflow_01u_openalex_sidecar_adapter.R")
      original <- file.path(outdir, if (src == "wos")
                            "wos_starter_sidecar_records.jsonl" else
                            paste0(src, "_sidecar_records.jsonl"))
      if (!file.exists(original)) run_script(script,
        c("--input-dir", input, "--output-dir", outdir))
      if (!file.copy(original, output))
        stop("No adapted JSONL produced for ", src, call. = FALSE)
    } else if (src %in% c("agricola", "pubmed", "ethos", "epmc_preprints")) {
      code <- c(agricola = "AGR", pubmed = "MED", ethos = "ETH",
                epmc_preprints = "PPR")[[src]]
      run_script("workflow_01x_europe_pmc_sidecar_adapter.R",
        c("--input-dir", input, "--output-dir", outdir,
          "--source-code", code, "--source-slug", src,
          "--source-collection", src))
      original <- file.path(outdir, paste0(src, "_sidecar_records.jsonl"))
      if (!file.copy(original, output))
        stop("No adapted JSONL produced for ", src, call. = FALSE)
    } else if (src %in% c("cba", "pqdt")) {
      registry <- jsonlite::fromJSON(revision, simplifyVector = FALSE)
      entry <- registry$imports[[src]]
      if (is.null(entry)) stop("Missing manual import registry: ", src)
      imported <- jsonlite::fromJSON(file.path(entry$import_directory,
                                     "manifest.json"), simplifyVector = FALSE)
      raw_root <- file.path(outdir, "original_ris")
      dir.create(raw_root)
      for (f in imported$chunks$files) {
        original <- file.path(entry$import_directory, f$path)
        if (!identical(digest::digest(file = original, algo = "sha256",
                                      serialize = FALSE), f$sha256))
          stop("Manual RIS checksum mismatch: ", src)
        dest_file <- file.path(raw_root, basename(original))
        if (file.exists(dest_file)) stop("Duplicate RIS filename: ", src)
        if (!file.copy(original, dest_file))
          stop("Could not copy original RIS: ", src)
      }
      run_script("workflow_01_generic_ris_to_sidecar.R",
        c("--input-dir", raw_root, "--source", src,
          "--submission-id", basename(dirname(entry$import_directory)),
          "--output", output))
    } else stop("W01 main does not support source: ", src, call. = FALSE)
    validate_output(output, src, expected)
    paths[[src]] <- output
    message("[W01 prepare] ", src, ": ", expected, " records")
  }
  if (sum(vapply(paths, function(p) length(readLines(p, warn = FALSE)),
                 integer(1))) != meta$record_count)
    stop("W01 total differs from W00 consolidation.", call. = FALSE)
  out <- file.path(dest, "source_inputs.json")
  jsonlite::write_json(list(
    schema = "living-evidence-map-workflow01-source-inputs-v1",
    status = "validated",
    source_w00_run = meta$run_id,
    consolidated_sha256 = meta$records_sha256,
    sources = paths), out, auto_unbox = TRUE, pretty = TRUE)
  message("[W01 prepare] PASS: ", meta$record_count,
          " manifestations from ", length(paths), " sources.")
  invisible(out)
}
