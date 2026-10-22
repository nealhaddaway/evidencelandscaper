#' Accept specific manual RIS imports for one W00 run
#'
#' Explicitly selects previously completed local RIS imports. Only selected
#' imports appear in this run's acceptance registry. Does not modify imports,
#' retrieve data, combine records, or dispatch W01.
#' @param plan Result of plan_w00() for the target run.
#' @param imports Optional named character vector of import output directories.\n#'   If omitted, discovers successful imports; ambiguity requires explicit choice.\n#' @param sources Source IDs to auto-discover, defaults to cba and pqdt.
#' @return Path to accepted_manual_imports.json, invisibly.
#' @export
accept_w00_ris <- function(plan, imports = NULL, sources = c("cba", "pqdt")) {
  if (!is.list(plan) || length(plan$search_plan) != 1L ||
      !file.exists(plan$search_plan) || length(plan$query_archive) != 1L ||
      !dir.exists(plan$query_archive)) {
    stop("Supply an existing plan_w00() result.", call. = FALSE)
  }
  if (is.null(imports)) {
    if (!is.character(sources) || !length(sources) || anyNA(sources) ||
        anyDuplicated(sources) ||
        any(!grepl("^[a-z0-9][a-z0-9_-]*$", sources))) {
      stop("sources must be unique, valid source IDs.", call. = FALSE)
    }
    run_dir0 <- normalizePath(dirname(dirname(plan$search_plan)),
                              winslash = "/", mustWork = TRUE)
    map0 <- normalizePath(file.path(run_dir0, "..", "..", ".."),
                          winslash = "/", mustWork = TRUE)
    root0 <- file.path(map0, "runs", "W00", "manual")
    dirs <- if (dir.exists(root0)) list.dirs(root0, recursive = FALSE,
                                              full.names = TRUE) else character()
    found <- list()
    for (source in sources) {
      candidates <- character()
      for (d in dirs) {
        mf <- file.path(d, "import", "manifest.json")
        if (!file.exists(mf)) next
        meta <- tryCatch(jsonlite::fromJSON(mf, simplifyVector = FALSE),
                         error = function(e) NULL)
        if (!is.null(meta) && identical(meta$source, source) &&
            identical(meta$status, "success")) {
          candidates <- c(candidates, file.path(d, "import"))
        }
      }
      if (!length(candidates)) {
        stop("No successful RIS import found for ", source, call. = FALSE)
      }
      if (length(candidates) > 1L) {
        stop("Multiple successful RIS imports found for ", source,
             "; select the intended import explicitly using imports=. Candidates: ",
             paste(candidates, collapse = "; "), call. = FALSE)
      }
      found[[source]] <- candidates[[1L]]
    }
    imports <- unlist(found, use.names = TRUE)
  }
  if (!is.character(imports) || !length(imports) ||
      is.null(names(imports)) || anyNA(imports) ||
      anyNA(names(imports)) || any(!nzchar(imports)) ||
      any(!grepl("^[a-z0-9][a-z0-9_-]*$", names(imports))) ||
      anyDuplicated(names(imports)) || anyDuplicated(imports)) {
    stop("imports must be a named vector of unique source IDs and directories.",
         call. = FALSE)
  }
  run_dir <- normalizePath(dirname(dirname(plan$search_plan)),
                           winslash = "/", mustWork = TRUE)
  map <- normalizePath(file.path(run_dir, "..", "..", ".."),
                       winslash = "/", mustWork = TRUE)
  manual_root <- file.path(map, "runs", "W00", "manual")
  registry_path <- file.path(run_dir, "accepted_manual_imports.json")
  if (file.exists(registry_path)) {
    stop("Registry already exists; refusing to overwrite: ", registry_path,
         call. = FALSE)
  }
  plan_data <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  # Source selection may include manually supplied databases in future; ensure
  # there can be no collision with an already executed API source.
  automated <- names(plan_data$source_queries)
  if (any(names(imports) %in% setdiff(automated, c("cba", "pqdt")))) {
    stop("Manual import collides with an automated planned source.", call. = FALSE)
  }
  hash <- function(x) digest::digest(file = x, algo = "sha256", serialize = FALSE)
  entries <- lapply(seq_along(imports), function(i) {
    source <- names(imports)[[i]]
    path <- normalizePath(imports[[i]], winslash = "/", mustWork = TRUE)
    if (!dir.exists(path) || !startsWith(paste0(path, "/"),
                                          paste0(normalizePath(manual_root,
                                            winslash = "/", mustWork = TRUE), "/"))) {
      stop("Import must be inside this map's W00/manual directory: ", path,
           call. = FALSE)
    }
    mf <- file.path(path, "manifest.json")
    handoff <- file.path(path, "handoff", "records.jsonl")
    if (!file.exists(mf) || !file.exists(handoff)) {
      stop("Missing import manifest or handoff: ", path, call. = FALSE)
    }
    manifest <- jsonlite::fromJSON(mf, simplifyVector = FALSE)
    if (!identical(manifest$status, "success") ||
        !identical(manifest$source, source) ||
        !identical(manifest$acquisition$method, "ris_upload")) {
      stop("Invalid import status, source or acquisition for ", source,
           call. = FALSE)
    }
    expected <- manifest$files$handoff_jsonl$sha256
    if (!identical(tolower(hash(handoff)), tolower(expected))) {
      stop("RIS handoff checksum mismatch for ", source, call. = FALSE)
    }
    raw <- manifest$chunks$files
    if (!length(raw)) stop("No raw RIS files for ", source, call. = FALSE)
    for (f in raw) {
      candidate <- file.path(path, f$path)
      if (!file.exists(candidate) ||
          !identical(tolower(hash(candidate)), tolower(f$sha256))) {
        stop("Raw RIS checksum mismatch for ", source, ": ", f$path,
             call. = FALSE)
      }
    }
    list(source = source, import_directory = path, manifest_sha256 = hash(mf),
         handoff_sha256 = hash(handoff),
         records = manifest$records$unique_records_for_handover,
         raw_files = length(raw))
  })
  names(entries) <- names(imports)
  registry <- list(schema = "evidencelandscaper-w00-manual-acceptance-v1",
                   run_id = basename(run_dir),
                   created_at_utc = format(Sys.time(), tz = "UTC",
                                          format = "%Y-%m-%dT%H:%M:%SZ"),
                   imports = entries)
  # A new registry is created only after every selected import validates.
  jsonlite::write_json(registry, registry_path, auto_unbox = TRUE,
                       pretty = TRUE)
  invisible(registry_path)
}
