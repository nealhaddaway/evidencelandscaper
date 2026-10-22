#' Count W00 search hits without harvesting records
#'
#' Reuses the unchanged LivingEvidenceMap/main count-only R implementation.
#' All outputs stay in this map's W00 run. Manual RIS sources are excluded.
#' @param plan Existing plan_w00() result.
#' @return Count CSV path invisibly.
#' @export
scope_w00 <- function(plan) {
  if (!is.list(plan) || !is.character(plan$search_plan) ||
      length(plan$search_plan) != 1L || !file.exists(plan$search_plan) ||
      !is.character(plan$query_archive) ||
      length(plan$query_archive) != 1L || !dir.exists(plan$query_archive)) {
    stop("Supply an existing plan_w00() result.", call. = FALSE)
  }
  p <- jsonlite::fromJSON(plan$search_plan, simplifyVector = FALSE)
  sources <- names(p$source_queries)
  if (!length(sources)) stop("No sources in search plan.", call. = FALSE)
  if (any(sources %in% c("cba", "pqdt"))) {
    stop("CAB Abstracts and PQD&T are manual RIS sources; no API count attempted.",
         call. = FALSE)
  }
  script <- system.file("w00", "workflow_00_search_scope_counts.R",
                        package = "evidencelandscaper")
  lens_config <- system.file("w00", "config/lens_search.json",
                             package = "evidencelandscaper")
  ebsco_config <- system.file("w00", "config/workflow00_ebsco_sources.json",
                              package = "evidencelandscaper")
  if (!nzchar(script) || !nzchar(lens_config) || !nzchar(ebsco_config)) {
    stop("W00 scoping assets unavailable.", call. = FALSE)
  }
  supported <- c("lens", "scopus", "openalex", "agricola", "pubmed",
                 "ethos", "epmc_preprints", "wos",
                 names(jsonlite::fromJSON(ebsco_config,
                                         simplifyVector = FALSE)$sources))
  missing <- setdiff(sources, supported)
  if (length(missing)) {
    stop("Unsupported scoping source(s): ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  run_dir <- dirname(dirname(plan$search_plan))
  out_dir <- file.path(run_dir, "scoping")
  if (file.exists(out_dir) || dir.exists(out_dir)) {
    stop("Scoping output already exists; refusing overwrite: ", out_dir,
         call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  data <- list()
  cmd <- file.path(R.home("bin"), "Rscript")
  for (i in seq_along(sources)) {
    src <- sources[[i]]
    message("[W00 scope] [", i, "/", length(sources), "] Counting ", src, "...")
    csv <- file.path(out_dir, paste0(src, ".csv"))
    json <- file.path(out_dir, paste0(src, ".json"))
    args <- c(script, "--plan", plan$search_plan, "--lens-config",
              lens_config, "--ebsco-config", ebsco_config,
              "--source", src, "--output-csv", csv, "--output-json", json)
    log <- suppressWarnings(system2(cmd,
       args = vapply(args, shQuote, character(1)),
       stdout = TRUE, stderr = TRUE))
    exit <- attr(log, "status")
    if (!is.null(exit) && exit != 0L) {
      stop("W00 scoping script failed for ", src, ":\n",
           paste(log, collapse = "\n"), call. = FALSE)
    }
    if (!file.exists(csv)) {
      stop("No scoping CSV returned for ", src, call. = FALSE)
    }
    row <- utils::read.csv(csv, stringsAsFactors = FALSE)
    if (nrow(row) != 1L || !identical(row$source_slug[[1L]], src)) {
      stop("Unexpected scoping response for ", src, call. = FALSE)
    }
    data[[src]] <- row
    message("[W00 scope] ", src, ": ",
            if (is.na(row$hits[[1L]])) row$status[[1L]] else
              format(row$hits[[1L]], big.mark = ",", scientific = FALSE))
  }
  summary <- do.call(rbind, data)
  rownames(summary) <- NULL
  path <- file.path(out_dir, "counts.csv")
  utils::write.csv(summary, path, row.names = FALSE, na = "")
  jsonlite::write_json(list(schema = "evidencelandscaper-w00-scoping-v1",
    run_id = basename(run_dir), count_only = TRUE,
    retrieved_records = 0L,
    sources = lapply(seq_len(nrow(summary)), function(i)
      as.list(summary[i, , drop = FALSE]))),
    file.path(out_dir, "manifest.json"), pretty = TRUE, auto_unbox = TRUE,
    na = "null")
  message("[W00 scope] Completed ", nrow(summary),
          " source counts. Saved: ", path)
  invisible(path)
}
