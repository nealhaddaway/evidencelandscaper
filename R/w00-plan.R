#' Plan Workflow 00 without performing searches
#'
#' Executes the unchanged source-selection, topic-agnostic translation, and
#' query-archiving scripts from LivingEvidenceMap/main. No network calls,
#' Zenodo writes, or downstream dispatches are performed.
#'
#' @param map Path to an existing evidence map or its configured name.
#' @param strategy Path to a JSON file with named concepts and expression
#'   (or advanced_boolean), as accepted by the upstream W00 validator.
#' @param sources Character vector of database source IDs for a full run.
#' @param mode One of full, fortnightly, expansion.
#' @param expansion_target Concept being expanded (expansion only).
#' @param additional_search_term New search term (expansion only).
#' @return A list containing run directory and search-plan paths.
#' @export
plan_w00 <- function(map, strategy, sources = NULL,
                     mode = c("full", "fortnightly", "expansion"),
                     expansion_target = "", additional_search_term = "") {
  mode <- match.arg(mode)
  map <- open_map(map)
  if (!requireNamespace("jsonlite", quietly = TRUE) ||
      !requireNamespace("digest", quietly = TRUE)) {
    stop("Install required packages jsonlite and digest.", call. = FALSE)
  }
  strategy <- normalizePath(path.expand(strategy), winslash = "/", mustWork = TRUE)
  if (mode == "full") {
    if (!is.character(sources) || !length(sources) ||
        anyNA(sources) || anyDuplicated(sources) || any(!nzchar(sources))) {
      stop("Supply unique source IDs for a full run.", call. = FALSE)
    }
  } else if (!is.null(sources)) {
    stop("Updates reuse the saved full-run source selection; omit sources.", call. = FALSE)
  }
  if (mode == "expansion" &&
      (!nzchar(expansion_target) || !nzchar(additional_search_term))) {
    stop("Expansion requires target and additional search term.", call. = FALSE)
  }
  asset <- function(name) {
    path <- system.file("w00", name, package = "evidencelandscaper")
    if (!nzchar(path)) stop("Missing packaged W00 asset: ", name, call. = FALSE)
    path
  }
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
                   "-", substr(basename(tempfile()), 1L, 12L))
  run_dir <- file.path(map, "runs", "W00", run_id)
  if (!dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create W00 run directory: ", run_dir, call. = FALSE)
  }
  plan_dir <- file.path(run_dir, "plan")
  dir.create(plan_dir)
  saved <- file.path(map, "data", "workflow00", "source_selection.json")
  selection <- file.path(plan_dir, "source_selection.json")
  local_strategy <- file.path(run_dir, "source_strategy.json")
  if (!file.copy(strategy, local_strategy)) stop("Could not snapshot strategy.", call. = FALSE)
  execute <- function(script, arguments, env = character()) {
    cmd <- file.path(R.home("bin"), "Rscript")
    old <- Sys.getenv(names(env), unset = NA_character_)
    on.exit({
      for (i in seq_along(env)) {
        if (is.na(old[[i]])) Sys.unsetenv(names(env)[[i]])
        else do.call(Sys.setenv, setNames(list(old[[i]]), names(env)[[i]]))
      }
    }, add = TRUE)
    if (length(env)) do.call(Sys.setenv, as.list(env))
    output <- suppressWarnings(system2(cmd,
        args = c(shQuote(asset(script)), vapply(arguments, shQuote, character(1))),
        stdout = TRUE, stderr = TRUE))
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L) {
      stop("W00 ", script, " failed:\n", paste(output, collapse = "\n"),
           call. = FALSE)
    }
    invisible(output)
  }
  execute("workflow_00_resolve_source_selection.R",
          c("--run-type", mode, "--saved", saved, "--output", selection,
            "--group-config", asset("config/workflow00_source_groups.json"),
            "--ebsco-config", asset("config/workflow00_ebsco_sources.json")),
          if (mode == "full") c(W00_ADDITIONAL_SOURCES = paste(sources, collapse = ","),
                                W00_EXCLUDE_SOURCES = "",
                                GITHUB_RUN_ID = run_id) else character())
  execute("workflow_00_validate_search_strategy.R",
          c("--config", local_strategy))
  execute("workflow_00_translate_generic.R",
          c("--config", local_strategy, "--output-dir", plan_dir,
            "--run-type", mode, "--source-selection", selection,
            "--ebsco-config", asset("config/workflow00_ebsco_sources.json"),
            "--expansion-target", expansion_target,
            "--additional-search-term", additional_search_term))
  execute("workflow_00_archive_queries.R",
          c("--plan", file.path(plan_dir, "search_plan.json"),
            "--strategy", local_strategy,
            "--archive-root", file.path(run_dir, "search_archive"),
            "--run-id", run_id))
  if (mode == "full") {
    dir.create(dirname(saved), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(selection, saved, overwrite = TRUE)) {
      stop("Could not save full-run database selection.", call. = FALSE)
    }
  }
  list(run_dir = run_dir, search_plan = file.path(plan_dir, "search_plan.json"),
       source_selection = selection,
       query_archive = file.path(run_dir, "search_archive", paste0("run-", run_id)))
}
