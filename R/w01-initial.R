#' Run original W01-main first-run union and candidate preparation
#'
#' This deliberately stops before duplicate classification, human adjudication,
#' clustering, and canonical publication. No W01-fast or GitHub actions.
#' @param source_manifest Output from prepare_w01().
#' @return W01 candidate directory, invisibly.
#' @export
run_w01_initial <- function(source_manifest) {
  if (!is.character(source_manifest) || length(source_manifest) != 1L ||
      is.na(source_manifest) || !file.exists(source_manifest))
    stop("Pass the source_inputs.json output from prepare_w01().", call. = FALSE)
  m <- jsonlite::fromJSON(source_manifest, simplifyVector = FALSE)
  if (!identical(m$schema, "living-evidence-map-workflow01-source-inputs-v1") ||
      !identical(m$status, "validated") || !length(m$sources))
    stop("W01 source input manifest has not been validated.", call. = FALSE)
  paths <- vapply(m$sources, as.character, character(1))
  if (any(!file.exists(paths)))
    stop("W01 source input is missing.", call. = FALSE)
  root <- dirname(source_manifest)
  union <- file.path(root, "union")
  candidates <- file.path(root, "candidates")
  if (dir.exists(union) || dir.exists(candidates)) {
    stop("W01 initial output already exists; refusing overwrite.", call. = FALSE)
  }
  empty <- file.path(root, "empty_prior_state")
  if (dir.exists(empty) && length(list.files(empty, all.files = TRUE,
                                             no.. = TRUE)))
    stop("First-run prior state is not empty.", call. = FALSE)
  dir.create(empty, showWarnings = FALSE)
  script <- function(name) {
    f <- system.file("w01", name, package = "evidencelandscaper")
    if (!nzchar(f)) stop("Missing original W01 main script: ", name)
    f
  }
  execute <- function(name, args) {
    result <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      args = vapply(c(script(name), args), shQuote, character(1)),
      stdout = TRUE, stderr = TRUE))
    exit <- attr(result, "status")
    if (!is.null(exit) && exit != 0L)
      stop("W01 main ", name, " failed: ",
           paste(tail(result, 30L), collapse = "\n"), call. = FALSE)
  }
  execute("workflow_01_deduplication_build_union.R",
    c("--first-run", "true", "--old-dir", empty,
      "--input-manifest", source_manifest,
      "--expected-prior-manifestations", "0", "--output-dir", union))
  union_manifest <- file.path(union, "source_inputs.json")
  if (!file.exists(union_manifest))
    stop("W01 union did not return a source manifest.", call. = FALSE)
  execute("workflow_01_deduplication_incremental_candidates.R",
    c("--first-run", "true", "--input-manifest", union_manifest,
      "--output-dir", candidates, "--score-n", "1"))
  message("[W01] PASS: original first-run union and candidate preparation.")
  message("[W01] Not yet classified, adjudicated, clustered or deduplicated.")
  invisible(candidates)
}
