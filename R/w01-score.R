#' Score all W01-main duplicate candidate pairs
#'
#' Reuses the original W01-main expanded scoring algorithm and existing
#' candidate artefact. Does not regenerate candidates or merge records.
#' @param candidates Directory returned by run_w01_initial().
#' @return Full scoring output directory, invisibly.
#' @export
score_w01_candidates <- function(candidates) {
  if (!is.character(candidates) || length(candidates) != 1L ||
      is.na(candidates) || !dir.exists(candidates))
    stop("Supply the directory returned by run_w01_initial().", call. = FALSE)
  candidates <- normalizePath(candidates, winslash = "/", mustWork = TRUE)
  if (basename(candidates) != "candidates")
    stop("Expected an existing W01 candidates directory.", call. = FALSE)
  summary_file <- file.path(candidates, "summary.json")
  if (!file.exists(summary_file))
    stop("W01 candidate summary missing.", call. = FALSE)
  summary <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
  if (!identical(summary$status, "success"))
    stop("W01 candidate generation is incomplete.", call. = FALSE)
  n <- as.integer(summary$total_candidate_pairs)
  if (length(n) != 1L || is.na(n) || n < 0L)
    stop("Invalid candidate pair count.", call. = FALSE)
  output <- file.path(dirname(candidates), "full_scoring")
  if (dir.exists(output) || file.exists(output))
    stop("W01 full scoring output exists; refusing overwrite: ",
         output, call. = FALSE)
  script <- system.file("w01", "workflow_01_deduplication_incremental_score.R",
                        package = "evidencelandscaper")
  if (!nzchar(script)) stop("W01-main scoring script not packaged.", call. = FALSE)
  # The original score script requires target-n >= 2000 and automatically
  # caps its deterministic scored selection to all available candidate pairs.
  args <- c(script, "--input-dir", candidates,
            "--upstream-dir", file.path(dirname(candidates), "union"),
            "--output-dir", output, "--target-n",
            as.character(max(2000L, n)))
  message("[W01] Stage 3/3: scoring ", format(n,big.mark=","), " candidate pairs...")
  result <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      args = vapply(args, shQuote, character(1)),
      stdout = TRUE, stderr = TRUE))
  code <- attr(result, "status")
  if (!is.null(code) && code != 0L)
    stop("W01-main scoring failed: ",
         paste(tail(result, 35L), collapse = "\n"), call. = FALSE)
  file <- file.path(output, "expanded_summary.json")
  if (!file.exists(file)) stop("W01 scoring summary missing.", call. = FALSE)
  x <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  if (!identical(x$status, "success") ||
      as.integer(x$total_candidate_pairs) != n ||
      as.integer(x$scoring_sample_n) != n)
    stop("W01 scoring incomplete: scored pair count differs from candidates.",
         call. = FALSE)
  message("[W01] Scored all ", n, " candidate pairs using W01-main rules.")
  message("[W01] No duplicate clusters finalised and no adjudication performed.")
  invisible(output)
}
