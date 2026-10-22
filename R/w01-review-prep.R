#' Prepare W01-main reviewed cases after deterministic classification
#'
#' Applies the original identifier-rescoring script to the already scored
#' candidate pairs, then builds a source-identity-validated full-bibliography
#' queue. Does not invoke the LLM, adjudicate, or cluster automatically.
#' @param scored Directory returned by score_w01_candidates().
#' @return Path to W01 adjudication JSONL, invisibly.
#' @export
prepare_w01_review <- function(scored) {
  if (!dir.exists(scored)) stop("W01 scored directory does not exist.")
  scored <- normalizePath(scored, winslash="/", mustWork=TRUE)
  if (!file.exists(file.path(scored,"expanded_summary.json")) ||
      !file.exists(file.path(scored,"scored_sample.csv")))
    stop("Full W01 scoring artefacts are missing.")
  parent <- dirname(scored)
  union <- file.path(parent,"union")
  if (!file.exists(file.path(union,"source_inputs.json")))
    stop("W01 source union missing.")
  destination <- file.path(parent,"review_preparation")
  if (dir.exists(destination))
    stop("Review preparation already exists; refusing overwrite: ",
         destination,call.=FALSE)
  asset <- function(name) {
    p <- system.file("w01",name,package="evidencelandscaper")
    if(!nzchar(p)) stop("Missing original W01 script: ",name)
    p
  }
  execute <- function(name,args) {
    o<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
      args=vapply(c(asset(name),args),shQuote,character(1)),
      stdout=TRUE,stderr=TRUE))
    status<-attr(o,"status")
    if(!is.null(status) && status!=0L)
      stop("W01 ",name," failed:\n",paste(tail(o,25L),collapse="\n"),
           call.=FALSE)
  }
  dir.create(destination,recursive=TRUE)
  rescored<-file.path(destination,"rescored")
  execute("workflow_01_deduplication_identifier_rescore.R",
      c("--input-dir",scored,"--output-dir",rescored))
  csv<-file.path(rescored,"rescored_sample.csv")
  metadata<-file.path(scored,"normalised_metadata.csv")
  if(!file.exists(csv)||!file.exists(metadata))
    stop("Identifier rescoring outputs missing.")
  pairs<-utils::read.csv(csv,stringsAsFactors=FALSE,check.names=FALSE)
  if(!all(c("record_i","record_j","rescored_classification",
            "rescored_rule","title_similarity","blocks") %in% names(pairs)))
    stop("W01 rescored CSV missing required fields.")
  expected<-jsonlite::fromJSON(file.path(scored,"expanded_summary.json"))$scoring_sample_n
  if(nrow(pairs)!=expected)
    stop("Identifier rescoring dropped candidate pairs.")
  # Stable indices are only accepted against exactly the metadata/union
  # snapshot that yielded the already validated scoring stage.
  if(!"stable_pair_key" %in% names(pairs) ||
     anyNA(pairs$stable_pair_key) ||
     any(!grepl("^[0-9a-f]{64}$",pairs$stable_pair_key)))
    stop("W01 stable source-identity pair keys missing or malformed.")
  pairs$pair_key<-pairs$stable_pair_key
  if(anyDuplicated(pairs$pair_key))
    stop("Repeated candidate-pair identity after rescoring.")
  review<-pairs[pairs$rescored_classification %in%
                  c("review","unresolved"),,drop=FALSE]
  path<-file.path(destination,"human_review_pairs.csv")
  utils::write.csv(review,path,row.names=FALSE,na="")
  queue<-file.path(destination,"w01_review_cases.jsonl")
  if(nrow(review)) {
    execute("workflow_01_build_duplicate_adjudication_cases.R",
      c("--review-pairs",path,"--metadata",metadata,
        "--union-dir",union,"--output",queue))
  } else {
    file.create(queue)
  }
  cases<-readLines(queue,warn=FALSE,encoding="UTF-8")
  cases<-cases[nzchar(trimws(cases))]
  if(length(cases)!=nrow(review)) stop("W01 review case count mismatch.")
  jsonlite::write_json(list(schema="local-w01-review-preparation-v1",
    status="success",total_pairs=nrow(pairs),review_pairs=nrow(review),
    queue_sha256=digest::digest(file=queue,algo="sha256",serialize=FALSE),
    original_w01_scripts=TRUE,llm_completed=FALSE,
    final_clustering_completed=FALSE),
    file.path(destination,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[W01] Rescored ",nrow(pairs)," pairs; ",nrow(review),
          " cases require further assessment.")
  message("[W01] LLM adjudication and final clustering have NOT run.")
  invisible(queue)
}
