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
  if (file.exists(file.path(destination,"manifest.json")))
    stop("Review preparation is already complete: ",
         file.path(destination,"manifest.json"),call.=FALSE)
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
  dir.create(destination,recursive=TRUE,showWarnings=FALSE)
  rescored<-file.path(destination,"rescored")
  csv<-file.path(rescored,"rescored_sample.csv")
  if(file.exists(csv)) {
    stopifnot(file.exists(file.path(rescored,"summary.json")))
    rs<-jsonlite::fromJSON(file.path(rescored,"summary.json"))
    if(!identical(rs$status,"success"))
      stop("Existing W01 rescored state is not marked complete.")
    message("[W01] Reusing completed identifier rescoring.")
  } else {
    if(dir.exists(rescored) && length(list.files(rescored,all.files=TRUE,no..=TRUE)))
      stop("Incomplete identifier rescore output present; preserve it for inspection.")
    execute("workflow_01_deduplication_identifier_rescore.R",
      c("--input-dir",scored,"--output-dir",rescored))
  }
  csv<-file.path(rescored,"rescored_sample.csv")
  metadata<-file.path(scored,"normalised_metadata.csv")
  if(!file.exists(csv)||!file.exists(metadata))
    stop("Identifier rescoring outputs missing.")
  # Ostara-only deterministic policy, after original identifier rescore and
  # before the LLM queue. Never modify packaged W01 upstream scripts.
  .apply_ostara_exact_abstract_policy(csv,metadata)
  pairs<-utils::read.csv(csv,stringsAsFactors=FALSE,check.names=FALSE)
  if(!all(c("record_i","record_j","rescored_classification",
            "rescored_rule","title_similarity","blocks") %in% names(pairs)))
    stop("W01 rescored CSV missing required fields.")
  expected<-jsonlite::fromJSON(file.path(scored,"expanded_summary.json"))$scoring_sample_n
  if(nrow(pairs)!=expected)
    stop("Identifier rescoring dropped candidate pairs.")
  # Stable indices are only accepted against exactly the metadata/union
  # snapshot that yielded the already validated scoring stage.
  required_ids <- c("source_id_i","source_record_id_i",
                    "source_id_j","source_record_id_j")
  if(length(setdiff(required_ids,names(pairs))))
    stop("W01 rescore lacks immutable source IDs.")
  meta <- utils::read.csv(metadata,stringsAsFactors=FALSE,check.names=FALSE)
  i <- as.integer(pairs$record_i)
  j <- as.integer(pairs$record_j)
  if(anyNA(i)||anyNA(j)||any(i<1L|i>nrow(meta)|j<1L|j>nrow(meta)))
    stop("Invalid W01 pair positions.")
  for(side in c("i","j")) {
    pos <- if(side=="i") i else j
    if(anyNA(pairs[[paste0("source_id_",side)]]) ||
       anyNA(pairs[[paste0("source_record_id_",side)]]) ||
       !identical(as.character(pairs[[paste0("source_id_",side)]]),
                  as.character(meta$source[pos])) ||
       !identical(as.character(pairs[[paste0("source_record_id_",side)]]),
                  as.character(meta$source_record_id[pos])))
      stop("W01 source identity changed since scoring.")
  }
  identity <- function(src,id) paste0(nchar(src,type="bytes"),":",src,
                                       nchar(id,type="bytes"),":",id)
  a<-identity(pairs$source_id_i,pairs$source_record_id_i)
  b<-identity(pairs$source_id_j,pairs$source_record_id_j)
  low<-ifelse(a<=b,a,b)
  high<-ifelse(a<=b,b,a)
  pairs$pair_key <- vapply(seq_along(low),function(k)
    digest::digest(paste0(nchar(low[[k]],type="bytes"),":",
                          low[[k]],high[[k]]),
                   algo="sha256",serialize=FALSE),character(1))
  if(anyDuplicated(pairs$pair_key))
    stop("Repeated candidate-pair identity after rescoring.")
  # Only explicit review decisions enter LLM adjudication.
  # Candidate-only unresolved pairs remain archived, not sent to the model.
  review<-pairs[pairs$rescored_classification == "review",,drop=FALSE]
  path<-file.path(destination,"human_review_pairs.csv")
  queue<-file.path(destination,"w01_review_cases.jsonl")
  if(file.exists(queue)) {
    if(!file.exists(path))
      stop("W01 review queue exists without its review-pair source.")
    old<-utils::read.csv(path,stringsAsFactors=FALSE,check.names=FALSE)
    if(nrow(old)!=nrow(review) ||
       !identical(as.character(old$pair_key),as.character(review$pair_key)))
      stop("Existing W01 review queue differs from rescored decisions.")
    message("[W01] Reusing previously generated review cases.")
  } else {
    if(file.exists(path))
      stop("Partial review pair export exists without queue. Preserve for inspection.")
    utils::write.csv(review,path,row.names=FALSE,na="")
    if(nrow(review)) {
      execute("workflow_01_build_duplicate_adjudication_cases.R",
        c("--review-pairs",path,"--metadata",metadata,
          "--union-dir",union,"--output",queue))
    } else {
      file.create(queue)
    }
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
