#' Continue a legacy W04 first pass with Luna without altering source checkpoints
#'
#' Copies a fully completed non-Luna pass 1 to a distinct model-specific W04
#' directory, then uses Luna for pass 2 and for any third-pass disagreements.
#' @param source_w04 Old W04 directory containing completed pass1.jsonl.
#' @param scored Original completed W01 scoring directory.
#' @param prompt_file Exact original W04 screening prompt.
#' @param record_ids_csv Exact original selected record IDs.
#' @return Directory of completed Luna-led W04 run, invisibly.
#' @export
continue_w04_with_luna <- function(source_w04,scored,prompt_file,record_ids_csv) {
  source_w04<-normalizePath(source_w04,winslash="/",mustWork=TRUE)
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  record_ids_csv<-normalizePath(record_ids_csv,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  pass1<-file.path(source_w04,"consensus","pass1.jsonl")
  old_config<-file.path(source_w04,"screening_config.json")
  old_prompt<-file.path(source_w04,"screening_prompt.txt")
  if(!all(file.exists(c(pass1,old_config,old_prompt))))
    stop("Existing W04 run has no first-pass ledger/config/prompt.")
  lines<-readLines(pass1,warn=FALSE,encoding="UTF-8")
  lines<-lines[nzchar(trimws(lines))]
  if(!length(lines))stop("The first pass contains no saved responses.")
  entries<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  ids<-vapply(entries,function(x)as.character(x$record_id),character(1))
  if(anyDuplicated(ids)||any(!vapply(entries,function(x)
      identical(as.integer(x$pass),1L)&&
      identical(x$model_requested,"gpt-5.6"),logical(1))))
    stop("First-pass checkpoint contains unexpected IDs, passes or model provenance.")
  selected<-utils::read.csv(record_ids_csv,stringsAsFactors=FALSE)
  if(!identical(names(selected),"record_id")||
     anyDuplicated(selected$record_id)||anyNA(selected$record_id)||
     any(!nzchar(selected$record_id)))
    stop("Invalid exact selection CSV.")
  # Verify every W03-eligible selected record received exactly one first vote.
  root<-dirname(scored)
  w03<-file.path(root,"W03_publication_status","full","publication_status.jsonl")
  if(!file.exists(w03))stop("W03 status file missing.")
  status_lines<-readLines(w03,warn=FALSE,encoding="UTF-8")
  statuses<-lapply(status_lines[nzchar(trimws(status_lines))],
                   jsonlite::fromJSON,simplifyVector=FALSE)
  eligible_ids<-vapply(Filter(function(x)
    !isTRUE(x$publication_status$exclude_from_workflow04 %||%
            x$exclude_from_workflow04 %||% FALSE),statuses),
    function(x)as.character(x$record_id),character(1))
  expected<-intersect(selected$record_id,eligible_ids)
  if(!length(expected)||!setequal(ids,expected))
    stop("Pass 1 is not complete for exactly the selected W03-eligible records; do not transition yet.")
  # Verify prompt and selected-ID fingerprints agree with the original run path.
  prompt_hash<-digest::digest(paste(readLines(prompt_file,warn=FALSE,
                      encoding="UTF-8"),collapse="\n"),algo="sha256",serialize=FALSE)
  old<-jsonlite::fromJSON(old_config,simplifyVector=FALSE)
  if(!identical(old$expected_prompt_sha256,prompt_hash)||
     !identical(paste(readLines(old_prompt,warn=FALSE,encoding="UTF-8"),
                      collapse="\n"),
                paste(readLines(prompt_file,warn=FALSE,encoding="UTF-8"),
                      collapse="\n"))||
     !identical(basename(source_w04),paste0("selected-",
       substr(sha(record_ids_csv),1L,12L),"-",substr(prompt_hash,1L,12L))))
    stop("Original W04 run is not the exact selected sample/prompt.")
  if(file.exists(file.path(source_w04,"consensus","summary.json")))
    stop("The old W04 run already completed; use its finalised outputs instead.")
  # Make sure the original script is no longer writing.
  later<-file.path(source_w04,"consensus","pass2.jsonl")
  if(file.exists(later)&&length(readLines(later,warn=FALSE)))
    stop("Pass 2 has already started. Stop the original process before migration.")
  dest<-file.path(root,"W04_local",paste0(
    "selected-",substr(sha(record_ids_csv),1L,12L),"-",
    substr(prompt_hash,1L,12L),"-model-gpt-5.6-luna"))
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
    stop("Luna destination already exists; refusing to overwrite: ",dest)
  dir.create(file.path(dest,"consensus"),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(pass1,file.path(dest,"consensus","pass1.jsonl")),
            file.copy(prompt_file,file.path(dest,"screening_prompt.txt")))
  new_config<-old
  new_config$prompt_path<-file.path(dest,"screening_prompt.txt")
  jsonlite::write_json(new_config,file.path(dest,"screening_config.json"),
                       auto_unbox=TRUE,pretty=TRUE)
  jsonlite::write_json(list(
    schema="w04-model-transition-v1",
    from_model="gpt-5.6",
    to_model="gpt-5.6-luna",
    pass1_records=length(ids),
    pass1_sha256=sha(file.path(dest,"consensus","pass1.jsonl")),
    source_pass1_sha256=sha(pass1),
    source_w04=source_w04,
    prompt_sha256=prompt_hash,
    selection_sha256=sha(record_ids_csv)),
    file.path(dest,"model_transition.json"),auto_unbox=TRUE,pretty=TRUE)
  message("[W04] Archived first-pass ",length(ids),
          " GPT-5.6 decisions. All remaining calls use gpt-5.6-luna.")
  run_w04(scored=scored,prompt_file=prompt_file,
          record_ids_csv=record_ids_csv,max_records=0L,
          model="gpt-5.6-luna",open_app=FALSE)
}
