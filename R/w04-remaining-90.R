#' Screen W03-eligible records not included in the previous W04 sample
#'
#' Derives the exact complement of a completed selected-cohort W04 run.
#' Never rescreens the sampled records and never changes the human decisions.
#' Uses the identical prompt as the original W04 sample.
#'
#' @param sample_w04 Exact previously completed 10-percent W04 run directory,
#'   or NULL to discover the uniquely human-imported selected cohort.
#' @param map_path Existing Ostara evidence-map project.
#' @param open_app Whether to open Shiny review (default FALSE).
#' @param workers Concurrent model API requests in direct mode.
#' @param execution Use batch for asynchronous Batch API, direct for original API calls.
#' @param batch_chunk_size Number of requests in each Batch API input file.
#' @return New selected-cohort W04 run directory, invisibly.
#' @export
run_w04_remaining_90 <- function(sample_w04=NULL,map_path=NULL,open_app=FALSE,workers=1L,execution=c("direct","batch"),batch_chunk_size=1000L) {
  execution<-match.arg(execution)
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(path){
    v<-readLines(path,warn=FALSE,encoding="UTF-8")
    lapply(v[nzchar(trimws(v))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x)if(is.character(x)&&length(x)==1L&&!is.na(x))x else ""
  if(is.null(sample_w04)) {
    roots<-list.dirs(file.path(map_path,"runs","W01"),
                     recursive=FALSE,full.names=TRUE)
    runs<-unlist(lapply(roots,function(root) {
      p<-file.path(root,"W04_local")
      if(!dir.exists(p))return(character())
      list.dirs(p,recursive=FALSE,full.names=TRUE)
    }),use.names=FALSE)
    candidates<-runs[vapply(runs,function(p)
      startsWith(basename(p),"selected-")&&
      file.exists(file.path(p,"local_manifest.json"))&&
      file.exists(file.path(p,"consensus","workflow04_consensus_layer.jsonl"))&&
      dir.exists(file.path(p,"human_authoritative","imports"))&&
      length(list.files(file.path(p,"human_authoritative","imports"),
                        pattern="^[a-f0-9]{64}[.]jsonl$"))>0L,
      logical(1))]
    if(length(candidates)!=1L)
      stop("Expected one completed selected W04 cohort with imported human ",
           "decisions; found ",length(candidates),
           ". Supply sample_w04 explicitly.")
    sample_w04<-candidates[[1L]]
  }
  sample_w04<-normalizePath(sample_w04,winslash="/",mustWork=TRUE)
  if(!startsWith(sample_w04,paste0(map_path,"/runs/W01/")))
    stop("Sample W04 is outside the specified map.")
  root<-dirname(dirname(sample_w04))
  source_file<-file.path(sample_w04,"consensus",
                         "workflow04_consensus_layer.jsonl")
  manifest_file<-file.path(sample_w04,"local_manifest.json")
  prompt<-file.path(sample_w04,"screening_prompt.txt")
  w03<-file.path(root,"W03_publication_status","full",
                 "publication_status.jsonl")
  w03_manifest<-file.path(root,"W03_publication_status","full",
                          "local_manifest.json")
  scored<-file.path(root,"full_scoring")
  if(!all(file.exists(c(source_file,manifest_file,prompt,w03,w03_manifest)))||
     !dir.exists(scored))
    stop("Missing original sample, prompt, W03 source, or W01 full scoring.")
  manifest<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
  if(!startsWith(scalar(manifest$run_type),"selected-")||
     !identical(scalar(manifest$consensus_sha256),sha(source_file)))
    stop("The supplied W04 is not a completed, verified selected run.")
  source<-readjl(source_file)
  screened_ids<-vapply(source,function(z)scalar(z$record_id),character(1))
  if(!length(screened_ids)||anyDuplicated(screened_ids)||
     any(!nzchar(screened_ids)))
    stop("Original W04 sample has invalid record IDs.")
  # Verify the original explicitly selected cohort, including any records
  # that were W03-ineligible and therefore not sent to Luna.
  selection<-manifest$selected_record_ids_csv
  if(!is.null(selection)&&nzchar(scalar(selection))&&file.exists(selection)) {
    original_selection<-utils::read.csv(selection,stringsAsFactors=FALSE)
    if(!identical(names(original_selection),"record_id")||
       anyDuplicated(original_selection$record_id)||
       !identical(sha(selection),scalar(manifest$selection_sha256)))
      stop("Original 10-percent selection provenance mismatch.")
  }
  status<-readjl(w03)
  if(!length(status))stop("W03 is empty.")
  sid<-vapply(status,function(z)scalar(z$record_id),character(1))
  if(anyDuplicated(sid)||any(!nzchar(sid)))
    stop("W03 record IDs are missing or duplicated.")
  eligible<-vapply(status,function(z) {
    excluded<-z$publication_status$exclude_from_workflow04
    if(is.null(excluded))excluded<-z$exclude_from_workflow04
    if(is.null(excluded))excluded<-FALSE
    if(!is.logical(excluded)||length(excluded)!=1L||is.na(excluded))
      stop("W03 missing/invalid exclusion status for ",scalar(z$record_id))
    !excluded
  },logical(1))
  eligible_ids<-sid[eligible]
  if(!all(screened_ids%in%eligible_ids))
    stop("The original sample contains IDs outside W03 eligibility.")
  remaining<-eligible_ids[!eligible_ids%in%screened_ids]
  if(!length(remaining))stop("No W03-eligible records remain to screen.")
  if(length(intersect(screened_ids,remaining)))
    stop("Sample and remaining record IDs overlap.")
  if(!identical(length(screened_ids)+length(remaining),length(eligible_ids)))
    stop("W04 sample and complement do not partition W03 eligible records.")
  prompt_hash<-digest::digest(paste(readLines(prompt,warn=FALSE,
    encoding="UTF-8"),collapse="\n"),algo="sha256",serialize=FALSE)
  if(!identical(prompt_hash,scalar(manifest$prompt_sha256)))
    stop("Original W04 prompt hash does not match manifest.")
  selection_root<-file.path(root,"W04_local","selections")
  dir.create(selection_root,recursive=TRUE,showWarnings=FALSE)
  selected_csv<-file.path(selection_root,paste0(
    "remaining-after-",substr(sha(source_file),1L,12L),".csv"))
  expected<-c("record_id",remaining)
  if(file.exists(selected_csv)) {
    actual<-readLines(selected_csv,warn=FALSE,encoding="UTF-8")
    if(!identical(actual,expected))
      stop("Existing remaining-cohort CSV differs; refusing overwrite.")
  } else {
    writeLines(expected,selected_csv,useBytes=TRUE)
  }
  message("[W04] Previous sample: ",length(screened_ids),
          " W03-eligible records; remaining: ",length(remaining),
          " of ",length(eligible_ids)," W03-eligible records.")
  message("[W04] Exact remaining-cohort CSV: ",selected_csv)
  message("[W04] Reusing original prompt: ",prompt)
  message("[W04] Luna will screen ONLY the remaining IDs. ",
          "Original LLM and human review outputs stay unchanged.")
  run_w04(scored=scored,prompt_file=prompt,
          max_records=0L,record_ids_csv=selected_csv,
          model="gpt-5.6-luna",open_app=open_app,workers=workers,
          execution=execution,batch_chunk_size=batch_chunk_size)
}
