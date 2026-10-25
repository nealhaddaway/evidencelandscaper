#' Run the original W04 consensus engine using an editable local prompt
#'
#' W04 title/abstract screening only. W00-W03 remain immutable. Uses upstream
#' 2-of-2/2-of-3 consensus and retains uncertain cases for subsequent review.
#' @param scored Completed W01 full-scoring directory.
#' @param prompt_file Path to a locally edited UTF-8 text prompt.
#' @param max_records Number of W03-eligible records to test (0 = all).
#' @param record_ids_csv Optional exact selected record_id CSV. Requires max_records=0.
#' @param model OpenAI API model identifier.
#' @param open_app NULL prompts in an interactive R console (default); TRUE opens without asking; FALSE never opens.
#' @return New local W04 run directory, invisibly.
#' @export
run_w04 <- function(scored, prompt_file, max_records=20L, model="gpt-5.6", open_app=NULL,record_ids_csv=NULL) {
  launch_review<-function(run_dir,status) {
    if(!identical(status,"HUMAN_REVIEW_REQUIRED"))return(invisible(NULL))
    if(identical(open_app,FALSE)) {
      message("[W04] Review required. Open later with adjudicate_w04(w04).")
      return(invisible(NULL))
    }
    if(isTRUE(open_app))return(adjudicate_w04(run_dir))
    if(!interactive()) {
      message("[W04] Non-interactive session: Shiny not opened. Use adjudicate_w04(w04) later.")
      return(invisible(NULL))
    }
    answer<-tolower(trimws(readline(
      "[W04] Human review is required. Open the Shiny adjudication app now? [y/N]: ")))
    if(answer%in%c("y","yes"))adjudicate_w04(run_dir) else
      message("[W04] Review deferred. Open later with adjudicate_w04(w04).")
    invisible(NULL)
  }
  scored<-normalizePath(scored,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  if(!grepl("\\.txt$",prompt_file,ignore.case=TRUE))
    stop("W04 requires a plain UTF-8 .txt prompt, not a Word document.")
  lines<-readLines(prompt_file,warn=FALSE,encoding="UTF-8")
  if(!length(lines)||!nzchar(trimws(paste(lines,collapse=""))))
    stop("W04 screening prompt is empty.")
  if(any(!validEnc(lines)))stop("Invalid prompt text encoding.")
  max_records<-as.integer(max_records)
  if(is.na(max_records)||max_records<0L)
    stop("max_records must be 0 (full corpus) or positive.")
  if(!is.null(record_ids_csv)){
    if(max_records!=0L)stop("For exact record IDs set max_records=0.")
    record_ids_csv<-normalizePath(record_ids_csv,winslash="/",mustWork=TRUE)
    selected<-utils::read.csv(record_ids_csv,stringsAsFactors=FALSE)
    if(!identical(names(selected),"record_id")||anyDuplicated(selected$record_id)||
       anyNA(selected$record_id)||any(!nzchar(selected$record_id)))
      stop("record_ids_csv must contain unique nonempty record_id values.")
  }
  root<-dirname(scored)
  map_path<-normalizePath(file.path(root,"..","..",".."),winslash="/",mustWork=TRUE)
  archive_map_prompt(prompt_file,"W04",map_path)
  canonical<-file.path(root,"W02_main","finalised","canonical.jsonl")
  w03<-file.path(root,"W03_publication_status","full","publication_status.jsonl")
  w03meta<-file.path(root,"W03_publication_status","full","local_manifest.json")
  for(p in c(canonical,w03,w03meta))
    if(!file.exists(p))stop("Missing finalised W02/W03 input: ",p)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  w03m<-jsonlite::fromJSON(w03meta)
  if(!identical(w03m$status,"final")||
     !isTRUE(w03m$full_corpus)||
     !identical(w03m$w02_canonical_sha256,sha(canonical))||
     !identical(w03m$status_sha256,sha(w03)))
    stop("W03 final input integrity validation failed.")
  if(!nzchar(Sys.getenv("OPENAI_API_KEY")))
    stop("OPENAI_API_KEY must be configured.")
  normalized_prompt<-paste(lines,collapse="\n")
  prompt_hash<-digest::digest(normalized_prompt,algo="sha256",serialize=FALSE)
  selection_hash<-if(is.null(record_ids_csv))NULL else sha(record_ids_csv)
  run_type<-if(!is.null(selection_hash))paste0("selected-",substr(selection_hash,1L,12L)) else
    if(max_records==0L)"full" else paste0("trial-",max_records)
  dest<-file.path(root,"W04_local",paste0(run_type,"-",substr(prompt_hash,1L,12L)))
  config<-file.path(dest,"screening_config.json")
  copied_prompt<-file.path(dest,"screening_prompt.txt")
  manifest<-file.path(dest,"local_manifest.json")
  if(file.exists(manifest)){
    previous<-jsonlite::fromJSON(manifest)
    if(!identical(previous$input_w02_sha256,sha(canonical))||
       !identical(previous$input_w03_sha256,sha(w03))||
       !identical(previous$prompt_sha256,prompt_hash)||
       !identical(previous$selection_sha256,selection_hash))
      stop("Existing W04 run fails immutable input checks.")
    message("[W04] Reusing existing model results: ",dest)
    sm<-jsonlite::fromJSON(file.path(dest,"consensus","summary.json"))
    launch_review(dest,sm$status)
    return(invisible(dest))
  }
  if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE))) {
    # Resume only an existing, identically configured W04 checkpoint.
    # The original W04 engine validates checkpoint record IDs and skips completed IDs.
    if(!file.exists(config)||!file.exists(copied_prompt))
      stop("Partial W04 run has no immutable prompt/config; preserve for investigation.")
    old<-jsonlite::fromJSON(config)
    if(!identical(old$expected_prompt_sha256,prompt_hash) ||
       !identical(old$prompt_path,copied_prompt) ||
       !identical(old$response_schema_name,"ostara_w04_relevance") ||
       !identical(sha(copied_prompt),sha(prompt_file)))
      stop("Partial W04 prompt/config mismatch; refusing resume.")
    message("[W04] Resuming existing checkpoints: ",dest)
  } else {
    dir.create(dest,recursive=TRUE,showWarnings=FALSE)
    writeLines(lines,copied_prompt,useBytes=TRUE)
    jsonlite::write_json(list(
      prompt_path=copied_prompt,
      prompt_version=paste0("local-ostara-",substr(prompt_hash,1,12)),
      response_schema_name="ostara_w04_relevance",
      expected_prompt_sha256=prompt_hash),config,pretty=TRUE,auto_unbox=TRUE)
  }
  script<-system.file("w04","workflow_04_luna_consensus.R",
                      package="evidencelandscaper")
  if(!nzchar(script))stop("Original W04 scanner was not installed.")
  output<-file.path(dest,"consensus")
  args<-c(script,"--canonical",canonical,"--workflow03-status",w03,
          "--screening-config",config,"--output-dir",output,
          "--model",model,"--max-records",as.character(max_records),
          if(!is.null(record_ids_csv))c("--include-record-ids",record_ids_csv),
          "--checkpoint-every","10")
  cat("[W04] Running original screening engine; model API requests will incur costs.\n")
  log<-suppressWarnings(system2(file.path(R.home("bin"),"Rscript"),
    args=vapply(args,shQuote,character(1)),stdout="",stderr=""))
  result<-attr(log,"status")
  if(!is.null(result)&&result!=0L)
    stop("[W04] Screening script failed:\n",paste(tail(log,35L),collapse="\n"))
  summary_file<-file.path(output,"summary.json")
  if(!file.exists(summary_file))stop("W04 consensus summary was not produced.")
  summary<-jsonlite::fromJSON(summary_file)
  if(!summary$status %in% c("PASS","HUMAN_REVIEW_REQUIRED"))
    stop("W04 screening incomplete: ",summary$status)
  expected<-if(max_records==0L)as.integer(w03m$publication_clusters) else NULL
  if(!is.null(expected)&&
     as.integer(summary$workflow03_eligible)+as.integer(summary$workflow03_excluded)!=expected)
    stop("W04 full-corpus record counts do not match W03.")
  jsonlite::write_json(list(
    schema="local-w04-consensus-handover-v1",status="screened",
    run_type=run_type,input_w02_sha256=sha(canonical),
    input_w03_sha256=sha(w03),prompt_sha256=prompt_hash,
    model=model,selection_sha256=selection_hash,
    selected_record_ids_csv=record_ids_csv,screening_summary_status=summary$status,
    consensus_sha256=sha(file.path(output,"workflow04_consensus_layer.jsonl"))),
    manifest,auto_unbox=TRUE,pretty=TRUE)
  message("[W04] ",summary$status,
          ": ",summary$records_screened," records screened; ",
          "uncertain cases remain for human adjudication.")
  launch_review(dest,summary$status)
  invisible(dest)
}
