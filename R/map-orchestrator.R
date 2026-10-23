#' Start or resume a named search expansion within an existing evidence map
#'
#' Each run is kept under runs/orchestrated/<run_name>; no new map is created.
#' The supplied JSON strategy is archived with its SHA-256 checksum.
#' This controller deliberately refuses to dispatch unsafe, single-root legacy
#' workflows. A status of handoff_required is not a completed pipeline.
#' @param map Existing evidence map name or directory.
#' @param run_name Unique method-expansion label (e.g. collectiveaction_oilandgas).
#' @param search_file W00-compatible JSON search strategy file.
#' @param sources Optional W00 source identifiers (defaults to configured full set).
#' @param execute_scoping Actually run API count queries; FALSE only creates plan.
#' @return State of the named run, invisibly.
#' @export
run_evidence_map <- function(map="Ostara_collective_action_map",run_name,
                             search_file,sources=NULL,execute_scoping=FALSE) {
  map <- open_map(map)
  if(length(run_name)!=1L || is.na(run_name) ||
     !grepl("^[A-Za-z][A-Za-z0-9_-]*$",run_name))
    stop("run_name must be a non-empty identifier using letters, digits, - or _.")
  search_file <- normalizePath(path.expand(search_file),winslash="/",mustWork=TRUE)
  if(!grepl("[.]json$",search_file,ignore.case=TRUE))
    stop("W00 currently requires a JSON strategy with named concepts and expression; TXT strings are not yet supported.")
  if(!isTRUE(execute_scoping) && !identical(execute_scoping,FALSE))
    stop("execute_scoping must be TRUE or FALSE.")
  # Validate JSON before touching the map.
  strategy <- jsonlite::fromJSON(search_file,simplifyVector=FALSE)
  if(!is.list(strategy)||!length(strategy))
    stop("Empty search strategy.")
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  home <- file.path(map,"runs","orchestrated",run_name)
  if(dir.exists(home))
    stop("Named run already exists. Use resume_evidence_map(), not a new run.")
  if(!dir.create(home,recursive=TRUE,showWarnings=FALSE))
    stop("Cannot create named run directory.")
  archived <- file.path(home,"search_strategy.json")
  if(!file.copy(search_file,archived,overwrite=FALSE))
    stop("Could not archive search strategy.")
  state <- list(schema="evidencelandscaper-orchestration-v1",
      run_name=run_name,map_path=map,run_dir=home,
      search_sha256=sha(archived),status="planned",
      created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
      decision1=NULL,decision2=NULL,scope=NULL,w00=NULL,
      notes="One cumulative map; methods expansions are distinct runs, not separate reviews.")
  .orchestrator_save(state)
  message("[",run_name,"] Archived search strategy: ",archived)
  message("[",run_name,"] Strategy SHA-256: ",state$search_sha256)
  # Never run a full search from an unscoped plan automatically.
  if(!execute_scoping) {
    message("[",run_name,"] Planned. Run resume_evidence_map('",run_name,
            "', step='scope') to scope and reach checkpoint 1.")
    return(invisible(state))
  }
  resume_evidence_map(run_name,map=map,step="scope",sources=sources)
}

#' Resume a named search expansion safely
#'
#' @param run_name Previously created run name.
#' @param map Existing map name/path.
#' @param step One of auto,scope,retrieve,status.
#' @param screening Optional list(mode="all"|"n"|"percent", n=..., percent=...,\n#'   uncertain="exclude"|"adjudicate").
#' @param extraction Optional analogous selection for case-bearing publications,\n#'   including uncertain="exclude"|"adjudicate".
#' @param sources Optional W00 database IDs for first scoping.
#' @return Updated run state, invisibly.
#' @export
resume_evidence_map <- function(run_name,map="Ostara_collective_action_map",
                                step=c("auto","scope","retrieve","deduplicate","finish_w01","enrich","screen","finalize_screen","code","prepare_extraction","publish","status"),
                                screening=NULL,extraction=NULL,sources=NULL,\n                                screening_prompt=NULL,w07_prompt=NULL,ontology_file=NULL,\n                                w08_prompt=NULL,model="gpt-6-luna",\n                                extraction_dir=NULL,coordinates_csv=NULL) {
  step <- match.arg(step)
  map <- open_map(map)
  home <- file.path(map,"runs","orchestrated",run_name)
  state_path <- file.path(home,"state.json")
  if(!file.exists(state_path))stop("Named orchestration run not found: ",run_name)
  state <- jsonlite::fromJSON(state_path,simplifyVector=FALSE)
  sha <- function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  strategy <- file.path(home,"search_strategy.json")
  if(!file.exists(strategy)||!identical(state$search_sha256,sha(strategy)))
    stop("Archived search input was changed; refusing resume.")
  if(step=="status") {
    .orchestrator_summary(state)
    return(invisible(state))
  }
  if(!is.null(screening)) {
    if(!identical(state$status,"awaiting_screening_scope"))
      stop("First decision not currently available. Current status: ",state$status)
    if(!file.exists(state$scope$population_path) ||
       !identical(sha(state$scope$population_path),state$scope$population_sha256))
      stop("Verified W01 screening population changed.")
    population<-utils::read.csv(state$scope$population_path,stringsAsFactors=FALSE)
    if(!identical(names(population),"cluster_id") ||
       anyDuplicated(population$cluster_id))
      stop("Invalid W01 screening population.")
    selection_path<-file.path(home,"screening_sample.csv")
    sampled<-sample_evidence_records(as.character(population$cluster_id),
      screening,output_csv=selection_path)
    state$decision1 <- .orchestrator_choice(screening, state$scope$eligible_records,
                                           stage="screening")
    state$decision1$selected_ids_path<-selection_path
    state$decision1$selected_ids_sha256<-sha(selection_path)
    state$decision1$selection_sha256<-sampled$selection_sha256
    state$status <- "screening_scope_saved"
    .orchestrator_save(state)
    message("[",run_name,"] Checkpoint 1 saved: ",state$decision1$selected,
            " of ",state$decision1$population," records; uncertain decisions: ",\n            state$decision1$uncertain_policy,".")
    message("[",run_name,"] Sample record IDs archived; W04 explicit-ID handover pending.")
    state$blocking_stage <- "W02-W04 exact sample-ID handover"
    .orchestrator_save(state)
    return(invisible(state))
  }
  if(!is.null(extraction)) {
    if(!identical(state$status,"awaiting_extraction_scope"))
      stop("Second decision not available; downstream screening must complete first.")
    assessments<-file.path(state$w08$run,"case_assessments.jsonl")
    if(!file.exists(assessments))stop("W08 case assessment registry missing.")
    rows<-lapply(readLines(assessments,warn=FALSE,encoding="UTF-8"),
                 jsonlite::fromJSON,simplifyVector=FALSE)
    eligible<-vapply(Filter(function(z)z$case_evidence_status %in%
      c("specific_case","multiple_cases"),rows),
      function(z)as.character(z$record_id),character(1))
    if(length(eligible)!=state$case_bearing_publications || anyDuplicated(eligible))
      stop("W08 candidate publications changed; cannot sample.")
    selection<-file.path(home,"extraction_sample.csv")
    chosen<-sample_evidence_records(eligible,extraction,output_csv=selection)
    state$decision2 <- .orchestrator_choice(extraction,state$case_bearing_publications,stage="extraction")
    state$decision2$selected_ids_path<-selection
    state$decision2$selected_ids_sha256<-sha(selection)
    state$decision2$selection_sha256<-chosen$selection_sha256
    state$status <- "extraction_scope_saved"
    .orchestrator_save(state)
    message("[",run_name,"] Checkpoint 2 saved: ",state$decision2$selected,\n            " of ",state$decision2$population,\n            " case-bearing publications; uncertain cases: ",\n            state$decision2$uncertain_policy,".")
    return(invisible(state))
  }
  if(step=="scope" || (step=="auto"&&identical(state$status,"planned"))) {
    if(!identical(state$status,"planned"))stop("Scope already started; inspect run status.")
    message("[",run_name,"] W00 Planning and scoping across the existing map.")
    plan <- plan_w00(map,strategy=strategy,sources=sources,mode="full")
    state$w00 <- plan
    state$status <- "scoping"
    .orchestrator_save(state)
    counts <- scope_w00(plan)
    x <- utils::read.csv(counts,stringsAsFactors=FALSE)
    if(!all(c("hits","source_slug") %in% names(x)))
      stop("W00 scoping report missing hits/source_slug.")
    hits <- suppressWarnings(as.numeric(x$hits))
    state$scope <- list(sources=nrow(x),gross_hits=sum(hits,na.rm=TRUE),
        unknown_source_counts=sum(is.na(hits)),counts_path=counts,
        eligible_records=NULL)
    state$status <- "scoped_counts_only"
    .orchestrator_save(state)
    message("[",run_name,"] W00 scoped ",nrow(x)," automated sources; ",
            format(state$scope$gross_hits,big.mark=",")," reported hits; ",
            state$scope$unknown_source_counts," source counts unavailable.")
    message("[",run_name,"] These are GROSS database hits, not deduplicated records.")
    message("[",run_name,"] Retrieval/deduplication required before sampling checkpoint 1.")
    return(invisible(state))
  }
  if(step=="publish") {
    if(!state$status %in% c("w09_prepared","published"))
      stop("Checkpoint 2 and W09 preparation must complete before publication.")
    if(is.null(extraction_dir))
      stop("Supply the explicit approved completed W09 extraction directory.")
    if(is.null(state$w09$run)||!dir.exists(state$w09$run))
      stop("Named W09 run missing.")
    selected<-state$decision2$selected_ids_path
    if(!file.exists(selected) ||
       !identical(sha(selected),state$decision2$selected_ids_sha256))
      stop("Archived extraction selection changed.")
    queue<-utils::read.csv(file.path(state$w09$run,"retrieval_queue.csv"),
                           stringsAsFactors=FALSE)
    sample<-utils::read.csv(selected,stringsAsFactors=FALSE)
    if(anyDuplicated(queue$record_id)||!setequal(queue$record_id,sample$record_id))
      stop("W09 retrieval queue does not reconcile with checkpoint 2 sample.")
    inventory<-file.path(state$w09$run,"w09_extraction_ready_inventory.csv")
    if(!file.exists(inventory))stop("W09 extraction-ready inventory not finalised.")
    inv<-utils::read.csv(inventory,stringsAsFactors=FALSE)
    if(anyDuplicated(inv$record_id)||any(!inv$record_id%in%sample$record_id))
      stop("W09 extraction-ready inventory includes records outside this sample.")
    output<-publish_w09_evidence(map_path=map,w09_run=state$w09$run,
               extraction_dir=extraction_dir,coordinates_csv=coordinates_csv)
    state$published<-list(atlas=output$atlas,report=output$report,
          full_report=output$full_report,inventory_sha256=sha(inventory),
          extraction_manifest_sha256=sha(file.path(extraction_dir,"manifest.json")))
    state$status<-"published"
    state$blocking_stage<-NULL
    .orchestrator_save(state)
    message("[",run_name,"] Published ",nrow(inv)," verified full-text publications",
            " from ",nrow(sample)," selected case-bearing publications; ",
            nrow(sample)-nrow(inv)," without extraction-ready full text.")
    message("[",run_name,"] Atlas: ",state$published$atlas)
    message("[",run_name,"] Full report: ",state$published$full_report)
    return(invisible(state))
  }
  if(step=="prepare_extraction") {
    if(!identical(state$status,"extraction_scope_saved"))
      stop("Checkpoint 2 must be completed before W09 preparation.")
    if(!identical(state$decision2$uncertain_policy,"exclude"))
      stop("Uncertain W08 records require adjudication before extraction; ",
           "no automatic positive assignment is permitted.")
    selected<-state$decision2$selected_ids_path
    if(!file.exists(selected) ||
       !identical(sha(selected),state$decision2$selected_ids_sha256))
      stop("Checkpoint 2 exact publication IDs changed.")
    w09<-prepare_w09_from_w08(state$w08$run,map_path=map,
      include_unclear=FALSE,include_unvalidated=FALSE,record_ids_csv=selected,\n      w04_run=state$w04$run)
    state$w09<-list(run=w09,source_w08=state$w08$run,
                    selection_sha256=sha(selected))
    state$status<-"w09_prepared"
    state$blocking_stage<-"W09 document retrieval, full-text validation, extraction and publication"
    .orchestrator_save(state)
    message("[",run_name,"] W09 prepared ",state$decision2$selected,
            " candidate publications; original W08 decision statuses preserved.")
    return(invisible(state))
  }
  if(step=="screen") {
    if(!identical(state$status,"w03_complete"))
      stop("W02/W03 and checkpoint 1 must complete before W04.")
    if(is.null(screening_prompt))stop("Supply the approved screening_prompt explicitly.")
    selected<-state$decision1$selected_ids_path
    if(!file.exists(selected) ||
       !identical(sha(selected),state$decision1$selected_ids_sha256))
      stop("Archived exact screening sample no longer verifies.")
    scored<-state$w01$scored
    state$status<-"screening"
    .orchestrator_save(state)
    w04<-run_w04(scored= scored,prompt_file=screening_prompt,max_records=0L,
                 model=model,open_app=FALSE,record_ids_csv=selected)
    state$w04<-list(run=w04)
    state$status<-"w04_screened"
    .orchestrator_save(state)
    message("[",run_name,"] W04 screened selected record IDs. ",
            "Finalise with step='finalize_screen' to apply the uncertainty policy.")
    return(invisible(state))
  }
  if(step=="finalize_screen") {
    if(!identical(state$status,"w04_screened"))
      stop("W04 must finish before screening finalisation.")
    w04<-state$w04$run
    policy<-state$decision1$uncertain_policy
    final<-finalize_w04(map_path=map,w04=w04,uncertain=policy)
    state$w04$finalised<-final
    state$status<-"w04_final"
    .orchestrator_save(state)
    report<-jsonlite::fromJSON(file.path(final,"summary.json"))
    message("[",run_name,"] W04 final: ",report$retain," retained; ",
            report$exclude," excluded; policy exclusions: ",
            report$policy_uncertain_excluded)
    return(invisible(state))
  }
  if(step=="code") {
    if(!identical(state$status,"w04_final"))
      stop("Final W04 screening must exist before W07 and W08.")
    if(is.null(w07_prompt)||is.null(ontology_file))
      stop("Provide approved w07_prompt and ontology_file explicitly.")
    w04<-state$w04$run
    state$status<-"coding"
    .orchestrator_save(state)
    w07<-trial_w07(ontology_file=ontology_file,prompt_file=w07_prompt,
                   n=0L,map_path=map,model=model,w04_run=w04)
    state$w07<-list(run=w07)
    .orchestrator_save(state)
    w08<-trial_w08_cases(n=0L,map_path=map,model=model,
                         prompt_file=w08_prompt,w04_run=w04)
    state$w08<-list(run=w08)
    state$status<-"awaiting_extraction_scope"
    # Read line-delimited records independently; publication count is not
    # the extracted case count.
    statuses<-lapply(readLines(file.path(w08,"case_assessments.jsonl"),
                               warn=FALSE),jsonlite::fromJSON)
    state$case_bearing_publications<-sum(vapply(statuses,function(z)
      z$case_evidence_status %in% c("specific_case","multiple_cases"),logical(1)))
    .orchestrator_save(state)
    message("[",run_name,"] CHECKPOINT 2: ",state$case_bearing_publications,
            " case-bearing publications. Choose extraction scope and uncertain policy.")
    return(invisible(state))
  }
  if(step=="enrich") {
    if(!identical(state$status,"screening_scope_saved"))
      stop("Checkpoint 1 must be saved after final W01 before W02/W03.")
    selected<-state$decision1$selected_ids_path
    if(!file.exists(selected) ||
       !identical(sha(selected),state$decision1$selected_ids_sha256))
      stop("Screening selection changed since checkpoint 1.")
    scored<-state$w01$scored
    if(!file.exists(file.path(dirname(scored),"finalised","summary.json")))
      stop("Final W01 data missing.")
    message("[",run_name,"] W02: enriching complete verified W01 corpus ",
      "(not just the sample; selected IDs stay archived for W04).")
    state$status<-"enriching"
    .orchestrator_save(state)
    enriched<-run_w02_main(scored)
    state$w02<-list(enriched=enriched)
    .orchestrator_save(state)
    final<-finalize_w02(scored)
    state$w02$finalised<-final
    .orchestrator_save(state)
    message("[",run_name,"] W03: checking publication status.")
    w03<-run_w03(scored,limit=0L)
    state$w03<-list(output=w03)
    state$status<-"w03_complete"
    state$blocking_stage<-"W04 exact random-ID filtering and uncertain handling"
    .orchestrator_save(state)
    message("[",run_name,"] W03 complete. W04 exact-ID adapter still required;",
      " screening model has NOT been called.")
    return(invisible(state))
  }
  if(step=="finish_w01") {
    if(!state$status %in% c("w01_scored","w01_review_pending"))
      stop("W01 scoring must complete before finalisation.")
    scored<-state$w01$scored
    if(!dir.exists(scored))stop("Original W01 scoring directory missing.")
    if(!file.exists(file.path(dirname(scored),"finalised","summary.json"))) {
      if(!file.exists(file.path(dirname(scored),"review_preparation","llm_adjudication","summary.json"))) {
        state$status<-"w01_review_pending"
        .orchestrator_save(state)
        message("[",run_name,"] W01 LLM review still needed. Use continue_w01(scored,run_llm=TRUE), ",
                "then resume_evidence_map(...,step='finish_w01').")
        return(invisible(state))
      }
      finalized<-finalize_w01(scored)
    } else {
      finalized<-file.path(dirname(scored),"finalised")
    }
    summary<-jsonlite::fromJSON(file.path(finalized,"summary.json"),simplifyVector=FALSE)
    if(!identical(summary$status,"final"))
      stop("W01 summary is not final.")
    cluster<-file.path(finalized,"manifestation_cluster_map.csv")
    if(!file.exists(cluster))stop("W01 cluster map missing.")
    d<-utils::read.csv(cluster,stringsAsFactors=FALSE)
    if(!"cluster_id"%in% names(d))
      stop("W01 cluster map missing cluster_id; inspect before using as screening corpus.")
    ids<-unique(as.character(d$cluster_id))
    if(length(ids)!=as.integer(summary$clusters))
      stop("W01 cluster count differs from finalisation summary.")
    if(anyNA(ids)||any(!nzchar(ids)))stop("Invalid W01 cluster identifiers.")
    ids<-sort(ids,method="radix")
    ids_file<-file.path(home,"w01_screening_population.csv")
    if(file.exists(ids_file))stop("W01 checkpoint population exists; refusing overwrite.")
    utils::write.csv(data.frame(cluster_id=ids),ids_file,row.names=FALSE)
    state$w01$finalised<-finalized
    state$scope$eligible_records<-length(ids)
    state$scope$population_path<-ids_file
    state$scope$population_sha256<-sha(ids_file)
    state$status<-"awaiting_screening_scope"
    .orchestrator_save(state)
    message("[",run_name,"] CHECKPOINT 1 (after W01): ",length(ids),
            " deduplicated publication clusters. Choose all, n, or percent",
            " AND uncertain='exclude' or 'adjudicate'.")
    return(invisible(state))
  }
  if(step=="deduplicate") {
    if(!identical(state$status,"retrieved"))
      stop("W00 retrieval must be completed before first-run deduplication.")
    plan<-state$w00
    if(is.null(plan$run_dir)||!file.exists(file.path(plan$run_dir,"w00_full_retrieval_manifest.json")))
      stop("Original completed W00 retrieval unavailable.")
    message("[",run_name,"] W00: consolidating exact archived run.")
    consolidated<-consolidate_w00(plan)
    state$w00$consolidated_manifest<-consolidated
    state$status<-"consolidated"
    .orchestrator_save(state)
    message("[",run_name,"] W01: preparing record adapters for the exact W00 run.")
    prepared<-prepare_w01(map,w00_run=plan$run_dir)
    state$w01<-list(prepared_manifest=prepared)
    state$status<-"w01_prepared"
    .orchestrator_save(state)
    message("[",run_name,"] W01: constructing duplicate candidates.")
    candidates<-run_w01_initial(prepared)
    state$w01$candidates<-candidates
    state$status<-"w01_candidates"
    .orchestrator_save(state)
    scored<-score_w01_candidates(candidates)
    state$w01$scored<-scored
    state$status<-"w01_scored"
    .orchestrator_save(state)
    message("[",run_name,"] W01 candidate scoring complete: ",scored)
    message("[",run_name,"] Duplicate classification/finalisation must be validated before sampling checkpoint 1.")
    return(invisible(state))
  }
  if(step=="retrieve") {
    if(!identical(state$status,"scoped_counts_only"))
      stop("W00 scope must complete before retrieval.")
    message("[",run_name,"] Running immutable W00 full retrieval from the scoped plan.")
    if(is.null(state$w00$run_dir) || is.null(state$w00$search_plan) ||
       is.null(state$w00$query_archive) || is.null(state$w00$source_selection))
      stop("Saved W00 plan is incomplete; refusing new retrieval plan.")
    plan <- state$w00
    if(!file.exists(plan$search_plan) || !dir.exists(plan$query_archive) ||
       !file.exists(plan$source_selection))
      stop("Saved W00 plan files missing; cannot safely retrieve.")
    state$status <- "retrieving"
    .orchestrator_save(state)
    retrieval <- run_w00_full(plan)
    state$w00$retrieval_manifest <- retrieval
    state$status <- "retrieved"
    .orchestrator_save(state)
    message("[",run_name,"] W00 retrieval completed; input checksum preserved.")
    message("[",run_name,"] Next: isolated first-run W01 deduplication adapter.")
    return(invisible(state))
  }
  .orchestrator_summary(state)
  invisible(state)
}

.orchestrator_choice <- function(x,population,stage=c("screening","extraction")) {\n  stage <- match.arg(stage)
  if(is.null(population)||length(population)!=1L||
     !is.finite(as.numeric(population)))
    stop("Cannot sample until exact unique eligible population is known.")
  modes <- c("all","n","percent")\n  if(is.null(x$uncertain) || length(x$uncertain)!=1L ||\n     !x$uncertain %in% c("exclude","adjudicate"))\n    stop("Checkpoint decision must set uncertain = exclude or adjudicate.")
  if(!is.list(x)||!x$mode %in% modes)stop("Specify mode all, n or percent.")
  N <- as.integer(population)
  if(identical(x$mode,"all"))n<-N
  else if(identical(x$mode,"n")) {
    n<-as.numeric(x$n)
    if(length(n)!=1L||is.na(n)||n!=as.integer(n)||n<1||n>N)
      stop("Invalid n sample size.")
  } else {
    p<-as.numeric(x$percent)
    if(length(p)!=1L||is.na(p)||p<=0||p>100)stop("Invalid sample percentage.")
    n<-min(N,ceiling(N*p/100))
  }
  list(stage=stage,mode=x$mode,uncertain_policy=x$uncertain,\n       uncertain_handling=if(identical(x$uncertain,"exclude"))\n         "Exclude unresolved uncertain records from downstream stages; retain audit status" else\n         "Hold unresolved uncertain records for adjudication before handover",\n       selected=as.integer(n),population=N,
       seed=20261023L,policy="sample without replacement; retain sampled record IDs")
}

.orchestrator_save <- function(state) {
  path<-file.path(state$run_dir,"state.json")
  tmp<-paste0(path,".pending")
  jsonlite::write_json(state,tmp,pretty=TRUE,auto_unbox=TRUE,null="null")
  if(file.exists(path)&&!file.remove(path))
    stop("Cannot replace state file.")
  if(!file.rename(tmp,path))stop("Failed to commit new orchestration state.")
  invisible(path)
}

.orchestrator_summary <- function(state) {
  message("OSTARA | ",state$run_name,"\n",
    "  Map: ",state$map_path,"\n",
    "  State: ",state$status,"\n",
    "  Strategy SHA-256: ",state$search_sha256)
  if(!is.null(state$scope))
    message("  Scoping hits: ",state$scope$gross_hits,
            " across ",state$scope$sources," automated sources.")
  if(!is.null(state$blocking_stage))
    message("  Blocked handover: ",state$blocking_stage)
}
