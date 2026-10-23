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
#' @param screening Optional list(mode="all"|"n"|"percent", n=..., percent=...).
#' @param extraction Optional analogous selection for case-bearing publications.
#' @param sources Optional W00 database IDs for first scoping.
#' @return Updated run state, invisibly.
#' @export
resume_evidence_map <- function(run_name,map="Ostara_collective_action_map",
                                step=c("auto","scope","retrieve","status"),
                                screening=NULL,extraction=NULL,sources=NULL) {
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
    state$decision1 <- .orchestrator_choice(screening, state$scope$eligible_records)
    state$status <- "screening_scope_saved"
    .orchestrator_save(state)
    message("[",run_name,"] Checkpoint 1 saved: ",state$decision1$selected,
            " of ",state$decision1$population," records.")
    message("[",run_name,"] W01–W04 multi-run input adapter is not yet validated. ",
      "STOPPING instead of mixing this expansion with a prior run.")
    state$status <- "handoff_required"
    state$blocking_stage <- "W01-W04 multi-run deduplication and sampled screening"
    .orchestrator_save(state)
    return(invisible(state))
  }
  if(!is.null(extraction)) {
    if(!identical(state$status,"awaiting_extraction_scope"))
      stop("Second decision not available; downstream screening must complete first.")
    state$decision2 <- .orchestrator_choice(extraction,state$case_bearing_publications)
    state$status <- "extraction_scope_saved"
    .orchestrator_save(state)
    message("[",run_name,"] Checkpoint 2 saved.")
    return(invisible(state))
  }
  if(step=="scope" || (step=="auto"&&identical(state$status,"planned"))) {
    if(!identical(state$status,"planned"))stop("Scope already started; inspect run status.")
    message("[",run_name,"] W00 Planning and scoping across the existing map.")
    plan <- plan_w00(map,strategy=strategy,sources=sources,mode="full")
    state$w00 <- list(plan=plan$run_dir,search_plan=plan$search_plan)
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
  if(step=="retrieve") {
    if(!identical(state$status,"scoped_counts_only"))
      stop("W00 scope must complete before retrieval.")
    message("[",run_name,"] Running immutable W00 full retrieval from the scoped plan.")
    # Existing run_w00_full accepts a plan list. Reconstructing a plan with an
    # archive is unsupported until the W00 plan descriptor is persisted.
    state$status <- "handoff_required"
    state$blocking_stage <- "W00 plan rehydration for safe full retrieval"
    .orchestrator_save(state)
    message("[",run_name,"] STOP: W00 plan descriptor is not yet persisted. ",
      "No duplicate W00 search plan created.")
    return(invisible(state))
  }
  .orchestrator_summary(state)
  invisible(state)
}

.orchestrator_choice <- function(x,population) {
  if(is.null(population)||length(population)!=1L||
     !is.finite(as.numeric(population)))
    stop("Cannot sample until exact unique eligible population is known.")
  modes <- c("all","n","percent")
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
  list(mode=x$mode,selected=as.integer(n),population=N,
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
