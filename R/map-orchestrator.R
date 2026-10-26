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
#' @param execute_scoping If TRUE, advance W00 onwards automatically until a
#'   checkpoint or a validation blocker. FALSE creates an inert named plan.
#' @param screening_prompt Optional W04 prompt; defaults to the existing verified Ostara prompt.
#' @param w07_prompt Optional W07 prompt; defaults to the verified 36-code archive.
#' @param ontology_file Optional W07 CSV; defaults to the verified 36-code archive.
#' @param w08_prompt Optional W08 case-identification prompt TXT.
#' @param model Model used for coding and extraction.
#' @param w01_uncertain How unresolved duplicate pairs are handled: quarantine
#'   all affected publication clusters or require adjudication.
#' @return State of the named run, invisibly.
#' @export
run_evidence_map <- function(map="Ostara_collective_action_map",run_name,
                             search_file,sources=NULL,execute_scoping=TRUE,
                             screening_prompt=NULL,w07_prompt=NULL,
                             ontology_file=NULL,w08_prompt=NULL,
                             model="gpt-6-luna",
                             w01_uncertain=c("quarantine","adjudicate"),
                             manual_ris=NULL,manual_search_strings=NULL) {
  w01_uncertain<-match.arg(w01_uncertain)
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
  supplied<-list(screening_prompt=screening_prompt,w07_prompt=w07_prompt,
                 ontology_file=ontology_file,w08_prompt=w08_prompt)
  supplied<-.orchestrator_reuse_inputs(map,supplied)
  for(nm in names(supplied))if(!is.null(supplied[[nm]])) {
    p<-normalizePath(path.expand(supplied[[nm]]),winslash="/",mustWork=TRUE)
    if(!nzchar(p)||file.info(p)$size==0)
      stop("Empty or missing approved run input: ",nm)
    supplied[[nm]]<-p
  }
  if(length(model)!=1L||is.na(model)||!nzchar(model))
    stop("Model identifier required.")

  if(!is.null(manual_ris)) {
    if(!is.list(manual_ris) || !setequal(names(manual_ris),c("cba","pqdt")))
      stop("manual_ris must be list(cba=...,pqdt=...), each with one or more RIS paths.")
    manual_ris<-lapply(manual_ris[c("cba","pqdt")],function(paths) {
      if(!is.character(paths)||!length(paths)||anyNA(paths)||
         any(!grepl("[.]ris$",paths,ignore.case=TRUE)))
        stop("Each manual source needs one or more .ris files.")
      paths<-vapply(paths,function(p)normalizePath(path.expand(p),
        winslash="/",mustWork=TRUE),character(1))
      if(anyDuplicated(basename(paths)))
        stop("Duplicate RIS filenames within a source. Rename the original exports uniquely.")
      paths
    })
    if(!is.null(manual_search_strings)&&
       (!is.character(manual_search_strings)||
        !setequal(names(manual_search_strings),c("cba","pqdt"))||
        anyNA(manual_search_strings)||
        any(!nzchar(trimws(manual_search_strings)))))
      stop("Manual search strings must be named cba and pqdt.")
  }
  home <- file.path(map,"runs","orchestrated",run_name)
  if(dir.exists(home))
    stop("Named run already exists. Use resume_evidence_map(), not a new run.")
  if(!dir.create(home,recursive=TRUE,showWarnings=FALSE))
    stop("Cannot create named run directory.")
  archived <- file.path(home,"search_strategy.json")
  if(!file.copy(search_file,archived,overwrite=FALSE))
    stop("Could not archive search strategy.")
  inputs<-list()
  if(any(!vapply(supplied,is.null,logical(1)))) {
    config_dir<-file.path(home,"configuration")
    dir.create(config_dir)
    for(nm in names(supplied)) {
      p<-supplied[[nm]]
      if(is.null(p))next
      archived_input<-file.path(config_dir,paste0(nm,if(nm=="ontology_file")".csv" else ".txt"))
      if(!file.copy(p,archived_input,overwrite=FALSE))
        stop("Failed to archive approved input ",nm)
      inputs[[nm]]<-list(path=archived_input,sha256=sha(archived_input))
    }
  }
  manual_inputs<-NULL
  if(!is.null(manual_ris)) {
    dir.create(file.path(home,"manual_inputs"))
    manual_inputs<-list()
    for(src in c("cba","pqdt")) {
      destdir<-file.path(home,"manual_inputs",src)
      dir.create(destdir)
      dest<-file.path(destdir,basename(manual_ris[[src]]))
      if(!all(file.copy(manual_ris[[src]],dest,overwrite=FALSE)))
        stop("Cannot archive all RIS files for ",src)
      if(any(file.info(dest)$size==0L))stop("Empty RIS file for ",src)
      q<-if(is.null(manual_search_strings))
        "NOT_RECORDED: exact manual query not supplied" else
        unname(manual_search_strings[[src]])
      manual_inputs[[src]]<-list(paths=unname(dest),
        sha256=unname(vapply(dest,sha,character(1))),search_string=q)
    }
  }
  state <- list(schema="evidencelandscaper-orchestration-v1",
      run_name=run_name,map_path=map,run_dir=home,
      search_sha256=sha(archived),status="planned",
      created_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"),
      decision1=NULL,decision2=NULL,scope=NULL,w00=NULL,
      inputs=inputs,model=model,manual_inputs=manual_inputs,
      w01_uncertain_policy=w01_uncertain,
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
  resume_evidence_map(run_name,map=map,step="auto",sources=sources,model=model)
}

#' Resume a named search expansion safely
#'
#' @param run_name Previously created run name.
#' @param map Existing map name/path.
#' @param step One of auto,scope,retrieve,status.
#' @param screening Optional list(mode="all"|"n"|"percent", n=..., percent=...,
#'   uncertain="exclude"|"adjudicate").
#' @param extraction Optional analogous selection for case-bearing publications,
#'   including uncertain="exclude"|"adjudicate".
#' @param sources Optional W00 database IDs for first scoping.
#' @return Updated run state, invisibly.
#' @export
resume_evidence_map <- function(run_name,map="Ostara_collective_action_map",
                                step=c("auto","scope","retrieve","import_manual","approve_partial_w00","deduplicate","prepare_w01_review","finish_w01","enrich","screen","finalize_screen","code","prepare_extraction","retrieve_fulltexts","extract","publish","status"),
                                screening=NULL,extraction=NULL,sources=NULL,
                                screening_prompt=NULL,w07_prompt=NULL,ontology_file=NULL,
                                w08_prompt=NULL,model="gpt-6-luna",
                                extraction_dir=NULL,coordinates_csv=NULL,cab_import=NULL,
                                execution=c("local","github"),allow_w01_llm=FALSE) {
  execution<-match.arg(execution)
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
  inputs<-state$inputs
  for(nm in names(inputs)) {
    obj<-inputs[[nm]]
    if(!file.exists(obj$path)||!identical(sha(obj$path),obj$sha256))
      stop("Archived run configuration changed: ",nm)
    if(nm=="screening_prompt"&&is.null(screening_prompt))screening_prompt<-obj$path
    if(nm=="w07_prompt"&&is.null(w07_prompt))w07_prompt<-obj$path
    if(nm=="ontology_file"&&is.null(ontology_file))ontology_file<-obj$path
    if(nm=="w08_prompt"&&is.null(w08_prompt))w08_prompt<-obj$path
  }
  if(identical(model,"gpt-6-luna")&&!is.null(state$model))model<-state$model
  if(step=="status") {
    .orchestrator_summary(state)
    return(invisible(state))
  }
  if(step=="auto" && is.null(screening) && is.null(extraction)) {
    # Advance using explicit stage interfaces. Stop only at user decisions,
    # unresolved human review, or unverifiable evidence. All stages save state.
    for(i in seq_len(20L)){
      phase<-state$status
      next_step<-switch(phase,
        planned="scope",scoping="scope",scoped_counts_only="retrieve",retrieving="retrieve",
        retrieved=if(is.null(state$manual_inputs))"deduplicate" else
          if(file.exists(file.path(state$w00$run_dir,
            "accepted_manual_imports.json")))"deduplicate" else "import_manual",
        manual_importing="import_manual",
        consolidated="deduplicate",w01_prepared="deduplicate",
        w01_candidates="deduplicate",
        w01_scored="finish_w01",
        w01_classifying="finish_w01",w01_review_pending="finish_w01",
        screening_scope_saved="enrich",enriching="enrich",w03_complete="screen",
        w04_screened="finalize_screen",w04_final="code",
        extraction_scope_saved="prepare_extraction",
        w09_prepared="retrieve_fulltexts",retrieving_fulltexts="retrieve_fulltexts",
        fulltexts_ready="extract",extracting="extract",
        extracted="publish",NULL)
      if(is.null(next_step))break
      message("[",run_name,"] Orchestrator: ",phase," -> ",next_step)
      state<-resume_evidence_map(run_name,map=map,step=next_step,
        sources=sources,screening_prompt=screening_prompt,
        w07_prompt=w07_prompt,ontology_file=ontology_file,
        w08_prompt=w08_prompt,model=model,
        extraction_dir=extraction_dir,coordinates_csv=coordinates_csv,
        execution=execution,allow_w01_llm=allow_w01_llm)
      if(identical(state$status,phase)) {
        if(identical(phase,"retrieving"))break
        stop("Orchestrator made no progress at ",phase)
      }
    }
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
            " of ",state$decision1$population," records; uncertain decisions: ",
            state$decision1$uncertain_policy,".")
    message("[",run_name,"] Sample record IDs archived; W04 explicit-ID handover pending.")
    state$blocking_stage <- NULL
    .orchestrator_save(state)
    if(step=="auto")
      return(resume_evidence_map(run_name,map=map,step="auto",
        sources=sources,screening_prompt=screening_prompt,
        w07_prompt=w07_prompt,ontology_file=ontology_file,
        w08_prompt=w08_prompt,model=model,
        extraction_dir=extraction_dir,coordinates_csv=coordinates_csv))
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
    message("[",run_name,"] Checkpoint 2 saved: ",state$decision2$selected,
            " of ",state$decision2$population,
            " case-bearing publications; uncertain cases: ",
            state$decision2$uncertain_policy,".")
    if(step=="auto")
      return(resume_evidence_map(run_name,map=map,step="auto",
        sources=sources,screening_prompt=screening_prompt,
        w07_prompt=w07_prompt,ontology_file=ontology_file,
        w08_prompt=w08_prompt,model=model,
        extraction_dir=extraction_dir,coordinates_csv=coordinates_csv))
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
  if(step=="approve_partial_w00") {
    if(!identical(state$status,"retrieving"))
      stop("Partial W00 approval is only allowed before W01.")
    path<-file.path(state$w00$run_dir,"w00_full_retrieval_manifest.json")
    if(!file.exists(path))stop("W00 retrieval manifest missing.")
    mf<-jsonlite::fromJSON(path,simplifyVector=FALSE)
    if(!identical(mf$status,"incomplete"))
      stop("Expected an incomplete, finished W00 harvest.")
    incomplete<-names(Filter(function(z)!identical(z$status,"success"),mf$sources))
    expected<-c("agricola","pubmed","ethos","epmc_preprints")
    if(!setequal(incomplete,expected) ||
       !all(vapply(mf$sources[incomplete],function(z)
         identical(z$status,"unavailable") &&
         grepl("HTTP 503",z$error,fixed=TRUE),logical(1))))
      stop("Incomplete W00 sources differ from the explicitly approved four 503 outages.")
    if(length(mf$sources)!=25L)
      stop("W00 is missing provider outcomes; partial approval refused.")
    if(!is.null(cab_import)) {
      if(!is.character(cab_import)||length(cab_import)!=1L)
        stop("cab_import must be the existing import_ris() output directory.")
      registry<-file.path(state$w00$run_dir,"accepted_manual_imports.json")
      if(file.exists(registry))stop("Manual RIS registry exists; verify before approval.")
      state$w00$manual_acceptance<-accept_w00_ris(
        state$w00,imports=c(cba=cab_import))
    }
    approval<-file.path(state$w00$run_dir,"approved_partial_retrieval.json")
    if(file.exists(approval))stop("Partial approval already exists.")
    jsonlite::write_json(list(
      schema="evidencelandscaper-w00-partial-approval-v1",
      run_id=basename(state$w00$run_dir),
      original_manifest_sha256=sha(path),
      unavailable_sources=as.list(sort(incomplete)),
      authorisation="User explicitly approved proceeding to W01 with four unavailable sources",
      created_at_utc=format(Sys.time(),tz="UTC",format="%Y-%m-%dT%H:%M:%SZ")
    ),approval,auto_unbox=TRUE,pretty=TRUE)
    state$w00$partial_approval<-approval
    state$status<-"retrieved"
    .orchestrator_save(state)
    message("[",run_name,"] Approved partial W00: ",
      length(mf$sources)-length(incomplete)," successful automated sources; ",
      "four 503 failures retained; proceeding towards W01.")
    return(invisible(state))
  }
  if(step=="import_manual") {
    if(!state$status %in% c("retrieved","manual_importing"))
      stop("Complete automated W00 before manual RIS ingestion.")
    if(is.null(state$manual_inputs)||
       !setequal(names(state$manual_inputs),c("cba","pqdt")))
      stop("Both manual RIS files must be supplied at the beginning of the named run.")
    if(file.exists(file.path(state$w00$run_dir,"accepted_manual_imports.json"))) {
      state$status<-"retrieved"
      .orchestrator_save(state)
      return(invisible(state))
    }
    state$status<-"manual_importing"
    .orchestrator_save(state)
    imports<-list()
    for(src in c("cba","pqdt")) {
      x<-state$manual_inputs[[src]]
      if(length(x$paths)!=length(x$sha256)||
         any(!file.exists(unlist(x$paths)))||
         !identical(unname(vapply(unlist(x$paths),sha,character(1))),
                    unname(unlist(x$sha256))))
        stop("Archived RIS checksum mismatch for ",src)
      existing<-state$manual_imports[[src]]
      if(is.null(existing)) {
        existing<-import_ris(map=map,files=unlist(x$paths),source=src,
          database=if(src=="cba")"CAB Abstracts" else "ProQuest Dissertations & Theses",
          search_date=as.character(Sys.Date()),search_string=x$search_string,
          keyword_semantics="unknown")
        if(is.null(state$manual_imports))state$manual_imports<-list()
        state$manual_imports[[src]]<-existing
        .orchestrator_save(state)
      }
      imports[[src]]<-existing
    }
    accepted<-accept_w00_ris(state$w00,imports=unlist(imports,use.names=TRUE))
    state$w00$manual_acceptance<-accepted
    state$status<-"retrieved"
    .orchestrator_save(state)
    message("[",run_name,"] Both manual RIS sources accepted before W01.")
    return(invisible(state))
  }
  if(step=="retrieve_fulltexts") {
    if(!state$status %in% c("w09_prepared","retrieving_fulltexts","fulltexts_ready"))
      stop("Checkpoint 2 and W09 selection required before retrieval.")
    w09<-state$w09$run
    queue<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),stringsAsFactors=FALSE)
    if(!nrow(queue))stop("No selected full-text publications.")
    state$status<-"retrieving_fulltexts"
    .orchestrator_save(state)
    message("[",run_name,"] W09: checking full-text availability for ",nrow(queue),
            " publications.")
    check_w09_fulltext(w09)
    retrieve_w09_from_availability(w09)
    retrieve_w09_structured(w09,max_records=nrow(queue))
    validated<-validate_w09_fulltexts(w09)
    v<-utils::read.csv(validated,stringsAsFactors=FALSE)
    if(!setequal(v$record_id,queue$record_id))
      stop("W09 validation and retrieval queue differ.")
    state$w09$validation_csv<-validated
    state$w09$validation_sha256<-sha(validated)
    state$w09$ready_count<-sum(v$status=="extraction_ready")
    state$w09$other_statuses<-as.list(table(v$status))
    if(state$w09$ready_count>0L) {
      inv<-finalise_w09_inventory(w09)
      state$w09$inventory<-inv
      state$w09$inventory_sha256<-sha(inv)
      state$status<-"fulltexts_ready"
    } else state$status<-"fulltext_review_required"
    .orchestrator_save(state)
    message("[",run_name,"] W09: ",state$w09$ready_count," / ",nrow(queue),
            " extraction-ready; see validation CSV for outstanding documents.")
    return(invisible(state))
  }
  if(step=="extract") {
    if(!identical(state$status,"fulltexts_ready"))
      stop("Verified full-text inventory required before model extraction.")
    w09<-state$w09$run
    inv<-state$w09$inventory
    if(!file.exists(inv)||!identical(sha(inv),state$w09$inventory_sha256))
      stop("Full-text inventory changed after validation.")
    df<-utils::read.csv(inv,stringsAsFactors=FALSE)
    if(anyDuplicated(df$record_id)||!nrow(df))stop("No unique extraction-ready IDs.")
    state$status<-"extracting"
    .orchestrator_save(state)
    extraction<-analyse_w09_cases_v2(w09,record_ids=as.character(df$record_id),
                         w07_run=state$w07$run,model=model,max_pages=0L)
    state$w09$extraction_dir<-extraction
    state$status<-"extracted"
    .orchestrator_save(state)
    message("[",run_name,"] W09 extraction saved: ",extraction)
    return(invisible(state))
  }
  if(step=="publish") {
    if(!state$status %in% c("extracted","published"))
      stop("Checkpoint 2 and W09 preparation must complete before publication.")
    if(is.null(extraction_dir))extraction_dir<-state$w09$extraction_dir
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
               extraction_dir=extraction_dir,coordinates_csv=coordinates_csv,
               named_run=run_name,w00_run=state$w00$run_dir,
               w04_run=state$w04$run,w07_run=state$w07$run,
               w08_run=state$w08$run)
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
    if(identical(state$decision2$uncertain_policy,"adjudicate") &&
       !is.null(state$uncertain_case_publications) &&
       state$uncertain_case_publications>0L)
      stop("W08 has ",state$uncertain_case_publications,
           " unclear case assessments requiring adjudication. ",
           "Do not promote uncertain records automatically.")
    selected<-state$decision2$selected_ids_path
    if(!file.exists(selected) ||
       !identical(sha(selected),state$decision2$selected_ids_sha256))
      stop("Checkpoint 2 exact publication IDs changed.")
    w09<-prepare_w09_from_w08(state$w08$run,map_path=map,
      include_unclear=FALSE,include_unvalidated=FALSE,record_ids_csv=selected,
      w04_run=state$w04$run)
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
    if(identical(state$status,"screening"))
      message("[",run_name,"] Resuming interrupted W04 from its saved checkpoint.")
    else if(!identical(state$status,"w03_complete"))
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
    if(!state$status %in% c("w04_final","coding"))
      stop("Final W04 screening must exist before W07 and W08.")
    if(is.null(w07_prompt)||is.null(ontology_file))
      stop("Provide approved w07_prompt and ontology_file explicitly.")
    w04<-state$w04$run
    if(identical(state$status,"w04_final")) {
      state$status<-"coding"
      .orchestrator_save(state)
    }
    if(!is.null(state$w07$run)) {
      w07<-state$w07$run
      if(!file.exists(file.path(w07,"manifest.json"))||
         !file.exists(file.path(w07,"coding.jsonl")))
        stop("Saved W07 coding outputs incomplete; refusing to repeat W07.")
      message("[",run_name,"] Reusing completed W07 coding: ",w07)
    } else {
      w07<-trial_w07(ontology_file=ontology_file,prompt_file=w07_prompt,
                     n=0L,map_path=map,model=model,w04_run=w04)
      state$w07<-list(run=w07)
      .orchestrator_save(state)
    }
    w08<-trial_w08_cases(n=0L,map_path=map,model=model,
                         prompt_file=w08_prompt,w04_run=w04)
    w08summary<-jsonlite::fromJSON(file.path(w08,"summary.json"),simplifyVector=FALSE)
    if(!w08summary$status %in% c("PILOT_COMPLETE","PILOT_PARTIAL"))
      stop("W08 summary has unexpected status; refusing checkpoint.")
    unresolved<-if(identical(w08summary$status,"PILOT_PARTIAL"))
      unlist(w08summary$unresolved_record_ids,use.names=FALSE) else character()
    state$w08<-list(run=w08,unresolved_record_ids=as.list(unresolved))
    state$unvalidated_case_publications<-length(unresolved)
    state$status<-"awaiting_extraction_scope"
    # Read line-delimited records independently; publication count is not
    # the extracted case count.
    statuses<-lapply(readLines(file.path(w08,"case_assessments.jsonl"),
                               warn=FALSE),jsonlite::fromJSON)
    state$case_bearing_publications<-sum(vapply(statuses,function(z)
      z$case_evidence_status %in% c("specific_case","multiple_cases"),logical(1)))
    state$uncertain_case_publications<-sum(vapply(statuses,function(z)
      identical(z$case_evidence_status,"unclear"),logical(1)))
    state$assessed_publications<-length(statuses)
    .orchestrator_save(state)
    message("[",run_name,"] CHECKPOINT 2: ",state$case_bearing_publications,
            " case-bearing publications across ",state$assessed_publications,
            " W08 assessments; ",state$uncertain_case_publications,
            " unclear classifications; ",length(unresolved),
            " unvalidated record(s) excluded from case-bearing pool. Choose extraction scope and uncertain policy.")
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
  if(step=="prepare_w01_review") {
    if(!state$status %in% c("w01_scored","w01_classifying"))
      stop("Completed W01 scoring required for review preparation.")
    scored<-state$w01$scored
    if(!dir.exists(scored))stop("Saved W01 scoring directory missing.")
    prepared<-continue_w01(scored,run_llm=FALSE,open_app=FALSE)
    queue<-prepared$review_queue
    n<-length(readLines(queue,warn=FALSE,encoding="UTF-8"))
    state$w01$review_queue<-queue
    state$w01$review_cases<-n
    state$blocking_stage<-"W01 review prepared: explicit approval required before LLM"
    state$status<-"w01_scored"
    .orchestrator_save(state)
    message("[",run_name,"] W01 review prepared: ",n,
            " explicit review cases. PAUSED: no LLM calls.")
    return(invisible(state))
  }
  if(step=="finish_w01" && !isTRUE(allow_w01_llm)) {
    message("[",run_name,"] W01 LLM calls are not authorised. ",
            "Run step='prepare_w01_review' to inspect the queue; ",
            "allow_w01_llm=TRUE is required for further LLM execution.")
    return(invisible(state))
  }
  if(step=="finish_w01") {
    if(!state$status %in% c("w01_scored","w01_classifying","w01_review_pending"))
      stop("W01 scoring must complete before finalisation.")
    scored<-state$w01$scored
    if(!dir.exists(scored))stop("Original W01 scoring directory missing.")
    if(!file.exists(file.path(dirname(scored),"finalised","summary.json"))) {
      if(!file.exists(file.path(dirname(scored),"review_preparation","llm_adjudication","summary.json"))) {
        state$status<-"w01_classifying"
        .orchestrator_save(state)
        classified<-continue_w01(scored,run_llm=TRUE,open_app=FALSE,
                  model=Sys.getenv("OPENAI_DUPLICATE_MODEL","gpt-5.6"))
        state$w01$llm_dir<-classified$llm_dir
        state$w01$human_cases<-classified$human_cases
        state$status<-"w01_scored"
        .orchestrator_save(state)
        message("[",run_name,"] W01 LLM classification completed; ",
                classified$human_cases," review-uncertain pairs.")
      }
      queue<-file.path(dirname(scored),"review_preparation",
                       "llm_adjudication","human_review_cases.jsonl")
      if(!file.exists(queue))stop("W01 LLM human-review queue missing.")
      pending<-readLines(queue,warn=FALSE,encoding="UTF-8")
      pending<-pending[nzchar(trimws(pending))]
      if(length(pending)>0L) {
        state$w01$human_cases<-length(pending)
        if(identical(state$w01_uncertain_policy,"quarantine")) {
          policy_csv<-file.path(dirname(scored),"review_preparation",
                     "llm_adjudication","w01_uncertain_quarantined_sources.csv")
          if(!file.exists(policy_csv))
            quarantine_w01_uncertain(scored)
          state$w01$quarantine_csv<-policy_csv
          state$w01$quarantine_sha256<-sha(policy_csv)
          .orchestrator_save(state)
          message("[",run_name,"] W01 policy: unresolved pairs quarantined ",
                  "from screening; no human adjudication claimed.")
        } else {
          state$status<-"w01_review_pending"
          state$blocking_stage<-"W01 unresolved duplicate pairs require adjudication"
          .orchestrator_save(state)
          message("[",run_name,"] W01 has ",length(pending),
                  " unresolved duplicate pairs; adjudication selected.")
          return(invisible(state))
        }
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
    if(!is.null(state$w01$quarantine_csv)) {
      file<-state$w01$quarantine_csv
      if(!file.exists(file)||!identical(sha(file),state$w01$quarantine_sha256))
        stop("W01 quarantine source manifest changed.")
      quarantined<-utils::read.csv(file,stringsAsFactors=FALSE)
      needed<-c("source","source_record_id")
      if(!all(needed%in%names(quarantined))||
         !all(c(needed,"cluster_id")%in%names(d)))
        stop("W01 cluster mapping cannot link quarantine identities.")
      key<-function(s,i)paste0(nchar(s,type="bytes"),":",s,
                                    nchar(i,type="bytes"),":",i)
      mapkey<-key(as.character(d$source),as.character(d$source_record_id))
      quarantinekey<-key(as.character(quarantined$source),
                         as.character(quarantined$source_record_id))
      matched<-match(quarantinekey,mapkey)
      if(anyNA(matched))stop("Uncertain W01 manifestations were lost in cluster mapping.")
      forbidden<-sort(unique(as.character(d$cluster_id[matched])),method="radix")
      excluded_path<-file.path(home,"w01_quarantined_cluster_ids.csv")
      if(file.exists(excluded_path))stop("Quarantined clusters already archived.")
      utils::write.csv(data.frame(cluster_id=forbidden),excluded_path,row.names=FALSE)
      state$w01$quarantined_cluster_ids_path<-excluded_path
      state$w01$quarantined_cluster_ids_sha256<-sha(excluded_path)
      state$w01$quarantined_cluster_count<-length(forbidden)
      message("[",run_name,"] W01 policy: ",length(forbidden),
              " ambiguous publication clusters excluded before screening.")
    }
    if(length(ids)!=as.integer(summary$clusters))
      stop("W01 cluster count differs from finalisation summary.")
    if(anyNA(ids)||any(!nzchar(ids)))stop("Invalid W01 cluster identifiers.")
    if(exists("forbidden",inherits=FALSE))
      ids<-setdiff(ids,forbidden)
    if(!length(ids))
      stop("No unambiguous W01 clusters remain after policy quarantine.")
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
    if(!state$status %in% c("retrieved","consolidated","w01_prepared","w01_candidates"))
      stop("W00 must be approved or W01 previously started before deduplication.")
    plan<-state$w00
    if(is.null(plan$run_dir)||!file.exists(file.path(plan$run_dir,"w00_full_retrieval_manifest.json")))
      stop("Original completed W00 retrieval unavailable.")
    if(identical(state$status,"retrieved")) {
      message("[",run_name,"] W00: reusing source manifests; no consolidation rewrite.")
      state$status<-"consolidated"
      .orchestrator_save(state)
    }
    if(identical(state$status,"consolidated")) {
      message("[",run_name,"] W01: preparing record adapters.")
      prepared<-if(is.list(state$w01) && !is.null(state$w01$prepared_manifest))
        state$w01$prepared_manifest else prepare_w01(map,w00_run=plan$run_dir)
      state$w01<-list(prepared_manifest=prepared)
      state$status<-"w01_prepared"
      .orchestrator_save(state)
    }
    if(identical(state$status,"w01_prepared") && identical(execution,"github")) {
      dispatch_w01_github(run_name,map=map)
      return(invisible(jsonlite::fromJSON(state_path,simplifyVector=FALSE)))
    }
    if(identical(state$status,"w01_prepared")) {
      message("[",run_name,"] W01: constructing or recovering candidate pairs.")
      candidates<-run_w01_initial(state$w01$prepared_manifest)
      state$w01$candidates<-candidates
      state$status<-"w01_candidates"
      .orchestrator_save(state)
    }
    candidates<-state$w01$candidates
    if(!dir.exists(candidates))stop("W01 candidate directory missing.")
    message("[",run_name,"] W01: scoring candidate pairs.")
    scored<-score_w01_candidates(candidates)
    state$w01$scored<-scored
    state$status<-"w01_scored"
    .orchestrator_save(state)
    message("[",run_name,"] W01 candidate scoring complete: ",scored)
    message("[",run_name,"] Duplicate classification/finalisation must be validated before sampling checkpoint 1.")
    return(invisible(state))
  }
  if(step=="retrieve") {
    if(!state$status %in% c("scoped_counts_only","retrieving"))
      stop("W00 must be scoped or an interrupted retrieval before continuation.")
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
    retrieval_status<-jsonlite::fromJSON(retrieval,simplifyVector=FALSE)
    if(!identical(retrieval_status$status,"success")) {
      state$status<-"retrieving"
      .orchestrator_save(state)
      message("[",run_name,"] W00 is incomplete; remaining provider outages ",
              "are recorded in the manifest. W01 has NOT started.")
      return(invisible(state))
    }
    state$status <- "retrieved"
    .orchestrator_save(state)
    message("[",run_name,"] W00 retrieval completed; input checksum preserved.")
    message("[",run_name,"] Next: isolated first-run W01 deduplication adapter.")
    return(invisible(state))
  }
  .orchestrator_summary(state)
  invisible(state)
}

.orchestrator_choice <- function(x,population,stage=c("screening","extraction")) {
  stage <- match.arg(stage)
  if(is.null(population)||length(population)!=1L||
     !is.finite(as.numeric(population)))
    stop("Cannot sample until exact unique eligible population is known.")
  modes <- c("all","n","percent")
  if(is.null(x$uncertain) || length(x$uncertain)!=1L ||
     !x$uncertain %in% c("exclude","adjudicate"))
    stop("Checkpoint decision must set uncertain = exclude or adjudicate.")
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
  list(stage=stage,mode=x$mode,uncertain_policy=x$uncertain,
       uncertain_handling=if(identical(x$uncertain,"exclude"))
         "Exclude unresolved uncertain records from downstream stages; retain audit status" else
         "Hold unresolved uncertain records for adjudication before handover",
       selected=as.integer(n),population=N,
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

#' Print counts directly from this run's saved, verified stage artefacts.
.orchestrator_summary <- function(state) {
  message("Evidence map | ",state$run_name,
          "\n  Map: ",state$map_path,
          "\n  Status: ",state$status,
          "\n  Search SHA-256: ",state$search_sha256)
  scope<-state$scope
  if(!is.null(scope)){
    message("  W00 gross database hits: ",format(scope$gross_hits,big.mark=","),
            " from ",scope$sources," sources",
            if(scope$unknown_source_counts>0L)paste0(" (",scope$unknown_source_counts,
              " sources without counts)") else "")
    if(!is.null(scope$eligible_records))
      message("  W01 eligible publication clusters: ",scope$eligible_records,
              "; quarantined ambiguous clusters: ",
              if(is.null(state$w01$quarantined_cluster_count)) 0L else
                state$w01$quarantined_cluster_count)
  }
  if(!is.null(state$decision1))
    message("  Checkpoint 1: ",state$decision1$selected," of ",
            state$decision1$population," selected for screening; uncertain=",
            state$decision1$uncertain_policy)
  if(!is.null(state$w04$finalised)){
    p<-file.path(state$w04$finalised,"summary.json")
    if(file.exists(p)){
      z<-jsonlite::fromJSON(p)
      message("  W04: ",z$records," assessed; ",z$retain," retained; ",
              z$exclude," excluded; uncertain excluded by policy: ",
              if(is.null(z$policy_uncertain_excluded))"not available" else
                z$policy_uncertain_excluded)
    }
  }
  if(!is.null(state$w07$run)){
    p<-file.path(state$w07$run,"summary.json")
    if(file.exists(p)){
      z<-jsonlite::fromJSON(p)
      message("  W07: ",if(is.null(z$records))"not available" else z$records,
              " publications mechanism-coded")
    }
  }
  if(!is.null(state$w08$run))
    message("  W08: ",state$case_bearing_publications,
            " case-bearing publications from ",state$assessed_publications,
            " assessments; ",state$uncertain_case_publications," uncertain.")
  if(!is.null(state$decision2))
    message("  Checkpoint 2: ",state$decision2$selected," of ",
            state$decision2$population," publications selected for extraction; uncertain=",
            state$decision2$uncertain_policy)
  if(!is.null(state$w09$ready_count))
    message("  W09: ",state$w09$ready_count,
            " publications with validated extraction-ready documents.")
  if(!is.null(state$published))
    message("  Atlas: ",state$published$atlas,
            "\n  Full review: ",state$published$full_report)
  if(!is.null(state$blocking_stage))
    message("  Stopped at: ",state$blocking_stage)
  invisible(state)
}
