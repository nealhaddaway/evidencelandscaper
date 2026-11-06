#' Review cases detected only by independent full-text assessment
#'
#' Launch a packaged Shiny app, without downloading scripts or selecting JSON
#' manifests. Identifies authoritative W08 human statuses, compares them to
#' versioned full-text mechanism case-presence assessments, and saves separate
#' human decisions. This does not initiate any LLM extraction.
#'
#' @param map_path OSTARA map path (default: named map).
#' @param mechanism_runs Optional named list with first10 and remaining90 run
#'   directories. If omitted, choose amongst completed/partial saved runs.
#' @return Local reviewer directory, invisibly, on app exit.
#' @export
review_ostara_fulltext_case_discrepancies <- function(map_path=NULL,
                                                       mechanism_runs=NULL,launch=TRUE) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  root<-file.path(map_path,"runs","W09")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    if(!file.exists(p))return(list())
    lines<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sc<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse="; ")
  w08root<-file.path(map_path,"runs","W08")
  w08dirs<-list.dirs(w08root,recursive=FALSE,full.names=TRUE)
  w08dirs<-w08dirs[file.exists(file.path(w08dirs,"manifest.json"))]
  get_manifest<-function(path)jsonlite::fromJSON(path,simplifyVector=FALSE)
  w08_dirs<-list()
  for(d in w08dirs) {
    m<-get_manifest(file.path(d,"manifest.json"))
    n<-length(unlist(m$selected_record_ids,use.names=FALSE))
    if(n%in%c(266L,2440L)) {
      label<-if(n==266L)"first10" else "remaining90"
      if(!is.null(w08_dirs[[label]]))
        stop("Ambiguous W08 ",label," sources. Resolve duplicate cohorts first.")
      w08_dirs[[label]]<-d
    }
  }
  if(!all(c("first10","remaining90")%in%names(w08_dirs)))
    stop("Authoritative W08 first10 and remaining90 manifests not found.")
  statuses<-list()
  for(cohort in c("first10","remaining90")) {
    d<-w08_dirs[[cohort]]
    human_root<-file.path(d,"human_adjudication")
    human_dirs<-if(dir.exists(human_root))list.dirs(human_root,
      recursive=FALSE,full.names=TRUE) else character()
    human_dirs<-human_dirs[file.exists(file.path(human_dirs,"manifest.json"))&
       file.exists(file.path(human_dirs,"effective_case_classifications.csv"))]
    if(length(human_dirs)!=1L)
      stop("Exactly one W08 human-adjudicated snapshot required for ",cohort)
    csv<-file.path(human_dirs,"effective_case_classifications.csv")
    hm<-get_manifest(file.path(human_dirs,"manifest.json"))
    if(!identical(hm$effective_case_classifications_sha256,sha(csv)))
      stop("W08 human snapshot checksum mismatch: ",csv)
    x<-utils::read.csv(csv,stringsAsFactors=FALSE)
    if(!all(c("record_id","final_case_status")%in%names(x))||
       anyDuplicated(x$record_id))stop("Invalid W08 human classification file: ",csv)
    if(cohort=="remaining90") {
      final_dirs<-list.dirs(w08root,recursive=FALSE,full.names=TRUE)
      final_files<-file.path(final_dirs,"final_case_classifications_2440.csv")
      final_files<-final_files[file.exists(final_files)]
      if(length(final_files)!=1L)stop("Expected one remaining90 final status CSV.")
      fin<-utils::read.csv(final_files,stringsAsFactors=FALSE)
      if(nrow(fin)!=2440L||anyDuplicated(fin$record_id)||
         !all(c("record_id","final_case_status")%in%names(fin)))
        stop("Invalid remaining90 final status CSV.")
      idx<-match(x$record_id,fin$record_id)
      if(anyNA(idx)||!identical(as.character(x$final_case_status),
                               as.character(fin$final_case_status[idx])))
        stop("Human snapshot and remaining90 final statuses disagree.")
      x<-fin
    }
    statuses[[cohort]]<-x
  }
  # Identify versioned mechanism runs by manifest source W09 path, never by
  # file modification time. User chooses explicitly if multiple are present.
  files<-list.files(root,pattern="^manifest\\.json$",recursive=TRUE,full.names=TRUE)
  candidates<-list()
  for(f in files) {
    m<-tryCatch(get_manifest(f),error=function(e)NULL)
    if(is.null(m)||!identical(m$schema,"ostara-fulltext-mechanism-all-v4"))
      next
    directory<-dirname(f)
    ledger<-file.path(directory,"mechanism_assessments.jsonl")
    if(!file.exists(ledger))next
    from<-sc(m$source_w09)
    cohort<-if(grepl("first10",from,fixed=TRUE))"first10" else
      if(grepl("remaining90",from,fixed=TRUE))"remaining90" else ""
    if(!nzchar(cohort))next
    candidates[[cohort]]<-c(candidates[[cohort]],directory)
  }
  chosen<-list()
  for(cohort in c("first10","remaining90")) {
    options<-unique(candidates[[cohort]])
    if(!is.null(mechanism_runs)) {
      selected<-mechanism_runs[[cohort]]
      if(is.null(selected)||!selected%in%options)
        stop("Supplied mechanism run does not match saved ",cohort," candidates.")
    } else {
      if(!length(options))
        stop("No saved full-text mechanism assessments for ",cohort)
      if(length(options)>1L) {
        labels<-vapply(options,function(p) {
          m<-get_manifest(file.path(p,"manifest.json"))
          paste0(basename(p)," | prompt ",
                 substr(sc(m$prompt_sha256),1,12)," | ontology ",
                 substr(sc(m$approved_ontology_sha256),1,12))
        },character(1))
        if(!interactive())
          stop("Multiple mechanism runs for ",cohort,"; specify mechanism_runs.")
        index<-utils::menu(labels,title=paste("Select reviewed",cohort,
            "mechanism inventory (use approved prompt)"))
        if(index<1L)stop("No full-text mechanism run selected.")
        selected<-options[[index]]
      } else selected<-options[[1L]]
    }
    chosen[[cohort]]<-normalizePath(selected,winslash="/",mustWork=TRUE)
  }
  # Build independent review queue as publication rows, preserving original
  # full-text evidence and W08 source status verbatim.
  out<-list()
  for(cohort in c("first10","remaining90")) {
    dir<-chosen[[cohort]]
    m<-get_manifest(file.path(dir,"manifest.json"))
    entries<-readjl(file.path(dir,"mechanism_assessments.jsonl"))
    ids<-vapply(entries,function(x)sc(x$record_id),character(1))
    if(anyDuplicated(ids))stop("Duplicate full-text mechanism IDs: ",dir)
    statuses_df<-statuses[[cohort]]
    w08_manifest<-get_manifest(file.path(w08_dirs[[cohort]],"manifest.json"))
    w08_selected_ids<-as.character(unlist(w08_manifest$selected_record_ids,
                                            use.names=FALSE))
    if(anyDuplicated(w08_selected_ids)||
       any(!statuses_df$record_id%in%w08_selected_ids))
      stop("W08 human classifications are inconsistent with selected records: ",cohort)
    queue_source<-file.path(sc(m$source_w09),"retrieval_queue.csv")
    if(!file.exists(queue_source)||!identical(sha(queue_source),
                                            sc(m$w09_queue_sha256)))
      stop("Full-text run W09 queue provenance mismatch: ",dir)
    q<-utils::read.csv(queue_source,stringsAsFactors=FALSE)
    if(anyDuplicated(q$record_id))stop("Duplicate queue record IDs.")
    for(z in entries) {
      st<-sc(z$case_presence_assessment$case_status)
      if(!identical(st,"case_present"))next
      id<-sc(z$record_id)
      row<-match(id,statuses_df$record_id)
      # W09 included records can extend beyond W08's sampled or validated
      # population. A missing W08 classification is NOT a negative decision.
      # Distinguish selected-but-unvalidated from never selected at W08.
      old<-if(!is.na(row))sc(statuses_df$final_case_status[[row]]) else
        if(id%in%w08_selected_ids)"unvalidated_W08" else "not_assessed_W08"
      if(old%in%c("specific_case","multiple_cases"))next
      if(!old%in%c("no_identifiable_case","unclear",
                   "unvalidated_W08","not_assessed_W08"))
        stop("Unexpected W08 status for full-text new case: ",old)
      qi<-match(id,q$record_id)
      if(is.na(qi))stop("Full-text ID absent from retrieval queue: ",id)
      cases<-z$case_presence_assessment$candidate_cases
      flagged<-Filter(function(v)identical(sc(v$location),"Case presence candidate"),
                      z$evidence_review_flags)
      out[[length(out)+1L]]<-list(
        record_id=id,cohort=cohort,w08_status=old,ft_case_status=st,
        title=sc(q$title[[qi]]),doi=sc(q$doi[[qi]]),
        candidate_case_count=z$case_presence_assessment$candidate_case_count,
        assessment_reason=sc(z$case_presence_assessment$assessment_reason),
        candidate_cases=cases,evidence_review_flags=flagged,
        fulltext_evidence_review_required=length(flagged)>0L,
        mechanism_run=dir,mechanism_manifest_sha256=sha(file.path(dir,"manifest.json")),
        mechanism_ledger_sha256=sha(file.path(dir,"mechanism_assessments.jsonl")),
        source_w09=sc(m$source_w09))
    }
  }
  if(!length(out))
    stop("No new full-text case-positive discrepancies yet in chosen checkpoints.")
  ids<-vapply(out,function(x)x$record_id,character(1))
  if(anyDuplicated(ids))stop("Cross-cohort duplicated publication IDs.")
  # Version the queue by the two immutable input manifests, not by how many
  # records have been assessed; preserve event history in a stable review home.
  review_dir<-file.path(root,"fulltext_case_discrepancy_human_review")
  dir.create(review_dir,recursive=TRUE,showWarnings=FALSE)
  signature<-substr(digest::digest(c(
    vapply(chosen,function(p)sha(file.path(p,"manifest.json")),character(1)),
    vapply(w08_dirs,function(p)sha(file.path(p,"manifest.json")),character(1))),
    algo="sha256"),1,12)
  queue_file<-file.path(review_dir,paste0("review_queue-",signature,".jsonl"))
  lines<-vapply(out,function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,
    null="null")),character(1))
  # The inventory can still be running: only publish changed snapshots, never
  # overwrite an old review queue with different content.
  if(file.exists(queue_file)) {
    if(!identical(readLines(queue_file,warn=FALSE,encoding="UTF-8"),unname(lines))) {
      queue_file<-file.path(review_dir,paste0("review_queue-",signature,"-",
        substr(digest::digest(lines,algo="sha256"),1,12),".jsonl"))
    }
  }
  if(!file.exists(queue_file))writeLines(unname(lines),queue_file,useBytes=TRUE)
  if(!isTRUE(launch))return(invisible(list(queue_file=queue_file,review_dir=review_dir,records=length(out),mechanism_runs=chosen)))
  script<-system.file("ostara","case_discrepancy_app.R",
                      package="evidencelandscaper")
  if(!nzchar(script))stop("Packaged OSTARA Shiny reviewer not installed.")
  if(!requireNamespace("shiny",quietly=TRUE))
    stop("Install the shiny package to launch case review.")
  message("[OSTARA case review] ",length(out)," full-text-only case-positive candidates.")
  message("[OSTARA case review] State: ",review_dir)
  status<-system2(file.path(R.home("bin"),"Rscript"),
    args=vapply(c(script,queue_file,review_dir),shQuote,character(1)),
    wait=TRUE)
  if(!identical(as.integer(status),0L))
    stop("OSTARA case reviewer exited unsuccessfully.")
  invisible(review_dir)
}
