#' Consolidate a random Ostara full-text mechanism pilot into one database.
#'
#' Reads existing local W09 checkpoint ledgers without any API calls.
#' The historical 10%/90% source split is not reflected in final output tables.
#' Missing records remain explicitly pending.
#' @param map_path Ostara project directory, default registered map.
#' @param pilot_dir Exact pilot directory, or NULL for the only/latest pilot.
#' @return Directory with consolidated database and CSV tables, invisibly.
#' @export
consolidate_ostara_fulltext_mechanism_pilot <- function(map_path=NULL,pilot_dir=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  root<-file.path(map_path,"runs","W09","random_fulltext_mechanism_pilots")
  if(is.null(pilot_dir)) {
    dirs<-list.dirs(root,recursive=FALSE,full.names=TRUE)
    dirs<-dirs[file.exists(file.path(dirs,"sample_manifest.json"))]
    if(!length(dirs))stop("No random mechanism pilot manifest found")
    dirs<-dirs[order(file.info(file.path(dirs,"sample_manifest.json"))$mtime,
                     decreasing=TRUE)]
    pilot_dir<-dirs[[1L]]
  }
  pilot_dir<-normalizePath(pilot_dir,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  manfile<-file.path(pilot_dir,"sample_manifest.json")
  csvfile<-file.path(pilot_dir,"random_sample.csv")
  if(!all(file.exists(c(manfile,csvfile))))stop("Pilot sample files missing")
  m<-jsonlite::fromJSON(manfile,simplifyVector=FALSE)
  if(!identical(m$schema,"ostara-random-multicohort-fulltexts-v1"))
    stop("Unsupported pilot sample schema")
  sample<-utils::read.csv(csvfile,stringsAsFactors=FALSE,
                          colClasses="character",check.names=FALSE)
  if(!identical(names(sample),c("record_id","w09","source_file"))||
     nrow(sample)!=as.integer(m$sample_size)||anyNA(sample)||
     anyDuplicated(sample$record_id))stop("Invalid pilot sample CSV")
  if(length(m$records)!=nrow(sample))stop("Manifest/sample size mismatch")
  for(i in seq_len(nrow(sample))) {
    z<-m$records[[i]]
    if(!identical(as.character(z$record_id),sample$record_id[[i]])||
       !identical(as.character(z$w09),sample$w09[[i]])||
       !identical(as.character(z$source_file),sample$source_file[[i]]))
      stop("Sample and manifest disagree at row ",i)
  }
  results<-list(); source_ledgers<-list()
  for(w09 in unique(sample$w09)) {
    parent<-file.path(pilot_dir,paste0("cohort-",basename(w09)))
    if(!dir.exists(parent))next
    runs<-list.dirs(parent,recursive=FALSE,full.names=TRUE)
    runs<-runs[file.exists(file.path(runs,"manifest.json"))]
    if(length(runs)!=1L)stop("Expected one checkpoint directory in ",parent)
    sub<-jsonlite::fromJSON(file.path(runs,"manifest.json"),simplifyVector=FALSE)
    if(!identical(as.character(sub$prompt_sha256),as.character(m$prompt_sha256))||
       !identical(as.character(sub$approved_ontology_sha256),as.character(m$ontology_sha256))||
       !identical(as.character(sub$model),as.character(m$model))||
       !identical(as.character(sub$source_w09),w09))
      stop("Checkpoint prompt, ontology or source provenance mismatch")
    ledger<-file.path(runs,"mechanism_assessments.jsonl")
    source_ledgers[[length(source_ledgers)+1L]]<-list(
      path=ledger,sha256=if(file.exists(ledger))sha(ledger) else NULL)
    if(!file.exists(ledger))next
    lines<-readLines(ledger,warn=FALSE,encoding="UTF-8")
    for(line in lines[nzchar(trimws(lines))]) {
      z<-jsonlite::fromJSON(line,simplifyVector=FALSE)
      id<-as.character(z$record_id)
      i<-match(id,sample$record_id)
      if(is.na(i)||!identical(sample$w09[[i]],w09))
        stop("Unexpected record/source cohort in checkpoint: ",id)
      if(!is.null(results[[id]]))stop("Duplicate record: ",id)
      if(!is.list(z$mechanism_inventory)||!is.list(z$new_novelty)||
         !is.list(z$case_presence_assessment)||is.null(z$provenance)||
         !identical(as.character(z$provenance$prompt_sha256),as.character(m$prompt_sha256))||
         !identical(as.character(z$provenance$approved_ontology_sha256),
                    as.character(m$ontology_sha256)))
        stop("Invalid extraction or prompt/ontology mismatch: ",id)
      if(!file.exists(sample$source_file[[i]])||
         !identical(sha(sample$source_file[[i]]),
                    as.character(z$provenance$source_sha256)))
        stop("Changed/missing source full text for ",id)
      results[[id]]<-z
    }
  }
  ids<-sample$record_id
  done<-ids[ids%in%names(results)]
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  rows<-function(kind) {
    out<-list()
    for(id in done) {
      z<-results[[id]]
      if(kind=="mechanism_inventory")for(v in z$mechanism_inventory)
        out[[length(out)+1L]]<-data.frame(record_id=id,
          mechanism_id=scalar(v$mechanism_id),status=scalar(v$status),
          source_quote=scalar(v$source_quote),source_anchor=scalar(v$source_anchor),
          reason=scalar(v$reason),stringsAsFactors=FALSE)
      if(kind=="new_novelty")for(v in z$new_novelty)
        out[[length(out)+1L]]<-data.frame(record_id=id,
          proposed_label=scalar(v$proposed_label),
          working_definition=scalar(v$working_definition),
          support_status=scalar(v$support_status),
          closest_approved_mechanism_id=scalar(v$closest_approved_mechanism_id),
          reason=scalar(v$reason),source_quote=scalar(v$source_quote),
          source_anchor=scalar(v$source_anchor),stringsAsFactors=FALSE)
      if(kind=="case_presence") {
        c<-z$case_presence_assessment
        out[[length(out)+1L]]<-data.frame(record_id=id,
          case_status=scalar(c$case_status),
          candidate_case_count=if(is.null(c$candidate_case_count))NA_integer_
                               else as.integer(c$candidate_case_count),
          assessment_reason=scalar(c$assessment_reason),stringsAsFactors=FALSE)
      }
      if(kind=="candidate_cases")for(v in z$case_presence_assessment$candidate_cases)
        out[[length(out)+1L]]<-data.frame(record_id=id,
          candidate_label=scalar(v$candidate_label),
          case_description=scalar(v$case_description),
          implementation_status=scalar(v$implementation_status),
          source_quote=scalar(v$source_quote),
          source_anchor=scalar(v$source_anchor),
          eligibility_reason=scalar(v$eligibility_reason),stringsAsFactors=FALSE)
    }
    fields<-switch(kind,
      mechanism_inventory=c("record_id","mechanism_id","status","source_quote","source_anchor","reason"),
      new_novelty=c("record_id","proposed_label","working_definition","support_status",
                    "closest_approved_mechanism_id","reason","source_quote","source_anchor"),
      case_presence=c("record_id","case_status","candidate_case_count","assessment_reason"),
      candidate_cases=c("record_id","candidate_label","case_description",
                        "implementation_status","source_quote","source_anchor","eligibility_reason"))
    if(!length(out))return(as.data.frame(setNames(
      rep(list(character()),length(fields)),fields),stringsAsFactors=FALSE))
    do.call(rbind,out)
  }
  tables<-lapply(c("mechanism_inventory","new_novelty","case_presence",
                   "candidate_cases"),rows)
  names(tables)<-c("mechanism_inventory","new_novelty","case_presence",
                   "candidate_cases")
  tables$publication_status<-data.frame(record_id=ids,
     extraction_status=ifelse(ids%in%done,"validated","not_yet_validated"),
     source_file=sample$source_file,stringsAsFactors=FALSE)
  # Write via temporary files before replacing existing consolidated copies.
  save_csv<-function(x,path) {
    tmp<-tempfile(pattern=".ostara-",tmpdir=dirname(path),fileext=".csv")
    on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
    utils::write.csv(x,tmp,row.names=FALSE,na="")
    if(file.exists(path))unlink(path)
    if(!file.rename(tmp,path))stop("Failed to write ",path)
  }
  for(nm in names(tables))
    save_csv(tables[[nm]],file.path(pilot_dir,paste0(nm,".csv")))
  database<-file.path(pilot_dir,"mechanism_database.jsonl")
  tmp<-tempfile(pattern=".ostara-",tmpdir=pilot_dir)
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  lines<-vapply(done,function(id)as.character(jsonlite::toJSON(results[[id]],
                 auto_unbox=TRUE,null="null",digits=NA)),character(1))
  writeLines(lines,tmp,useBytes=TRUE)
  if(file.exists(database))unlink(database)
  if(!file.rename(tmp,database))stop("Could not write unified JSONL database")
  provenance<-list(schema="ostara-unified-mechanism-pilot-v1",
    sample_manifest_sha256=sha(manfile),
    selected_publications=length(ids),validated_publications=length(done),
    missing_publications=as.list(setdiff(ids,done)),
    prompt_sha256=m$prompt_sha256,ontology_sha256=m$ontology_sha256,
    model=m$model,source_ledgers=source_ledgers,database_sha256=sha(database))
  jsonlite::write_json(provenance,file.path(pilot_dir,"consolidated_manifest.json"),
                       pretty=TRUE,auto_unbox=TRUE)
  message("[FT mechanism database] ",length(done),"/",length(ids),
    " validated publications, ",nrow(tables$mechanism_inventory),
    " mechanism rows, one database at ",database,
    ". Zero model calls.")
  invisible(pilot_dir)
}
