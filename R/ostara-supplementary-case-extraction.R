#' Extract supplementary candidate cases from unextracted journal full texts
#'
#' Uses the completed all-available mechanism run, subtracts ALL validated
#' publications in the locked case extraction and subsequently checkpointed
#' case runs, then runs the unchanged locked case prompt/schema. W08 status is
#' retained for provenance, not used to discard never-extracted publications.
#' Does not overwrite the original case database.
#' @param map_path OSTARA project root.
#' @param mechanism_dir The new all-available mechanism extraction folder.
#' @param max_records Maximum new case-data calls, 0 for all.
#' @return New supplementary case extraction directory, invisibly.
#' @export
run_ostara_supplementary_case_extraction<-function(
    map_path=NULL,mechanism_dir=NULL,max_records=0L) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  norm<-function(x)normalizePath(x,winslash="/",mustWork=TRUE)
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  csv<-function(x)utils::read.csv(x,stringsAsFactors=FALSE,
                                  check.names=FALSE,fileEncoding="UTF-8-BOM")
  readjl<-function(p) {
    if(!file.exists(p))return(list())
    l<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer")
  map_path<-norm(map_path)
  if(is.null(mechanism_dir))
    mechanism_dir<-file.path(map_path,"runs","W09",
      "fulltext_mechanism_all_available","all-906-c7bc72ce01cb8025")
  mechanism_dir<-norm(mechanism_dir)
  mf<-norm(file.path(mechanism_dir,"pool_manifest.json"))
  mechanism_file<-norm(file.path(mechanism_dir,"ostara_fulltext_mechanisms.csv"))
  source_file<-norm(file.path(mechanism_dir,"source_pool.csv"))
  m<-jsonlite::fromJSON(mf,simplifyVector=FALSE)
  tab<-csv(mechanism_file)
  pool<-csv(source_file)
  if(!identical(m$schema,"ostara-fulltext-all-available-v1")||
     !all(c("record_id","publication_status","case_status",
            "candidate_case_count","source_sha256")%in%names(tab))||
     !all(c("record_id","title","doi","original_source",
            "source_sha256")%in%names(pool))||
     anyNA(pool$record_id)||anyDuplicated(pool$record_id)||
     nrow(pool)!=as.integer(m$selected_count)||
     !setequal(tab$record_id,pool$record_id)||
     !setequal(as.character(unlist(m$selected_record_ids,use.names=FALSE)),
               pool$record_id))
    stop("Mechanism source pool, results and manifest do not agree")
  versions<-list.dirs(mechanism_dir,recursive=FALSE,full.names=TRUE)
  versions<-versions[grepl("^v4-",basename(versions))&
                       file.exists(file.path(versions,"manifest.json"))]
  if(length(versions)!=1L)
    stop("Expected exactly one versioned mechanism checkpoint")
  vm<-jsonlite::fromJSON(file.path(versions,"manifest.json"),
                         simplifyVector=FALSE)
  if(!identical(vm$prompt_sha256,m$prompt_sha256)||
     !identical(vm$approved_ontology_sha256,m$ontology_sha256))
    stop("Mechanism inventory and source pool differ in prompt/ontology")
  for(id in unique(tab$record_id)) {
    a<-tab[tab$record_id==id,,drop=FALSE]
    for(col in c("publication_status","case_status",
                 "candidate_case_count","source_sha256"))
      if(length(unique(as.character(a[[col]])))!=1L)
        stop("Inconsistent case candidate assessment for publication ",id)
  }
  candidate<-unique(as.character(tab$record_id[
    tab$publication_status=="validated"&tab$case_status=="case_present"&
    !is.na(tab$candidate_case_count)&tab$candidate_case_count>0L]))
  if(!length(candidate))stop("No validated full-text publications with candidate cases")
  locked<-file.path(map_path,"reports","ostara_case_fulltext_locked")
  old_status_path<-norm(file.path(locked,"combined_publication_status.csv"))
  old_cases_path<-norm(file.path(locked,"OSTARA_all_case_data.csv"))
  old_status<-csv(old_status_path)
  old_cases<-csv(old_cases_path)
  if(!all(c("record_id","extraction_status","case_count")%in%names(old_status))||
     !all(c("record_id","case_id")%in%names(old_cases))||
     anyDuplicated(old_status$record_id)||anyDuplicated(old_cases$case_id)||
     any(!old_cases$record_id%in%old_status$record_id)||
     any(!old_status$extraction_status%in%c("validated","unresolved"))||
     sum(old_status$case_count,na.rm=TRUE)!=nrow(old_cases))
    stop("Locked historical case database/status fails consistency checks")
  done<-as.character(old_status$record_id[
    old_status$extraction_status=="validated"])
  # Exclude other already validated case extractions as well, including those
  # with zero cases, to prevent duplicate charged extraction work.
  w09root<-file.path(map_path,"runs","W09")
  dirs<-list.dirs(w09root,recursive=FALSE,full.names=TRUE)
  checked_ledgers<-character()
  for(d in dirs) {
    case_root<-file.path(d,"case_data_extraction")
    if(!dir.exists(case_root))next
    p<-list.dirs(case_root,recursive=FALSE,full.names=TRUE)
    for(x in p) {
      manifest<-file.path(x,"manifest.json")
      ledger<-file.path(x,"case_data.jsonl")
      if(!file.exists(manifest)||!file.exists(ledger))next
      info<-jsonlite::fromJSON(manifest,simplifyVector=FALSE)
      if(!identical(info$schema,"ostara-w09-csv-case-data-extraction-v1"))next
      z<-readjl(ledger)
      zids<-vapply(z,function(a)as.character(a$record_id),character(1))
      if(anyDuplicated(zids))stop("Duplicate completed case extraction IDs: ",ledger)
      done<-c(done,zids)
      checked_ledgers<-c(checked_ledgers,ledger)
    }
  }
  done<-unique(done)
  selected<-sort(setdiff(candidate,done))
  if(!length(selected))stop("All candidate-case publications have already been case-extracted")
  # Human-adjudicated W08 status is descriptive; abstract-positive but
  # never-extracted publications remain in the new queue.
  w08root<-file.path(map_path,"runs","W08")
  w08files<-c(
    file.path(w08root,"sample-266-seed-20261023-f83478f861-5997a8f147-model-gpt-5.6-luna",
      "human_adjudication","v1-b7a30eb3123c","effective_case_classifications.csv"),
    file.path(w08root,"remaining90-final-case-export-3d194580b61a",
      "final_case_classifications_2440.csv"))
  if(!all(file.exists(w08files)))stop("Missing authoritative W08 classification CSV")
  decisions<-do.call(rbind,lapply(w08files,function(path) {
    z<-csv(path)
    if(!all(c("record_id","final_case_status")%in%names(z))||
       anyDuplicated(z$record_id))stop("Invalid W08 classifications: ",path)
    z[,c("record_id","final_case_status"),drop=FALSE]
  }))
  if(anyDuplicated(decisions$record_id))stop("W08 decision IDs are not unique")
  src<-pool[match(selected,pool$record_id),,drop=FALSE]
  if(anyNA(src$record_id)||any(!file.exists(src$original_source)))
    stop("Supplementary full-text sources are missing")
  hashes<-vapply(src$original_source,sha,character(1))
  if(!identical(unname(hashes),as.character(src$source_sha256)))
    stop("Source full texts have changed since mechanism extraction")
  w08status<-as.character(decisions$final_case_status[
    match(selected,decisions$record_id)])
  w08status[is.na(w08status)]<-"missing_final_W08_classification"
  route<-ifelse(w08status%in%c("specific_case","multiple_cases"),
    "abstract_positive_not_previously_extracted",
    "newly_detected_after_abstract_screening")
  queue<-data.frame(record_id=selected,title=src$title,doi=src$doi,
    final_abstract_case_status=w08status,supplementary_route=route,
    candidate_case_count=as.integer(tab$candidate_case_count[
      match(selected,tab$record_id)]),
    original_source=src$original_source,source_sha256=src$source_sha256,
    stringsAsFactors=FALSE)
  pilot<-norm(file.path(map_path,"reports","ostara_case_pilots",
                         "case_pilot_20_20261113"))
  locked_manifest<-jsonlite::fromJSON(
    file.path(pilot,"batch_manifest.json"),simplifyVector=FALSE)
  prompt<-norm(file.path(pilot,"prompt.txt"))
  schema<-norm(file.path(pilot,"case_schema.csv"))
  if(!identical(sha(prompt),locked_manifest$staged_prompt_sha256)||
     !identical(sha(schema),locked_manifest$schema_sha256))
    stop("Locked case-data prompt/schema provenance mismatch")
  key<-substr(digest::digest(c(sha(mechanism_file),sha(source_file),
    sha(old_status_path),sha(old_cases_path),vapply(w08files,sha,character(1)),
    sha(prompt),sha(schema),selected,src$source_sha256),
    algo="sha256"),1L,16L)
  base<-file.path(w09root,"supplementary_case_extraction",
                   paste0("supplementary-",length(selected),"-",key))
  snapshot_file<-file.path(base,"selection_manifest.json")
  snapshot<-list(schema="ostara-supplementary-case-data-v1",
    mechanism_csv_sha256=sha(mechanism_file),source_pool_sha256=sha(source_file),
    locked_case_status_sha256=sha(old_status_path),
    locked_case_database_sha256=sha(old_cases_path),
    w08_sha256=as.list(vapply(w08files,sha,character(1))),
    previous_checkpoint_sha256=as.list(vapply(checked_ledgers,sha,character(1))),
    prompt_sha256=sha(prompt),schema_sha256=sha(schema),
    selected_ids=as.list(selected),source_sha256=as.list(src$source_sha256))
  if(dir.exists(base)&&!file.exists(snapshot_file))
    stop("Supplementary output directory exists without manifest")
  dir.create(base,recursive=TRUE,showWarnings=FALSE)
  if(file.exists(snapshot_file)) {
    old<-jsonlite::fromJSON(snapshot_file,simplifyVector=FALSE)
    if(!identical(old,snapshot))stop("Existing supplementary selection changed")
  } else jsonlite::write_json(snapshot,snapshot_file,pretty=TRUE,auto_unbox=TRUE)
  selection_file<-file.path(base,"supplementary_selection.csv")
  if(file.exists(selection_file)) {
    prior<-csv(selection_file)
    if(!identical(prior,queue))stop("Existing selection register differs")
  } else utils::write.csv(queue,selection_file,row.names=FALSE)
  stage<-file.path(base,"source_fulltexts")
  dir.create(file.path(stage,"structured"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(stage,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  q<-queue[,c("record_id","title","doi"),drop=FALSE]
  target_queue<-file.path(stage,"retrieval_queue.csv")
  if(file.exists(target_queue)) {
    prior<-csv(target_queue)
    if(!identical(prior,q))stop("Staged queue changed")
  } else utils::write.csv(q,target_queue,row.names=FALSE)
  for(i in seq_len(nrow(src))) {
    ext<-tolower(tools::file_ext(src$original_source[[i]]))
    if(!ext%in%c("xml","json","pdf"))stop("Unexpected source format")
    target<-file.path(stage,if(ext=="pdf")"pdfs" else "structured",
                      paste0(selected[[i]],".",ext))
    if(!file.exists(target)&&!file.symlink(src$original_source[[i]],target))
      stop("Could not stage supplementary full text: ",selected[[i]])
    if(!identical(sha(target),src$source_sha256[[i]]))
      stop("Staged source bytes mismatch: ",selected[[i]])
  }
  message("[Supplementary cases] ",length(candidate),
    " mechanism-positive FTs; ",length(intersect(candidate,done)),
    " already extracted; ",length(selected)," to extract; ",
    sum(route=="abstract_positive_not_previously_extracted"),
    " previously abstract-positive but not extracted.")
  ex<-run_w09_case_data_extraction(
    w09=stage,prompt_file=prompt,schema_file=schema,
    record_ids=selected,max_records=max_records,
    model="gpt-5.6-luna",max_pages=0L,revalidate_only=FALSE)
  data<-readjl(file.path(ex,"case_data.jsonl"))
  completed<-vapply(data,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(completed)||any(!completed%in%selected))
    stop("Unexpected supplementary case checkpoint IDs")
  report<-queue
  report$extraction_status<-ifelse(report$record_id%in%completed,
                                   "validated","outstanding")
  report$extracted_case_count<-NA_integer_
  for(z in data)
    report$extracted_case_count[match(z$record_id,report$record_id)]<-length(z$cases)
  utils::write.csv(report,file.path(base,"supplementary_publication_status.csv"),
                   row.names=FALSE,na="")
  if(length(completed)) {
    wide<-export_w09_cases_wide_evidence(
      w09=stage,extraction_dir=ex,include_status=TRUE)
    if(!file.copy(wide,file.path(base,"OSTARA_supplementary_case_data.csv"),
                  overwrite=TRUE))
      stop("Unable to save supplementary case-data CSV")
  }
  message("[Supplementary cases] ",length(completed),"/",length(selected),
    " validated; ",sum(report$extracted_case_count,na.rm=TRUE),
    " extracted candidate cases. Output: ",base)
  invisible(base)
}
