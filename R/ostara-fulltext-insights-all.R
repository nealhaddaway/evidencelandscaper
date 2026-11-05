#' Run revised independent full-text Insights across both OSTARA cohorts
#'
#' Stages previously downloaded full texts from W09 queues into separate
#' read-only-by-convention locations. No retrieval or source overwriting.
#' Queue identities must match title/DOI/record_id. Conflicting source
#' checksums stop the run and are reported. Does not inspect W08 classifications
#' or case/mechanism extraction results.
#'
#' @param map_path OSTARA project directory.
#' @param prompt_file Explicit revised Insights TXT.
#' @param ontology_file Explicit approved 30-code ontology CSV.
#' @param max_records Maximum new assessments per cohort; 0 means all.
#' @return Named list with two checkpoint directories, invisibly.
#' @export
run_ostara_downloaded_fulltext_insights <- function(
    map_path,prompt_file,ontology_file,max_records=0L) {
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  root<-file.path(map_path,"runs","W09")
  if(!dir.exists(root))stop("W09 root does not exist: ",root)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  dirs<-list.dirs(root,recursive=FALSE,full.names=TRUE)
  dirs<-dirs[file.exists(file.path(dirs,"retrieval_queue.csv"))]
  get_one<-function(prefix) {
    x<-dirs[startsWith(basename(dirs),prefix)]
    if(length(x)!=1L)
      stop("Expected exactly one ",prefix," source queue; found ",length(x),
           ". Prepare the authoritative W09 all-included queue first.")
    x[[1L]]
  }
  queues<-list(first10=get_one("first10-all-included-"),
               remaining90=get_one("remaining90-all-included-"))
  getq<-function(p) {
    q<-utils::read.csv(file.path(p,"retrieval_queue.csv"),
                       stringsAsFactors=FALSE)
    if(!all(c("record_id","title","doi")%in%names(q))||
       anyNA(q$record_id)||anyDuplicated(q$record_id))
      stop("Invalid W09 queue: ",p)
    q
  }
  norm_title<-function(x) {
    if(is.na(x))return("")
    tolower(gsub("\\s+"," ",trimws(x)))
  }
  norm_doi<-function(x) {
    if(is.na(x))return("")
    x<-tolower(trimws(x))
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x)
    sub("^doi:\\s*","",x)
  }
  source_queues<-lapply(dirs,getq)
  control<-file.path(root,"fulltext_insights_v2_control")
  dir.create(control,recursive=TRUE,showWarnings=FALSE)
  manifests<-list()
  selections<-list()
  staged<-list()
  issues<-list()
  for(cohort in names(queues)) {
    original<-queues[[cohort]]
    q<-getq(original)
    stage<-file.path(root,paste0("ft-insights-stage-",cohort))
    dir.create(file.path(stage,"structured"),recursive=TRUE,showWarnings=FALSE)
    dir.create(file.path(stage,"pdfs"),recursive=TRUE,showWarnings=FALSE)
    q_source<-file.path(original,"retrieval_queue.csv")
    q_target<-file.path(stage,"retrieval_queue.csv")
    if(!file.exists(q_target)&&!file.copy(q_source,q_target,overwrite=FALSE))
      stop("Cannot stage queue: ",cohort)
    if(!identical(sha(q_source),sha(q_target)))
      stop("Staged queue no longer matches original: ",cohort)
    formats<-list(c("structured","xml"),c("structured","json"),c("pdfs","pdf"))
    for(i in seq_len(nrow(q))) {
      id<-as.character(q$record_id[[i]])
      title<-norm_title(q$title[[i]])
      doi<-norm_doi(q$doi[[i]])
      for(format in formats) {
        folder<-format[[1L]]
        extension<-format[[2L]]
        destination<-file.path(stage,folder,paste0(id,".",extension))
        candidate_paths<-character()
        for(j in seq_along(dirs)) {
          sq<-source_queues[[j]]
          k<-match(id,sq$record_id)
          if(is.na(k)||!identical(norm_title(sq$title[[k]]),title)||
             !identical(norm_doi(sq$doi[[k]]),doi))next
          candidate<-file.path(dirs[[j]],folder,paste0(id,".",extension))
          if(file.exists(candidate))
            candidate_paths<-c(candidate_paths,candidate)
        }
        if(file.exists(destination))
          candidate_paths<-c(candidate_paths,destination)
        candidate_paths<-unique(candidate_paths)
        if(!length(candidate_paths))next
        hashes<-vapply(candidate_paths,sha,character(1))
        if(length(unique(hashes))>1L) {
          issues[[length(issues)+1L]]<-data.frame(
            cohort=cohort,record_id=id,format=extension,
            candidate_paths=paste(candidate_paths,collapse=" | "),
            checksums=paste(hashes,collapse=" | "),
            stringsAsFactors=FALSE)
          next
        }
        if(!file.exists(destination)) {
          if(!file.copy(candidate_paths[[1L]],destination,overwrite=FALSE))
            stop("Unable to stage source: ",destination)
          if(!identical(sha(destination),hashes[[1L]]))
            stop("Staged checksum mismatch: ",destination)
        }
      }
    }
    selection<-q$record_id[vapply(q$record_id,function(id) {
      any(file.exists(c(file.path(stage,"structured",paste0(id,".xml")),
                        file.path(stage,"structured",paste0(id,".json")),
                        file.path(stage,"pdfs",paste0(id,".pdf")))))
    },logical(1))]
    staged[[cohort]]<-stage
    selections[[cohort]]<-sort(as.character(selection))
    manifests[[cohort]]<-data.frame(
      cohort=cohort,queue_records=nrow(q),
      downloaded_fulltexts=length(selection),stringsAsFactors=FALSE)
  }
  report<-do.call(rbind,manifests)
  utils::write.csv(report,file.path(control,"selection_report.csv"),row.names=FALSE)
  if(length(issues)) {
    conflicts<-do.call(rbind,issues)
    utils::write.csv(conflicts,file.path(control,"source_conflicts.csv"),row.names=FALSE)
    stop("Conflicting downloaded source files; see ",
         file.path(control,"source_conflicts.csv"),
         ". No model requests were started.")
  }
  message("[FT Insights v2] Source selection:")
  print(report,row.names=FALSE)
  outputs<-list()
  for(cohort in names(staged)) {
    ids<-selections[[cohort]]
    if(!length(ids)) {
      outputs[[cohort]]<-NULL
      next
    }
    outputs[[cohort]]<-extract_ostara_fulltext_insights_v2(
      w09=staged[[cohort]],prompt_file=prompt_file,
      ontology_file=ontology_file,record_ids=ids,
      max_records=max_records,model="gpt-5.6-luna")
  }
  invisible(outputs)
}
