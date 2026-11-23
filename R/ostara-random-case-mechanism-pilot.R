#' Randomly select ten existing OSTARA case-extraction work IDs
#'
#' Samples from DISTINCT record_id values in the authoritative retrospective
#' case extraction CSV BEFORE searching any full-text files. Resolves each
#' selected ID against earlier W09 retrieval runs. Never replaces a randomly
#' selected ID because its full text cannot be found. Frozen selection and
#' source audit support independent verification and reproducibility.
#'
#' @param map_path OSTARA Google Drive project root (synced locally).
#' @param cases_csv Authoritative retrospective case-extraction CSV.
#' @param ontology_csv Approved v5 26-code ontology CSV.
#' @param output_dir Permanent staging and extraction output directory.
#' @param seed Reproducible random seed.
#' @param run_extraction FALSE stages only; TRUE makes Luna calls if all ten
#'   sources are available and readable.
#' @param model Luna model.
#' @param max_new Maximum new requests, default ten.
#' @return Paths to frozen selection, source audit, and pilot result.
#' @export
run_ostara_random_case_mechanism_pilot <- function(
    map_path,cases_csv,ontology_csv,output_dir,
    seed=20261123L,run_extraction=FALSE,
    model="gpt-5.6-luna",max_new=10L) {
  for(pkg in c("jsonlite","digest"))
    if(!requireNamespace(pkg,quietly=TRUE))
      stop("Install required package: ",pkg)
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  map_path<-norm(map_path)
  cases_csv<-norm(cases_csv)
  ontology_csv<-norm(ontology_csv)
  if(length(seed)!=1L||is.na(seed)||!is.numeric(seed)||
     seed!=as.integer(seed))stop("seed must be a single integer.")
  cases<-utils::read.csv(cases_csv,check.names=FALSE,
    colClasses="character",stringsAsFactors=FALSE,
    fileEncoding="UTF-8-BOM",na.strings=character())
  if(!all(c("record_id","case_id","01. Case name")%in%names(cases))||
     !nrow(cases)||anyDuplicated(cases$case_id)||
     any(!nzchar(trimws(cases$record_id)))||
     any(!nzchar(trimws(cases$case_id))))
    stop("Invalid case-extraction CSV.")
  frame<-sort(unique(as.character(cases$record_id)))
  if(length(frame)<10L)stop("Fewer than ten case-positive works in case sheet.")
  if(!dir.exists(output_dir))
    dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  output_dir<-norm(output_dir)
  frozen<-file.path(output_dir,"random_work_selection.csv")
  frozen_manifest<-file.path(output_dir,"random_selection_provenance.json")
  provenance<-list(schema="ostara-random-case-work-selection-v1",
    cases_sha256=sha(cases_csv),ontology_sha256=sha(ontology_csv),
    random_seed=as.integer(seed),frame_size=length(frame))
  if(file.exists(frozen)||file.exists(frozen_manifest)) {
    if(!file.exists(frozen)||!file.exists(frozen_manifest))
      stop("Incomplete prior sample: selection and provenance required.")
    old<-jsonlite::fromJSON(frozen_manifest,simplifyVector=FALSE)
    if(!identical(old,provenance))
      stop("Case register, ontology or random seed changed. Use a NEW output_dir.")
    selection<-utils::read.csv(frozen,colClasses="character",
      stringsAsFactors=FALSE)
    if(!identical(names(selection),c("selection_order","record_id","case_count"))||
       nrow(selection)!=10L||anyDuplicated(selection$record_id)||
       any(!selection$record_id%in%frame))
      stop("Previously frozen case-positive sample is invalid.")
  } else {
    if(length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
      stop("Output directory contains data without a locked sample.")
    had_rng<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
    if(had_rng)old_rng<-get(".Random.seed",envir=.GlobalEnv)
    set.seed(as.integer(seed))
    ids<-sample(frame,size=10L,replace=FALSE)
    if(had_rng)assign(".Random.seed",old_rng,envir=.GlobalEnv)
    else rm(".Random.seed",envir=.GlobalEnv)
    counts<-table(cases$record_id)
    selection<-data.frame(selection_order=as.character(1:10),
      record_id=ids,case_count=as.character(counts[ids]),
      stringsAsFactors=FALSE)
    utils::write.csv(selection,frozen,row.names=FALSE,na="")
    jsonlite::write_json(provenance,frozen_manifest,
      auto_unbox=TRUE,pretty=TRUE,null="null")
  }
  ids<-selection$record_id
  # Reuse existing W09 retrieval queues and independently archived W09 sources.
  # Candidate filenames must match the selected ID exactly, never fuzzy text.
  w09_root<-file.path(map_path,"runs","W09")
  if(!dir.exists(w09_root))stop("Existing OSTARA W09 runs not found: ",w09_root)
  runs<-list.dirs(w09_root,recursive=TRUE,full.names=TRUE)
  runs<-runs[file.exists(file.path(runs,"retrieval_queue.csv"))]
  runs<-unique(runs)
  catalog<-list()
  for(run in runs) {
    q<-tryCatch(utils::read.csv(file.path(run,"retrieval_queue.csv"),
      stringsAsFactors=FALSE,check.names=FALSE),
      error=function(e)NULL)
    if(is.null(q)||!"record_id"%in%names(q)||
       anyDuplicated(q$record_id))next
    matched<-intersect(ids,as.character(q$record_id))
    for(id in matched) {
      for(ext in c("pdf","xml","json")) {
        folder<-if(ext=="pdf")"pdfs" else "structured"
        p<-file.path(run,folder,paste0(id,".",ext))
        if(file.exists(p))catalog[[length(catalog)+1L]]<-data.frame(
          record_id=id,fulltext_path=norm(p),ext=ext,
          source_run=run,source_sha256=sha(p),
          stringsAsFactors=FALSE)
      }
    }
  }
  # Also inspect the established pooled full-text run manifests.
  # These contain an original_source column with verified source identities.
  pools<-list.files(w09_root,pattern="^source_pool\\.csv$",
    full.names=TRUE,recursive=TRUE)
  for(pool_path in pools) {
    q<-tryCatch(utils::read.csv(pool_path,
      stringsAsFactors=FALSE,check.names=FALSE),error=function(e)NULL)
    if(is.null(q)||!all(c("record_id","original_source")%in%names(q)))next
    for(id in intersect(ids,q$record_id)) {
      z<-q[q$record_id==id,,drop=FALSE]
      for(p in unique(z$original_source)) {
        if(!nzchar(p)||!file.exists(p))next
        ext<-tolower(tools::file_ext(p))
        if(!ext%in%c("pdf","xml","json"))next
        catalog[[length(catalog)+1L]]<-data.frame(
          record_id=id,fulltext_path=norm(p),ext=ext,
          source_run=dirname(dirname(norm(p))),source_sha256=sha(p),
          stringsAsFactors=FALSE)
      }
    }
  }
  sources<-if(length(catalog))unique(do.call(rbind,catalog)) else
    data.frame(record_id=character(),fulltext_path=character(),
      ext=character(),source_run=character(),source_sha256=character())
  results<-list()
  for(id in ids) {
    candidates<-sources[sources$record_id==id,,drop=FALSE]
    if(nrow(candidates)) {
      # Prefer the most directly inspectable PDF; use XML/JSON otherwise.
      candidates$rank<-match(candidates$ext,c("pdf","xml","json"))
      candidates<-candidates[order(candidates$rank,
        candidates$fulltext_path),,drop=FALSE]
      best<-candidates[candidates$rank==min(candidates$rank),,drop=FALSE]
      if(length(unique(best$source_sha256))>1L) {
        result<-data.frame(record_id=id,
          availability="conflicting_same_format_sources",
          fulltext_path="",ext="",source_sha256="",
          candidate_count=nrow(candidates),stringsAsFactors=FALSE)
      } else {
        result<-data.frame(record_id=id,availability="located",
          fulltext_path=best$fulltext_path[[1L]],ext=best$ext[[1L]],
          source_sha256=best$source_sha256[[1L]],
          candidate_count=nrow(candidates),stringsAsFactors=FALSE)
      }
    } else result<-data.frame(record_id=id,availability="not_found_in_prior_W09_runs",
      fulltext_path="",ext="",source_sha256="",
      candidate_count=0L,stringsAsFactors=FALSE)
    results[[length(results)+1L]]<-result
  }
  audit<-do.call(rbind,results)
  audit_path<-file.path(output_dir,"selected_work_fulltext_audit.csv")
  if(file.exists(audit_path)) {
    prior<-utils::read.csv(audit_path,colClasses="character")
    now<-audit
    now$candidate_count<-as.character(now$candidate_count)
    if(!identical(prior,now))
      stop("Full-text sources changed since selection. Review the frozen audit before resuming.")
  } else utils::write.csv(audit,audit_path,row.names=FALSE,na="")
  missing<-audit[audit$availability!="located",,drop=FALSE]
  if(nrow(missing)) {
    message("[OSTARA] Ten random work IDs locked, but ",nrow(missing),
      " full text(s) unresolved or conflicting. No substitutions and no API calls.")
    return(invisible(list(selection=frozen,source_audit=audit_path,
      missing=missing,pilot=NULL,completed=0L)))
  }
  manifest<-audit[,c("record_id","fulltext_path"),drop=FALSE]
  manifest_path<-file.path(output_dir,"fulltext_manifest.csv")
  if(file.exists(manifest_path)) {
    before<-utils::read.csv(manifest_path,colClasses="character",
                            stringsAsFactors=FALSE)
    if(!identical(before,manifest))stop("Frozen source manifest changed.")
  } else utils::write.csv(manifest,manifest_path,row.names=FALSE,na="")
  pilot<-run_ostara_case_mechanism_fulltext_pilot(
    cases_csv=cases_csv,ontology_csv=ontology_csv,
    fulltext_manifest_csv=manifest_path,
    output_dir=file.path(output_dir,"attribution"),
    record_ids=ids,model=model,run_extraction=run_extraction,
    max_new=max_new,seed=seed)
  invisible(list(selection=frozen,source_audit=audit_path,
    missing=missing,pilot=pilot,completed=pilot$completed))
}
