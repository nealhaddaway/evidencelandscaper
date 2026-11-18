#' Run a fresh mechanism inventory on every available Ostara journal full text
#'
#' Both original all-included W09 retrieval collections form one publication
#' pool. Files from other W09 queues may be reused only after matching exact
#' record ID, normalised title and DOI. Original PDFs/XML/JSON are not changed.
#' A single W09-compatible staging queue of symlinks supports existing validated
#' and checkpointed extraction. Every file is assessed under the exact chosen
#' prompt and ontology, even when an earlier pilot already assessed that ID.
#'
#' @param map_path Existing Ostara project path; default configured map.
#' @param prompt_file Explicit mechanism-inventory prompt TXT.
#' @param ontology_file Explicit approved ontology CSV.
#' @param model Luna model.
#' @param max_records New calls in this invocation, 0 for all remaining.
#' @return Directory with ostara_fulltext_mechanisms.csv; invisibly.
#' @export
run_ostara_all_available_fulltext_mechanisms <- function(
    map_path=NULL,prompt_file,ontology_file,model="gpt-5.6-luna",
    max_records=0L) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  if(!is.numeric(max_records)||length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer")
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  root<-file.path(map_path,"runs","W09")
  runs<-list.dirs(root,recursive=FALSE,full.names=TRUE)
  runs<-runs[file.exists(file.path(runs,"retrieval_queue.csv"))]
  canonical<-runs[grepl("^(first10|remaining90)-all-included-",basename(runs))]
  groups<-split(canonical,ifelse(grepl("^first10-",basename(canonical)),
                                 "first10","remaining90"))
  if(!all(c("first10","remaining90")%in%names(groups))||
     any(lengths(groups)!=1L))
    stop("Cannot identify exactly one authoritative all-included queue for each historical retrieval collection")
  # Retrieval cohorts determine source discovery only, not separate analyses.
  qread<-function(dir) {
    q<-utils::read.csv(file.path(dir,"retrieval_queue.csv"),
                       stringsAsFactors=FALSE)
    if(!all(c("record_id","title","doi")%in%names(q))||
       anyNA(q$record_id)||anyDuplicated(q$record_id))
      stop("Invalid retrieval identities in ",dir)
    q
  }
  standard_title<-function(x) {
    x[is.na(x)]<-""
    tolower(gsub("\\s+"," ",trimws(x)))
  }
  standard_doi<-function(x) {
    x[is.na(x)]<-""
    x<-tolower(trimws(x))
    x<-sub("^https?://(dx\\.)?doi\\.org/","",x)
    sub("^doi:\\s*","",x)
  }
  own<-lapply(canonical,qread)
  universe<-do.call(rbind,lapply(seq_along(canonical),function(j){
    q<-own[[j]]
    data.frame(record_id=as.character(q$record_id),
      title=as.character(q$title),doi=as.character(q$doi),
      source_queue=canonical[[j]],stringsAsFactors=FALSE)
  }))
  if(anyDuplicated(universe$record_id)) {
    duplicates<-unique(universe$record_id[duplicated(universe$record_id)])
    for(id in duplicates) {
      a<-universe[universe$record_id==id,,drop=FALSE]
      if(length(unique(standard_title(a$title)))>1L||
         length(unique(standard_doi(a$doi)))>1L)
        stop("Conflicting bibliographic identities for ",id)
    }
    universe<-universe[!duplicated(universe$record_id),,drop=FALSE]
  }
  universe<-universe[order(universe$record_id),,drop=FALSE]
  # Some legacy W09 queues do not contain usable bibliographic identity
  # columns; they cannot safely donate full texts to this run.
  usable<-vapply(runs,function(p) {
    q<-tryCatch(utils::read.csv(file.path(p,"retrieval_queue.csv"),
                                stringsAsFactors=FALSE),
                error=function(e)NULL)
    !is.null(q)&&all(c("record_id","title","doi")%in%names(q))&&
      !anyNA(q$record_id)&&!anyDuplicated(q$record_id)
  },logical(1))
  runs<-runs[usable]
  all_queues<-lapply(runs,qread)
  formats<-data.frame(folder=c("structured","structured","pdfs"),
                      ext=c("xml","json","pdf"),stringsAsFactors=FALSE)
  selected<-vector("list",nrow(universe))
  missing<-character()
  for(i in seq_len(nrow(universe))) {
    id<-universe$record_id[[i]]
    target_title<-standard_title(universe$title[[i]])
    target_doi<-standard_doi(universe$doi[[i]])
    candidates<-list()
    for(j in seq_along(runs)) {
      q<-all_queues[[j]]
      k<-match(id,q$record_id)
      if(is.na(k)||
         !identical(standard_title(q$title[[k]]),target_title)||
         !identical(standard_doi(q$doi[[k]]),target_doi))next
      for(t in seq_len(nrow(formats))) {
        f<-file.path(runs[[j]],formats$folder[[t]],
                     paste0(id,".",formats$ext[[t]]))
        if(file.exists(f))candidates[[length(candidates)+1L]]<-list(
          path=normalizePath(f,winslash="/",mustWork=TRUE),
          ext=formats$ext[[t]],rank=t)
      }
    }
    if(!length(candidates)) {
      missing<-c(missing,id)
      next
    }
    # Prefer XML, then JSON, then PDF. Same-format conflicts must be resolved,
    # never silently choose one based on directory order.
    ranks<-vapply(candidates,function(x)x$rank,integer(1))
    best<-candidates[ranks==min(ranks)]
    hashes<-vapply(best,function(x)sha(x$path),character(1))
    if(length(unique(hashes))!=1L)
      stop("Conflicting full-text sources for ",id,
           "; resolve before extraction")
    selected[[i]]<-data.frame(record_id=id,
      title=universe$title[[i]],doi=universe$doi[[i]],
      original_source=best[[1L]]$path,ext=best[[1L]]$ext,
      source_sha256=hashes[[1L]],stringsAsFactors=FALSE)
  }
  selected<-Filter(Negate(is.null),selected)
  if(!length(selected))stop("No available canonical W09 journal full texts")
  pool<-do.call(rbind,selected)
  rownames(pool)<-NULL
  # Hash of selected full-text content + exact prompt/ontology/model ensures a
  # revised word causes a NEW run, not reuse of old pilot checkpoints.
  key<-substr(digest::digest(c(sha(prompt_file),sha(ontology_file),
    model,pool$record_id,pool$source_sha256),algo="sha256"),1L,16L)
  base<-file.path(root,"fulltext_mechanism_all_available",
                   paste0("all-",nrow(pool),"-",key))
  manifest_path<-file.path(base,"pool_manifest.json")
  manifest<-list(schema="ostara-fulltext-all-available-v1",
    prompt_sha256=sha(prompt_file),
    ontology_sha256=sha(ontology_file),model=model,
    selected_count=nrow(pool),canonical_total=nrow(universe),
    missing_fulltext_count=length(missing),
    selected_record_ids=as.list(pool$record_id),
    source_hashes=as.list(pool$source_sha256))
  if(dir.exists(base)&&!file.exists(manifest_path))
    stop("Existing run lacks manifest: ",base)
  dir.create(base,recursive=TRUE,showWarnings=FALSE)
  if(file.exists(manifest_path)) {
    old<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(old,manifest))
      stop("Changed provenance for existing mechanism run")
  } else jsonlite::write_json(manifest,manifest_path,auto_unbox=TRUE,pretty=TRUE)
  pool_file<-file.path(base,"source_pool.csv")
  if(file.exists(pool_file)) {
    prior<-utils::read.csv(pool_file,stringsAsFactors=FALSE)
    if(!identical(prior,pool))stop("Existing source pool differs")
  } else utils::write.csv(pool,pool_file,row.names=FALSE)
  missing_file<-file.path(base,"fulltexts_missing.csv")
  if(!file.exists(missing_file))
    utils::write.csv(data.frame(record_id=missing),missing_file,row.names=FALSE)
  stage<-file.path(base,"source_fulltexts")
  dir.create(file.path(stage,"structured"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(stage,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  q<-pool[,c("record_id","title","doi"),drop=FALSE]
  queue_path<-file.path(stage,"retrieval_queue.csv")
  if(file.exists(queue_path)) {
    prior<-utils::read.csv(queue_path,stringsAsFactors=FALSE)
    if(!identical(prior,q))stop("Staged W09 queue differs from original")
  } else utils::write.csv(q,queue_path,row.names=FALSE)
  # Symlinks avoid copying large full texts and are checked before every run.
  for(i in seq_len(nrow(pool))) {
    f<-pool$original_source[[i]]
    dest<-file.path(stage,if(pool$ext[[i]]=="pdf")"pdfs" else "structured",
                     paste0(pool$record_id[[i]],".",pool$ext[[i]]))
    if(!file.exists(dest)&&!file.symlink(f,dest))
      stop("Unable to link full text ",f)
    if(!identical(sha(dest),pool$source_sha256[[i]]))
      stop("Staged full text checksum mismatch for ",pool$record_id[[i]])
  }
  message("[FT mechanisms] One pooled inventory: ",nrow(pool),
          " full texts; ",length(missing)," included publications missing a source. ",
          "Prior pilot records remain eligible.")
  extraction<-extract_ostara_all_fulltext_mechanisms(
    w09=stage,record_ids=as.character(pool$record_id),
    max_records=max_records,model=model,output_dir=base,
    ontology_file=ontology_file,prompt_file=prompt_file)
  .ostara_export_pooled_mechanisms_csv(extraction,base,pool)
  invisible(base)
}

# Produce one complete CSV: one row per approved mechanism per assessed
# publication, and one pending row per unprocessed full text.
.ostara_export_pooled_mechanisms_csv<-function(extraction,base,pool) {
  ledger<-file.path(extraction,"mechanism_assessments.jsonl")
  lines<-if(file.exists(ledger))readLines(ledger,warn=FALSE,
                                              encoding="UTF-8") else character()
  lines<-lines[nzchar(trimws(lines))]
  docs<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  ids<-vapply(docs,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(ids)||any(!ids%in%pool$record_id))
    stop("Duplicate or unexpected records in mechanism checkpoint")
  by_id<-setNames(docs,ids)
  sc<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  js<-function(x)as.character(jsonlite::toJSON(x,auto_unbox=TRUE,
                                                null="null",digits=NA))
  rows<-list()
  for(i in seq_len(nrow(pool))) {
    id<-pool$record_id[[i]]
    z<-by_id[[id]]
    if(!is.null(z)&&(!is.list(z$mechanism_inventory)||
                    !is.list(z$case_presence_assessment)||
                    !is.list(z$new_novelty)||
                    !identical(sc(z$provenance$source_sha256),
                               pool$source_sha256[[i]])))
      stop("Incomplete result or changed source for ",id)
    mechanisms<-if(is.null(z))list(NULL) else z$mechanism_inventory
    if(!length(mechanisms))stop("Validated publication has no mechanisms: ",id)
    for(v in mechanisms)
      rows[[length(rows)+1L]]<-data.frame(
        record_id=id,
        publication_status=if(is.null(z))"not_yet_validated" else "validated",
        mechanism_id=if(is.null(v))"" else sc(v$mechanism_id),
        mechanism_status=if(is.null(v))"" else sc(v$status),
        mechanism_source_quote=if(is.null(v))"" else sc(v$source_quote),
        mechanism_source_anchor=if(is.null(v))"" else sc(v$source_anchor),
        mechanism_reason=if(is.null(v))"" else sc(v$reason),
        case_status=if(is.null(z))"" else sc(z$case_presence_assessment$case_status),
        candidate_case_count=if(is.null(z)||is.null(z$case_presence_assessment$candidate_case_count))
                               NA_integer_ else as.integer(z$case_presence_assessment$candidate_case_count),
        case_assessment_reason=if(is.null(z))"" else sc(z$case_presence_assessment$assessment_reason),
        case_presence_json=if(is.null(z))"" else js(z$case_presence_assessment),
        new_novelty_json=if(is.null(z))"" else js(z$new_novelty),
        candidate_cases_json=if(is.null(z))"" else js(z$case_presence_assessment$candidate_cases),
        evidence_review_required=if(is.null(z))NA else isTRUE(z$evidence_review_required),
        evidence_review_flags_json=if(is.null(z))"" else js(z$evidence_review_flags),
        fulltext_path=pool$original_source[[i]],
        source_sha256=pool$source_sha256[[i]],
        prompt_sha256=if(is.null(z))"" else sc(z$provenance$prompt_sha256),
        ontology_sha256=if(is.null(z))"" else sc(z$provenance$approved_ontology_sha256),
        model=if(is.null(z))"" else sc(z$provenance$model),
        stringsAsFactors=FALSE)
  }
  tab<-do.call(rbind,rows)
  csv<-file.path(base,"ostara_fulltext_mechanisms.csv")
  tmp<-tempfile(pattern=".ostara-",tmpdir=base,fileext=".csv")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  utils::write.csv(tab,tmp,row.names=FALSE,na="")
  if(file.exists(csv))unlink(csv)
  if(!file.rename(tmp,csv))stop("Could not write single pooled mechanism CSV")
  novelty_csv<-export_ostara_fulltext_candidate_mechanisms(base)
  message("[FT mechanisms] Single mechanism database: ",csv," (",
          length(ids),"/",nrow(pool)," validated; ",nrow(tab)," rows)")
  message("[FT mechanisms] Separate candidate mechanisms: ",novelty_csv)
  invisible(csv)
}


#' Export proposed novel mechanisms from the completed full-text inventory
#'
#' Offline, no API calls. Reads the original validated, versioned JSONL ledger
#' rather than parsing repeated JSON cells in the mechanism-wide CSV.
#' Produces one row per proposed mechanism and preserves supporting evidence.
#' Previously saved extraction checkpoints are never changed.
#'
#' @param mechanism_dir Full-text mechanism run folder containing source_pool.csv.
#' @return Path to the flat new_novelty.csv.
#' @export
export_ostara_fulltext_candidate_mechanisms <- function(mechanism_dir) {
  base<-normalizePath(mechanism_dir,winslash="/",mustWork=TRUE)
  sha<-function(path)digest::digest(file=path,algo="sha256",serialize=FALSE)
  pool_path<-file.path(base,"source_pool.csv")
  manifest_path<-file.path(base,"pool_manifest.json")
  if(!all(file.exists(c(pool_path,manifest_path))))
    stop("Missing source pool or manifest in: ",base)
  pool<-utils::read.csv(pool_path,stringsAsFactors=FALSE,
                        check.names=FALSE,fileEncoding="UTF-8-BOM")
  meta<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
  if(!identical(meta$schema,"ostara-fulltext-all-available-v1")||
     !all(c("record_id","source_sha256","original_source")%in%names(pool))||
     anyDuplicated(pool$record_id)||anyNA(pool$record_id)||
     nrow(pool)!=as.integer(meta$selected_count)||
     !setequal(pool$record_id,
               as.character(unlist(meta$selected_record_ids,use.names=FALSE))))
    stop("Full-text mechanism source pool is not valid")
  dirs<-list.dirs(base,recursive=FALSE,full.names=TRUE)
  dirs<-dirs[grepl("^v4-",basename(dirs))&
               file.exists(file.path(dirs,"manifest.json"))&
               file.exists(file.path(dirs,"mechanism_assessments.jsonl"))]
  if(length(dirs)!=1L)
    stop("Expected exactly one completed versioned mechanism ledger in ",base,
         "; found ",length(dirs))
  run<-dirs[[1L]]
  run_meta<-jsonlite::fromJSON(file.path(run,"manifest.json"),
                                simplifyVector=FALSE)
  if(!identical(run_meta$prompt_sha256,meta$prompt_sha256)||
     !identical(run_meta$approved_ontology_sha256,meta$ontology_sha256))
    stop("Extraction checkpoint prompt/ontology differs from source pool")
  ledger_path<-file.path(run,"mechanism_assessments.jsonl")
  raw<-readLines(ledger_path,warn=FALSE,encoding="UTF-8")
  raw<-raw[nzchar(trimws(raw))]
  assessments<-lapply(raw,jsonlite::fromJSON,simplifyVector=FALSE)
  ids<-vapply(assessments,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(ids)||any(!ids%in%pool$record_id))
    stop("Duplicate or unexpected record IDs in mechanism ledger")
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  columns<-c("record_id","candidate_mechanism_number","proposed_label",
             "working_definition","support_status",
             "closest_approved_mechanism_id","reason","source_quote",
             "source_anchor","evidence_review_required",
             "source_file","source_sha256","prompt_sha256",
             "ontology_sha256","model")
  rows<-list()
  for(z in assessments) {
    id<-as.character(z$record_id)
    i<-match(id,pool$record_id)
    if(!identical(scalar(z$provenance$source_sha256),
                  as.character(pool$source_sha256[[i]]))||
       !identical(scalar(z$provenance$prompt_sha256),
                  as.character(meta$prompt_sha256))||
       !identical(scalar(z$provenance$approved_ontology_sha256),
                  as.character(meta$ontology_sha256))||
       !is.list(z$new_novelty))
      stop("Candidate mechanism provenance/structure mismatch for ",id)
    if(!length(z$new_novelty))next
    for(k in seq_along(z$new_novelty)) {
      x<-z$new_novelty[[k]]
      if(!is.list(x))stop("Malformed novelty entry for ",id)
      row<-data.frame(
        record_id=id,candidate_mechanism_number=k,
        proposed_label=scalar(x$proposed_label),
        working_definition=scalar(x$working_definition),
        support_status=scalar(x$support_status),
        closest_approved_mechanism_id=scalar(x$closest_approved_mechanism_id),
        reason=scalar(x$reason),
        source_quote=scalar(x$source_quote),
        source_anchor=scalar(x$source_anchor),
        evidence_review_required=isTRUE(z$evidence_review_required),
        source_file=as.character(pool$original_source[[i]]),
        source_sha256=as.character(pool$source_sha256[[i]]),
        prompt_sha256=scalar(z$provenance$prompt_sha256),
        ontology_sha256=scalar(z$provenance$approved_ontology_sha256),
        model=scalar(z$provenance$model),
        stringsAsFactors=FALSE,check.names=FALSE)
      rows[[length(rows)+1L]]<-row
    }
  }
  out<-if(length(rows))do.call(rbind,rows) else {
    blank<-as.data.frame(matrix(character(),nrow=0L,
                    ncol=length(columns)),stringsAsFactors=FALSE)
    names(blank)<-columns
    blank
  }
  path<-file.path(base,"new_novelty.csv")
  tmp<-tempfile(pattern=".novelty-",tmpdir=base,fileext=".csv")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  utils::write.csv(out,tmp,row.names=FALSE,na="",fileEncoding="UTF-8")
  if(!file.copy(tmp,path,overwrite=TRUE))
    stop("Could not save candidate mechanism CSV: ",path)
  message("[FT mechanisms] Exported ",nrow(out),
          " candidate mechanism proposals from ",length(ids),
          " validated publications: ",path)
  invisible(path)
}
