#' Run fixed-prompt insights extraction across all available OSTARA full texts
#'
#' Uses the provenance-checked source pool from the all-available mechanism
#' inventory for source selection ONLY. No prior mechanism classifications,
#' case decisions or previous insights enter the model input. The exact chosen
#' prompt and ontology are archived and their hashes define the new checkpoint.
#' @param map_path OSTARA project root.
#' @param prompt_file Exact insights prompt selected by the user.
#' @param ontology_file Exact v5 ontology selected by the user.
#' @param mechanism_dir All-available fulltext mechanism source pool.
#' @param max_records Maximum new LLM assessments, zero for all.
#' @return Versioned unified insights output directory, invisibly.
#' @export
run_ostara_all_fulltext_insights <- function(
    map_path=NULL,prompt_file,ontology_file,mechanism_dir=NULL,
    max_records=0L) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  norm<-function(p)normalizePath(p,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  map_path<-norm(map_path)
  prompt_file<-norm(prompt_file)
  ontology_file<-norm(ontology_file)
  if(length(max_records)!=1L||is.na(max_records)||
     max_records!=as.integer(max_records)||max_records<0L)
    stop("max_records must be a nonnegative integer")
  if(is.null(mechanism_dir))
    mechanism_dir<-file.path(map_path,"runs","W09",
      "fulltext_mechanism_all_available","all-906-c7bc72ce01cb8025")
  mechanism_dir<-norm(mechanism_dir)
  source_csv<-file.path(mechanism_dir,"source_pool.csv")
  source_manifest<-file.path(mechanism_dir,"pool_manifest.json")
  pool<-utils::read.csv(source_csv,stringsAsFactors=FALSE,
                        check.names=FALSE,fileEncoding="UTF-8-BOM")
  m<-jsonlite::fromJSON(source_manifest,simplifyVector=FALSE)
  if(!identical(m$schema,"ostara-fulltext-all-available-v1")||
     !all(c("record_id","title","doi","original_source","source_sha256","ext")%in%
           names(pool))||anyDuplicated(pool$record_id)||anyNA(pool$record_id)||
     nrow(pool)!=as.integer(m$selected_count)||
     !setequal(pool$record_id,
               unlist(m$selected_record_ids,use.names=FALSE)))
    stop("Invalid full-text source pool")
  pool<-pool[order(pool$record_id),,drop=FALSE]
  if(any(!file.exists(pool$original_source)))
    stop("Missing source full texts")
  hashes<-unname(vapply(pool$original_source,sha,character(1)))
  if(!identical(hashes,as.character(pool$source_sha256)))
    stop("Full-text source bytes differ from archived mechanism source pool")
  ont<-utils::read.csv(ontology_file,stringsAsFactors=FALSE,
                       check.names=FALSE,fileEncoding="UTF-8-BOM")
  if(!identical(names(ont),c("mechanism_id","category","function",
                              "mechanism","definition"))||nrow(ont)!=26L||
     anyNA(ont)||anyDuplicated(ont$mechanism_id))
    stop("Please select the approved 26-code v5 ontology CSV")
  key<-substr(digest::digest(c(sha(prompt_file),sha(ontology_file),
    pool$record_id,pool$source_sha256,"gpt-5.6-luna"),algo="sha256"),1L,16L)
  base<-file.path(map_path,"runs","W09","fulltext_insights_all_available",
                   paste0("all-",nrow(pool),"-",key))
  manifest_file<-file.path(base,"pool_manifest.json")
  manifest<-list(schema="ostara-all-insights-v1",
    source_pool_sha256=sha(source_csv),
    original_mechanism_pool_manifest_sha256=sha(source_manifest),
    prompt_sha256=sha(prompt_file),ontology_sha256=sha(ontology_file),
    source_record_ids=as.list(pool$record_id),
    source_sha256=as.list(as.character(pool$source_sha256)),
    model="gpt-5.6-luna")
  if(dir.exists(base)&&!file.exists(manifest_file))
    stop("An existing insights run directory lacks a manifest")
  dir.create(base,recursive=TRUE,showWarnings=FALSE)
  if(file.exists(manifest_file)) {
    old<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
    if(!identical(manifest,old))stop("Insights source provenance changed")
  } else jsonlite::write_json(manifest,manifest_file,
                              pretty=TRUE,auto_unbox=TRUE)
  stage<-file.path(base,"source_fulltexts")
  dir.create(file.path(stage,"structured"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(stage,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  queue<-pool[,c("record_id","title","doi"),drop=FALSE]
  qfile<-file.path(stage,"retrieval_queue.csv")
  if(file.exists(qfile)) {
    old<-utils::read.csv(qfile,stringsAsFactors=FALSE,check.names=FALSE)
    if(!identical(old,queue))stop("Existing insights queue has changed")
  } else utils::write.csv(queue,qfile,row.names=FALSE)
  for(i in seq_len(nrow(pool))) {
    ext<-as.character(pool$ext[[i]])
    if(!ext%in%c("xml","json","pdf"))
      stop("Invalid source extension for ",pool$record_id[[i]])
    dest<-file.path(stage,if(ext=="pdf")"pdfs" else "structured",
                     paste0(pool$record_id[[i]],".",ext))
    if(!file.exists(dest)&&!file.symlink(pool$original_source[[i]],dest))
      stop("Could not stage source file for ",pool$record_id[[i]])
    if(!identical(sha(dest),pool$source_sha256[[i]]))
      stop("Staged full text was altered: ",pool$record_id[[i]])
  }
  extraction<-extract_ostara_fulltext_insights_v2(
    w09=stage,prompt_file=prompt_file,ontology_file=ontology_file,
    record_ids=as.character(pool$record_id),max_records=max_records,
    model="gpt-5.6-luna",use_exact_prompt=TRUE)
  ledger<-file.path(extraction,"insights_assessments.jsonl")
  lines<-if(file.exists(ledger))readLines(ledger,warn=FALSE,encoding="UTF-8")
         else character()
  lines<-lines[nzchar(trimws(lines))]
  data<-lapply(lines,jsonlite::fromJSON,simplifyVector=FALSE)
  validated<-vapply(data,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(validated)||any(!validated%in%pool$record_id))
    stop("Insights ledger has duplicated or unselected publications")
  by_id<-setNames(data,validated)
  sc<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  rows<-list()
  for(i in seq_len(nrow(pool))) {
    id<-pool$record_id[[i]]
    z<-by_id[[id]]
    values<-if(is.null(z)||!length(z$insights))list(NULL) else z$insights
    for(x in values)rows[[length(rows)+1L]]<-data.frame(
      record_id=id,
      publication_status=if(is.null(z))"not_yet_validated" else sc(z$status),
      assessment_reason=if(is.null(z))"" else sc(z$assessment_reason),
      insight_id=if(is.null(x))"" else sc(x$insight_id),
      category=if(is.null(x))"" else sc(x$category),
      insight_summary=if(is.null(x))"" else sc(x$insight_summary),
      insight_subtype=if(is.null(x))"" else sc(x$insight_subtype),
      evidence_basis=if(is.null(x))"" else sc(x$evidence_basis),
      support_scope=if(is.null(x))"" else sc(x$support_scope),
      context=if(is.null(x))"" else sc(x$context),
      mechanism_ids=if(is.null(x))"" else sc(x$mechanism_ids),
      source_quote=if(is.null(x))"" else sc(x$source_quote),
      source_anchor=if(is.null(x))"" else sc(x$source_anchor),
      caveats=if(is.null(x))"" else sc(x$caveats),
      evidence_review_required=if(is.null(z))NA else
        isTRUE(z$evidence_review_required),
      evidence_review_flags_json=if(is.null(z))"" else
        as.character(jsonlite::toJSON(z$evidence_review_flags,
                                      auto_unbox=TRUE,null="null")),
      original_source_file=pool$original_source[[i]],
      source_sha256=pool$source_sha256[[i]],
      prompt_sha256=sha(prompt_file),ontology_sha256=sha(ontology_file),
      model="gpt-5.6-luna",stringsAsFactors=FALSE)
  }
  table<-do.call(rbind,rows)
  target<-file.path(base,"ostara_fulltext_insights.csv")
  tmp<-tempfile(tmpdir=base,fileext=".csv")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  utils::write.csv(table,tmp,row.names=FALSE,na="")
  if(file.exists(target))unlink(target)
  if(!file.rename(tmp,target))stop("Cannot save unified insights CSV")
  jsonlite::write_json(list(schema="ostara-all-insights-csv-v1",
    source_ledger=ledger,source_ledger_sha256=if(file.exists(ledger))sha(ledger) else NULL,
    source_records=nrow(pool),validated_records=length(validated),
    insight_count=sum(nzchar(table$insight_id)),csv_sha256=sha(target)),
    file.path(base,"csv_manifest.json"),pretty=TRUE,auto_unbox=TRUE)
  message("[FT insights] ",length(validated),"/",nrow(pool),
          " publications validated; ",sum(nzchar(table$insight_id)),
          " insights. SINGLE CSV: ",target)
  invisible(base)
}
