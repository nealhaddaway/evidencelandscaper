#' Run a fixed ten-publication insights test with one combined CSV.
#'
#' Uses the exact selected records from the latest OSTARA mechanism pilot,
#' regardless of their original W09 retrieval partition. Copies only the
#' required local full text files into one versioned W09-compatible staging
#' directory. The model receives only the fixed prompt, supplied ontology and
#' each publication's full text; prior mechanism outputs are not supplied.
#' Re-extraction and checkpointing are keyed by prompt, ontology and source IDs.
#' The prompt is never modified.
#'
#' @param map_path Existing Ostara project path.
#' @param prompt_file Fixed user-supplied insights prompt TXT.
#' @param ontology_file Exact approved ontology CSV; NULL locates the one
#'   matching the selected mechanism pilot.
#' @param source_pilot_dir Exact prior mechanism pilot sample directory;
#'   NULL discovers latest completed sample.
#' @param model Luna model.
#' @return One versioned directory with ostara_fulltext_insights.csv.
#' @export
run_ostara_fulltext_insights_pilot <- function(
    map_path=NULL,prompt_file,ontology_file=NULL,source_pilot_dir=NULL,
    model="gpt-5.6-luna") {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  root<-file.path(map_path,"runs","W09","random_fulltext_mechanism_pilots")
  if(is.null(source_pilot_dir)) {
    dirs<-if(dir.exists(root))list.dirs(root,recursive=FALSE,full.names=TRUE)
          else character()
    dirs<-dirs[file.exists(file.path(dirs,"sample_manifest.json"))&
               file.exists(file.path(dirs,"random_sample.csv"))]
    if(!length(dirs))stop("No existing mechanism pilot sample found")
    dirs<-dirs[order(file.info(file.path(dirs,"sample_manifest.json"))$mtime,
                     decreasing=TRUE)]
    source_pilot_dir<-dirs[[1L]]
  }
  source_pilot_dir<-normalizePath(source_pilot_dir,winslash="/",mustWork=TRUE)
  sm<-jsonlite::fromJSON(file.path(source_pilot_dir,"sample_manifest.json"),
                         simplifyVector=FALSE)
  if(!identical(sm$schema,"ostara-random-multicohort-fulltexts-v1"))
    stop("Unsupported mechanism pilot sample manifest")
  selection<-utils::read.csv(file.path(source_pilot_dir,"random_sample.csv"),
                            stringsAsFactors=FALSE,colClasses="character",
                            check.names=FALSE)
  if(!identical(names(selection),c("record_id","w09","source_file"))||
     nrow(selection)!=10L||anyNA(selection)||
     anyDuplicated(selection$record_id)||length(sm$records)!=nrow(selection))
    stop("Expected 10 unique prior pilot source records")
  for(i in seq_len(nrow(selection))) {
    x<-sm$records[[i]]
    if(!identical(as.character(x$record_id),selection$record_id[[i]])||
       !identical(as.character(x$source_file),selection$source_file[[i]])||
       !identical(as.character(x$w09),selection$w09[[i]]))
      stop("Sample register and provenance mismatch")
  }
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  # Discover the ontology archive of this exact sample; if multiple
  # versions are present require an explicit choice, never guess.
  if(is.null(ontology_file)) {
    archives<-list.files(source_pilot_dir,pattern="^ontology\\.csv$",
                         recursive=TRUE,full.names=TRUE)
    archives<-archives[grepl("/v4-[^/]+/ontology\\.csv$",archives)]
    hashes<-if(length(archives))vapply(archives,sha,character(1)) else character()
    if(!length(archives)||length(unique(hashes))!=1L)
      stop("Cannot identify a unique archived mechanism ontology; supply ontology_file")
    ontology_file<-archives[[1L]]
  }
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  files<-selection$source_file
  if(any(!file.exists(files)))stop("One or more original full texts are missing")
  source_hashes<-vapply(files,sha,character(1))
  # The original records are fixed, but a new prompt or ontology means a new
  # version of the results. No old insight checkpoints are imported.
  key<-substr(digest::digest(c(sha(prompt_file),sha(ontology_file),
      selection$record_id,source_hashes,model),algo="sha256"),1L,16L)
  base<-file.path(map_path,"runs","W09","fulltext_insights_pilots",
                   paste0("n10-",key))
  manifest_path<-file.path(base,"pilot_manifest.json")
  manifest<-list(schema="ostara-insights-fixed10-v1",
    source_sample=source_pilot_dir,
    sample_manifest_sha256=sha(file.path(source_pilot_dir,"sample_manifest.json")),
    prompt_sha256=sha(prompt_file),ontology_sha256=sha(ontology_file),
    model=model,record_ids=as.list(selection$record_id),
    source_sha256=as.list(unname(source_hashes)))
  if(dir.exists(base)&&!file.exists(manifest_path))
    stop("Existing insights pilot directory without manifest: ",base)
  dir.create(base,recursive=TRUE,showWarnings=FALSE)
  if(file.exists(manifest_path)) {
    old<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(old,manifest))stop("Insights pilot provenance mismatch")
  } else jsonlite::write_json(manifest,manifest_path,auto_unbox=TRUE,pretty=TRUE)
  # One staged W09 dataset, not first10/remaining90 outputs.
  stage<-file.path(base,"source_fulltexts")
  dir.create(file.path(stage,"pdfs"),recursive=TRUE,showWarnings=FALSE)
  dir.create(file.path(stage,"structured"),recursive=TRUE,showWarnings=FALSE)
  queue<-data.frame(record_id=selection$record_id,
                    source_file=selection$source_file,stringsAsFactors=FALSE)
  queue_path<-file.path(stage,"retrieval_queue.csv")
  if(file.exists(queue_path)) {
    prior<-utils::read.csv(queue_path,stringsAsFactors=FALSE)
    if(!identical(prior,queue))stop("Staged queue changed")
  } else utils::write.csv(queue,queue_path,row.names=FALSE)
  for(i in seq_len(nrow(selection))) {
    source<-files[[i]]
    ext<-tolower(tools::file_ext(source))
    if(!ext%in%c("xml","json","pdf"))
      stop("Unsupported full-text format: ",source)
    folder<-if(ext=="pdf")"pdfs" else "structured"
    target<-file.path(stage,folder,paste0(selection$record_id[[i]],".",ext))
    if(file.exists(target)) {
      if(!identical(sha(target),source_hashes[[i]]))
        stop("Staged full text content changed: ",target)
    } else {
      if(!file.copy(source,target,overwrite=FALSE)||
         !identical(sha(target),source_hashes[[i]]))
        stop("Could not stage verified source: ",source)
    }
  }
  output<-extract_ostara_fulltext_insights_v2(
    w09=stage,prompt_file=prompt_file,ontology_file=ontology_file,
    record_ids=as.character(selection$record_id),max_records=0L,model=model)
  # Consolidate completed outputs, preserving no-insight and unclear
  # publications as explicit publication-level rows.
  ledger<-file.path(output,"insights_assessments.jsonl")
  raw<-if(file.exists(ledger))readLines(ledger,warn=FALSE,encoding="UTF-8")
       else character()
  raw<-raw[nzchar(trimws(raw))]
  data<-lapply(raw,jsonlite::fromJSON,simplifyVector=FALSE)
  observed<-vapply(data,function(z)as.character(z$record_id),character(1))
  if(anyDuplicated(observed)||any(!observed%in%selection$record_id))
    stop("Invalid or duplicated records in insights ledger")
  by_id<-setNames(data,observed)
  scalar<-function(x)if(is.null(x)||!length(x))"" else
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  rows<-list()
  for(i in seq_len(nrow(selection))) {
    id<-selection$record_id[[i]]
    z<-by_id[[id]]
    vals<-if(is.null(z)||!length(z$insights))list(NULL) else z$insights
    for(x in vals) {
      rows[[length(rows)+1L]]<-data.frame(
        record_id=id,
        publication_status=if(is.null(z))"not_yet_validated" else scalar(z$status),
        assessment_reason=if(is.null(z))"" else scalar(z$assessment_reason),
        insight_id=if(is.null(x))"" else scalar(x$insight_id),
        category=if(is.null(x))"" else scalar(x$category),
        insight_summary=if(is.null(x))"" else scalar(x$insight_summary),
        insight_subtype=if(is.null(x))"" else scalar(x$insight_subtype),
        evidence_basis=if(is.null(x))"" else scalar(x$evidence_basis),
        support_scope=if(is.null(x))"" else scalar(x$support_scope),
        context=if(is.null(x))"" else scalar(x$context),
        mechanism_ids=if(is.null(x))"" else scalar(x$mechanism_ids),
        source_quote=if(is.null(x))"" else scalar(x$source_quote),
        source_anchor=if(is.null(x))"" else scalar(x$source_anchor),
        caveats=if(is.null(x))"" else scalar(x$caveats),
        evidence_review_required=if(is.null(z))NA else isTRUE(z$evidence_review_required),
        evidence_review_flags_json=if(is.null(z))"" else as.character(
          jsonlite::toJSON(z$evidence_review_flags,auto_unbox=TRUE,null="null")),
        original_source_file=selection$source_file[[i]],
        source_sha256=source_hashes[[i]],
        prompt_sha256=sha(prompt_file),ontology_sha256=sha(ontology_file),
        model=model,stringsAsFactors=FALSE)
    }
  }
  tab<-do.call(rbind,rows)
  csv<-file.path(base,"ostara_fulltext_insights.csv")
  tmp<-tempfile(pattern=".ostara-",tmpdir=base,fileext=".csv")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  utils::write.csv(tab,tmp,row.names=FALSE,na="")
  if(file.exists(csv))unlink(csv)
  if(!file.rename(tmp,csv))stop("Cannot save unified insights CSV")
  jsonlite::write_json(list(
    schema="ostara-insights-fixed10-csv-v1",
    validated_records=length(observed),selected_records=nrow(selection),
    insight_rows=sum(nzchar(tab$insight_id)),csv_sha256=sha(csv),
    source_ledger=ledger,source_ledger_sha256=if(file.exists(ledger))sha(ledger) else NULL),
    file.path(base,"csv_manifest.json"),pretty=TRUE,auto_unbox=TRUE)
  message("[FT insights pilot] ",length(observed),"/",nrow(selection),
          " publications validated; ",sum(nzchar(tab$insight_id)),
          " insights. SINGLE CSV: ",csv)
  invisible(base)
}
