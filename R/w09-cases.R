#' Prepare an independent W09 full-text case pilot
#'
#' Creates a six-publication retrieval queue, without downloading anything.
#' Existing W04, W07 and W08 files are read-only.
#' @param map_path Ostara map, or another compatible map.
#' @param record_ids Selected case-candidate publication IDs.\n#' @param w04_run Optional explicit finalised W04 run, including selected-record runs.
#' @return W09 run folder.
#' @export
prepare_w09_cases <- function(map_path=NULL,record_ids=c(
 "work-e35e229f44c00c48","work-87a2aaa8f5ab66fd",
 "work-a98a0ec08d809374","work-dfee6fc216d262be",
 "work-40763e339da408a7","work-5996de5c2a387c4c"),w04_run=NULL) {
 if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
 map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
 if(!length(record_ids)||anyDuplicated(record_ids)||anyNA(record_ids))
   stop("Provide distinct case-candidate record IDs.")
 hash<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 readjl<-function(p) {
   x<-readLines(p,warn=FALSE,encoding="UTF-8")
   lapply(x[nzchar(trimws(x))],jsonlite::fromJSON,simplifyVector=FALSE)
 }
 roots<-list.dirs(file.path(map_path,"runs","W01"),recursive=FALSE,full.names=TRUE)
 runs<-unlist(lapply(roots,function(r) {
   p<-file.path(r,"W04_local")
   if(dir.exists(p))list.dirs(p,recursive=FALSE,full.names=TRUE) else character()
 }),use.names=FALSE)
 if(!is.null(w04_run)) {\n   w04_run<-normalizePath(w04_run,winslash="/",mustWork=TRUE)\n   runs<-runs[runs==w04_run]\n } else runs<-runs[grepl("^full-",basename(runs))]\n runs<-runs[file.exists(file.path(runs,"finalised","summary.json"))]
 if(length(runs)!=1L)stop("Expected one finalised W04 run; supply w04_run explicitly.")
 w04<-runs[[1L]]
 final<-file.path(w04,"finalised","workflow04_final_screening_layer.jsonl")
 meta<-jsonlite::fromJSON(file.path(w04,"finalised","summary.json"))
 if(!identical(meta$status,"final")||!identical(meta$final_layer_sha256,hash(final)))
   stop("W04 checksum/status check failed.")
 retained<-vapply(Filter(function(z)identical(z$screening$decision,"retain"),
                          readjl(final)),function(z)z$record_id,character(1))
 if(any(!record_ids %in% retained))stop("W09 record IDs not retained in W04.")
 canonical<-file.path(dirname(dirname(w04)),"W02_main","finalised","canonical.jsonl")
 rows<-readjl(canonical)
 byid<-setNames(rows,vapply(rows,function(z)z$identity$record_id,character(1)))
 if(any(!record_ids%in%names(byid)))stop("Selected W09 records missing from W02.")
 scalar<-function(v)if(is.null(v)||!length(v))"" else
   paste(as.character(unlist(v,use.names=FALSE)),collapse="; ")
 df<-do.call(rbind,lapply(record_ids,function(id) {
   z<-byid[[id]]$canonical
   data.frame(record_id=id,title=scalar(z$title),doi=scalar(z$doi),
              pdf_path="",retrieval_status="not_attempted",notes="",
              stringsAsFactors=FALSE)
 }))
 prompt<-system.file("w09","fulltext_case_prompt_v1.txt",package="evidencelandscaper")
 if(!nzchar(prompt))stop("W09 prompt not packaged.")
 dest<-file.path(map_path,"runs","W09",paste0("pilot-",length(record_ids),"-",
              substr(digest::digest(record_ids,algo="sha256"),1,12)))
 manifest<-file.path(dest,"manifest.json")
 provenance<-list(schema="local-w09-case-pilot-v1",source_w04_sha256=hash(final),
   source_w02_sha256=hash(canonical),prompt_sha256=hash(prompt),
   selected_record_ids=as.list(record_ids))
 if(file.exists(manifest)) {
   old<-jsonlite::fromJSON(manifest,simplifyVector=FALSE)
   for(k in names(provenance))
     if(!identical(old[[k]],provenance[[k]]))stop("W09 manifest conflict: ",k)
   message("[W09] Reusing existing pilot without overwriting: ",dest)
   return(invisible(dest))
 }
 if(dir.exists(dest)&&length(list.files(dest,all.files=TRUE,no..=TRUE)))
   stop("Existing W09 directory cannot be safely overwritten.")
 dir.create(file.path(dest,"pdfs"),recursive=TRUE,showWarnings=FALSE)
 utils::write.csv(df,file.path(dest,"retrieval_queue.csv"),row.names=FALSE,na="")
 file.copy(prompt,file.path(dest,"prompt.txt"))
 jsonlite::write_json(provenance,manifest,auto_unbox=TRUE,pretty=TRUE)
 message("[W09] Created six-case publication retrieval queue: ",dest)
 invisible(dest)
}

#' Verify locally supplied PDFs for a W09 pilot
#'
#' Users may save full-text PDFs as <record_id>.pdf within W09/pdfs.
#' Records with missing PDFs remain in the retrieval queue. No external
#' requests, LLM calls, or changes to source records are made.
#' @param w09 Directory created by prepare_w09_cases().
#' @return CSV file path.
#' @export
inventory_w09_pdfs <- function(w09) {
 w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
 queue<-file.path(w09,"retrieval_queue.csv")
 if(!file.exists(queue))stop("W09 retrieval queue missing.")
 src<-utils::read.csv(queue,stringsAsFactors=FALSE)
 out<-file.path(w09,"pdf_inventory.csv")
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 v<-lapply(src$record_id,function(id) {
   f<-file.path(w09,"pdfs",paste0(id,".pdf"))
   exists<-file.exists(f)
   data.frame(record_id=id,pdf_present=exists,
              pdf_file=if(exists)f else "",
              pdf_sha256=if(exists)sha(f) else "",
              size_bytes=if(exists)file.info(f)$size else NA_real_)
 })
 df<-do.call(rbind,v)
 if(file.exists(out)) {
   prev<-utils::read.csv(out,stringsAsFactors=FALSE)
   if(!identical(prev,df)) {
     # Preserve the original inventory as an immutable historical snapshot.
     archive<-file.path(w09,paste0("pdf_inventory-",
       substr(digest::digest(file=out,algo="sha256",serialize=FALSE),1,12),
       ".csv"))
     if(!file.exists(archive)&&!file.copy(out,archive,overwrite=FALSE))
       stop("Cannot preserve previous PDF inventory.")
     if(!identical(digest::digest(file=archive,algo="sha256",serialize=FALSE),
                   digest::digest(file=out,algo="sha256",serialize=FALSE)))
       stop("Historical inventory archive checksum mismatch.")
     utils::write.csv(df,out,row.names=FALSE,na="")
   }
 } else utils::write.csv(df,out,row.names=FALSE,na="")
 message("[W09] ",sum(df$pdf_present),"/",nrow(df)," PDFs supplied.")
 invisible(out)
}
