#' Export 100% of human-included OSTARA records with W02 bibliography
#'
#' Sources of truth are the two existing W09 "all-included" manifests, each
#' of which pins the human-authoritative W04 snapshot and canonical W02 data.
#' Validates hashes, cohort membership, unique IDs, and exact W04 retention
#' before exporting. This function makes NO API/model calls and does not
#' change any source files.
#'
#' @param map_path Local OSTARA map directory.
#' @return Output CSV path, invisibly.
#' @export
export_ostara_all_included_bibliography <- function(map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  root<-normalizePath(map_path,winslash="/",mustWork=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    if(!file.exists(p))stop("Required provenance file missing: ",p)
    lines<-readLines(p,encoding="UTF-8",warn=FALSE)
    lapply(lines[nzchar(trimws(lines))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  scalar<-function(x) {
    if(is.null(x)||!length(x))return("")
    if(is.list(x)&&!is.null(names(x))&&any(nzchar(names(x))))
      return(as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null")))
    paste(as.character(unlist(x,use.names=FALSE)),collapse=" | ")
  }
  w09root<-file.path(root,"runs","W09")
  dirs<-list.dirs(w09root,recursive=FALSE,full.names=TRUE)
  by_prefix<-function(prefix) {
    match<-dirs[startsWith(basename(dirs),prefix)&
      file.exists(file.path(dirs,"manifest.json"))&
      file.exists(file.path(dirs,"retrieval_queue.csv"))]
    if(length(match)!=1L)
      stop("Expected exactly one provenance-complete W09 ",prefix,
           " queue; found ",length(match),". Refusing to mix runs.")
    match[[1L]]
  }
  queues<-list(first10=by_prefix("first10-all-included-"),
               remaining90=by_prefix("remaining90-all-included-"))
  output<-vector("list",length(queues))
  report<-vector("list",length(queues))
  for(k in seq_along(queues)) {
    cohort<-names(queues)[[k]]
    dir<-queues[[k]]
    manifest<-jsonlite::fromJSON(file.path(dir,"manifest.json"),
                                  simplifyVector=FALSE)
    if(!identical(manifest$schema,paste0("w09-",
         if(cohort=="first10")"first10" else "remaining90",
         "-all-included-v1")))
      stop("Unexpected W09 inclusion manifest schema: ",dir)
    snapshot<-normalizePath(manifest$source_w04_snapshot,
                            winslash="/",mustWork=TRUE)
    layer<-file.path(snapshot,"workflow04_effective_screening_layer.jsonl")
    if(!identical(sha(layer),manifest$source_w04_layer_sha256))
      stop("W04 effective screening checksum mismatch: ",cohort)
    screened<-readjl(layer)
    if(!all(vapply(screened,function(z)
      is.character(z$record_id)&&length(z$record_id)==1L&&
      !is.na(z$record_id),logical(1))))
      stop("Invalid W04 work identifiers: ",cohort)
    decisions<-vapply(screened,function(z)scalar(z$screening$decision),
                       character(1))
    retained<-vapply(screened[decisions=="retain"],
                       function(z)z$record_id,character(1))
    if(anyDuplicated(retained)||any(!nzchar(retained)))
      stop("W04 retained work ID missing or duplicated: ",cohort)
    expected<-as.character(unlist(manifest$selected_record_ids,
                                 use.names=FALSE))
    if(length(retained)!=as.integer(manifest$selected_count)||
       !setequal(retained,expected)||anyDuplicated(expected))
      stop("W04 retained IDs do not exactly match W09 manifest: ",cohort)
    if(cohort=="remaining90"&&length(screened)!=45284L)
      stop("Remaining90 cohort has unexpected size")
    q<-utils::read.csv(file.path(dir,"retrieval_queue.csv"),
      stringsAsFactors=FALSE,colClasses="character",check.names=FALSE,
      fileEncoding="UTF-8-BOM")
    if(!"record_id"%in%names(q)||anyDuplicated(q$record_id)||
       nrow(q)!=length(retained)||!setequal(q$record_id,retained))
      stop("W09 retrieval queue does not exactly match W04 retained IDs: ",
           cohort)
    w04<-normalizePath(manifest$source_w04,winslash="/",mustWork=TRUE)
    canonical<-file.path(dirname(dirname(w04)),"W02_main",
                         "finalised","canonical.jsonl")
    if(!identical(sha(canonical),manifest$canonical_w02_sha256))
      stop("Canonical W02 bibliography checksum mismatch: ",cohort)
    records<-readjl(canonical)
    ids<-vapply(records,function(z)scalar(z$identity$record_id),character(1))
    if(anyDuplicated(ids)||any(!retained%in%ids))
      stop("Missing or duplicate W02 identities: ",cohort)
    records<-records[match(sort(retained),ids)]
    # Include every available W02 canonical field; do not silently drop
    # metadata when its name is absent in another record.
    fields<-unique(unlist(lapply(records,function(z)
      names(z$canonical)),use.names=FALSE))
    fields<-fields[nzchar(fields)]
    bibliog<-data.frame(record_id=sort(retained),
      screening_cohort=rep(cohort,length(retained)),
      screening_decision=rep("retain",length(retained)),
      stringsAsFactors=FALSE,check.names=FALSE)
    for(field in fields)
      bibliog[[field]]<-vapply(records,function(z)
        scalar(z$canonical[[field]]),character(1))
    # Explicit alias so consumers know record_id is the preserved work ID.
    bibliog$work_id<-bibliog$record_id
    output[[k]]<-bibliog
    report[[k]]<-list(cohort=cohort,selected=length(retained),
      w09_queue=dir,w04_layer=layer,w04_sha256=sha(layer),
      canonical_w02=canonical,w02_sha256=sha(canonical))
  }
  keys<-unique(unlist(lapply(output,names),use.names=FALSE))
  output<-lapply(output,function(d){
    for(n in setdiff(keys,names(d)))d[[n]]<-rep("",nrow(d))
    d[,keys,drop=FALSE]
  })
  all<-do.call(rbind,output)
  rownames(all)<-NULL
  if(anyDuplicated(all$record_id)||any(!nzchar(all$record_id)))
    stop("The two authoritative cohorts overlap or contain missing IDs")
  expected_n<-sum(vapply(report,function(z)z$selected,integer(1)))
  if(nrow(all)!=expected_n)
    stop("Incomplete included bibliography: expected ",expected_n,
         " but found ",nrow(all))
  all<-all[order(all$record_id),,drop=FALSE]
  outdir<-file.path(root,"reports","ostara_included_bibliography")
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  target<-file.path(outdir,"OSTARA_all_included_bibliographic_records.csv")
  tmp<-tempfile(pattern=".included-",tmpdir=outdir,fileext=".csv")
  on.exit(if(file.exists(tmp))unlink(tmp),add=TRUE)
  con<-file(tmp,"wb")
  writeBin(as.raw(c(239L,187L,191L)),con)
  utils::write.table(all,con,sep=",",row.names=FALSE,col.names=TRUE,
                     quote=TRUE,na="",qmethod="double")
  close(con)
  check<-utils::read.csv(tmp,stringsAsFactors=FALSE,check.names=FALSE,
    colClasses="character",fileEncoding="UTF-8-BOM",na.strings=character())
  if(nrow(check)!=expected_n||
     anyDuplicated(check$record_id)||
     !setequal(check$record_id,all$record_id))
    stop("Export failed 100% inclusion verification; old output preserved")
  if(!file.copy(tmp,target,overwrite=TRUE))
    stop("Cannot save the complete bibliography: ",target)
  jsonlite::write_json(list(schema="ostara-included-bibliography-v1",
    exported_records=nrow(all),unique_work_ids=length(unique(all$record_id)),
    cohorts=report,csv_sha256=sha(target),
    validated_exactly_against_W04=TRUE),
    file.path(outdir,"export_manifest.json"),
    auto_unbox=TRUE,pretty=TRUE)
  message("[OSTARA inclusion bibliography] 100% export verified: ",
          nrow(all)," unique included work IDs; ",ncol(all),
          " columns. File: ",target)
  invisible(target)
}
