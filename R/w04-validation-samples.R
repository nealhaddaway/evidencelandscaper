#' Draw independent manual screening batches before model screening
#' @param scored W01 full-scoring directory.
#' @param seed Fixed random seed.
#' @return Directory containing five nonoverlapping CSV screening batches.
#' @export
sample_w04_validation <- function(scored,seed=20261126L) {
  scored<-normalizePath(scored,mustWork=TRUE)
  root<-dirname(scored)
  main<-file.path(root,"W02_main")
  locked<-file.path(main,"locked")
  w03<-file.path(root,"W03_publication_status","full")
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readj<-function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)
  readjl<-function(p)lapply(readLines(p,warn=FALSE,encoding="UTF-8"),jsonlite::fromJSON,simplifyVector=FALSE)
  scalar<-function(x){
    z<-as.character(unlist(x,use.names=FALSE))
    z<-z[!is.na(z)&nzchar(trimws(z))]
    if(length(z))z[[1L]] else ""
  }
  pointer<-readj(file.path(main,"final_lock.json"))
  manifest_path<-file.path(locked,"manifest.json")
  stopifnot(identical(pointer$schema,"local-w02-final-lock-v1"),
    identical(pointer$directory,"locked"),identical(pointer$manifest_sha256,sha(manifest_path)))
  lm<-readj(manifest_path)
  stopifnot(identical(lm$status,"final"))
  for(n in names(lm$files))stopifnot(identical(basename(n),n),identical(lm$files[[n]],sha(file.path(locked,n))))
  canonical<-file.path(locked,"canonical.jsonl")
  statuses<-file.path(w03,"publication_status.jsonl")
  sm<-readj(file.path(w03,"local_manifest.json"))
  stopifnot(identical(sm$status,"final"),isTRUE(sm$full_corpus),
    identical(sm$w02_canonical_sha256,sha(canonical)),identical(sm$status_sha256,sha(statuses)),
    identical(sm$report_sha256,sha(file.path(w03,"report.json"))),
    identical(sm$audit_sha256,sha(file.path(w03,"api_audit.jsonl"))))
  rows<-readjl(canonical); states<-readjl(statuses)
  ids<-vapply(rows,function(r)scalar(r$identity$record_id),character(1))
  sid<-vapply(states,function(r)scalar(r$record_id),character(1))
  stopifnot(all(nzchar(ids)),!anyDuplicated(ids),!anyDuplicated(sid),setequal(ids,sid),
    length(ids)==as.integer(sm$publication_clusters))
  states<-states[match(ids,sid)]
  excluded<-vapply(states,function(r){
    v<-r$publication_status$exclude_from_workflow04
    if(!is.logical(v)||length(v)!=1L||is.na(v))stop("Invalid W03 exclusion flag.")
    v
  },logical(1))
  eligible<-which(!excluded)
  stopifnot(length(eligible)>=1000L,length(seed)==1L,is.finite(seed),seed==as.integer(seed))
  seed<-as.integer(seed)
  out<-file.path(root,"W04_validation",paste0("random_batches_seed_",seed))
  spec<-list(schema="w04-blind-random-validation-v1",seed=seed,
    population=length(eligible),excluded=sum(excluded),batches=5L,batch_size=200L,
    sampling="simple random sampling without replacement across all batches",
    w02_sha256=sha(canonical),w03_sha256=sha(statuses))
  if(dir.exists(out)){
    m<-readj(file.path(out,"manifest.json"))
    stopifnot(identical(m$spec,spec))
    # Batch CSVs are editable review files; do not overwrite them.
    stopifnot(identical(m$membership_sha256,sha(file.path(out,"membership.csv"))),
      all(file.exists(file.path(out,paste0("batch ",1:5,".csv")))))
    message("[W04] Existing batches retained (including any manual edits).")
    return(invisible(out))
  }
  old_kind<-RNGkind()
  had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed<-get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    do.call(RNGkind,as.list(old_kind))
    if(had_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)
    else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed,kind="Mersenne-Twister",normal.kind="Inversion",sample.kind="Rejection")
  chosen<-eligible[sample.int(length(eligible),1000L,replace=FALSE)]
  stopifnot(!anyDuplicated(chosen))
  batch<-rep(1:5,each=200L)
  pos<-rep(1:200,5)
  membership<-data.frame(batch=batch,position=pos,record_id=ids[chosen])
  dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
  tmp<-tempfile("sample_",tmpdir=dirname(out));dir.create(tmp)
  on.exit(if(dir.exists(tmp))unlink(tmp,recursive=TRUE),add=TRUE)
  utils::write.csv(membership,file.path(tmp,"membership.csv"),row.names=FALSE)
  for(k in 1:5){
    ix<-chosen[batch==k]
    field<-function(name)vapply(rows[ix],function(r)scalar(r$canonical[[name]]),character(1))
    d<-data.frame(record_id=ids[ix],title=field("title"),abstract=field("abstract"),
      doi=field("doi"),decision=rep("",200L),notes=rep("",200L),stringsAsFactors=FALSE)
    utils::write.csv(d,file.path(tmp,paste0("batch ",k,".csv")),row.names=FALSE,na="",fileEncoding="UTF-8")
  }
  spec$seed<-as.integer(spec$seed)
  files<-paste0("batch ",1:5,".csv")
  jsonlite::write_json(list(spec=spec,membership_sha256=sha(file.path(tmp,"membership.csv")),
    initial_batch_sha256=setNames(lapply(file.path(tmp,files),sha),files),
    missing_abstracts_by_batch=as.list(setNames(vapply(1:5,function(k)
      sum(!vapply(rows[chosen[batch==k]],function(r)nzchar(scalar(r$canonical$abstract)),logical(1))),integer(1)),files))),
    file.path(tmp,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  if(!file.rename(tmp,out))stop("Could not publish validation batches.")
  message("[W04] PASS: five disjoint batches of 200 from ",length(eligible)," eligible records.")
  invisible(out)
}
