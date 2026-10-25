#!/usr/bin/env Rscript
# Remote execution wrapper only: original installed W01 functions do the work.
args<-commandArgs(TRUE)
if(length(args)!=1L)stop("Usage: w01_runner.R input_dir")
root<-normalizePath(args[[1L]],mustWork=TRUE)
library(evidencelandscaper)
checks<-file.path(root,"sources.sha256.json")
if(!file.exists(checks))stop("Input source checksums missing")
expected<-jsonlite::fromJSON(checks,simplifyVector=TRUE)
for(nm in names(expected)) {
  f<-file.path(root,"sources",paste0(nm,".jsonl"))
  if(!file.exists(f)||!identical(unname(digest::digest(file=f,algo="sha256",serialize=FALSE)),
       unname(expected[[nm]])))stop("Input SHA-256 mismatch: ",nm)
}
inputs<-file.path(root,"sources")
files<-sort(list.files(inputs,pattern="\\.jsonl$",full.names=TRUE))
if(!length(files))stop("No input source files")
source_names<-sub("\\.jsonl$","",basename(files))
manifest<-file.path(root,"runner_work","source_inputs.json")
dir.create(dirname(manifest),recursive=TRUE)
jsonlite::write_json(list(
 schema="living-evidence-map-workflow01-source-inputs-v1",
 status="validated",sources=as.list(setNames(files,source_names))),
 manifest,auto_unbox=TRUE,pretty=TRUE)
message("[W01 Github] Running unmodified existing W01 stages")
candidates<-run_w01_initial(manifest)
scored<-score_w01_candidates(candidates)
if(!file.exists(file.path(scored,"expanded_summary.json")))
 stop("W01 scoring summary missing")
summary<-jsonlite::fromJSON(file.path(scored,"expanded_summary.json"))
if(!identical(summary$status,"success"))stop("W01 scoring not successful")
# Archive only the run worktree; validate all restored outputs locally.
checks<-c("full_scoring/expanded_summary.json",
 "full_scoring/scored_sample.csv", "full_scoring/expanded_scoring_sample.csv",
 "candidates/all_candidate_pairs.csv")
if(any(!file.exists(file.path(root,"runner_work",checks))))
 stop("Required original W01 output missing")
sha<-vapply(file.path(root,"runner_work",checks),function(p)
 digest::digest(file=p,algo="sha256",serialize=FALSE),character(1))
names(sha)<-checks
jsonlite::write_json(as.list(sha),file.path(root,"runner_work","results.sha256.json"),
 auto_unbox=TRUE,pretty=TRUE)
oldwd<-getwd()
setwd(root)
on.exit(setwd(oldwd),add=TRUE)
status<-system2("tar",c("-czf",shQuote(file.path(oldwd,"w01-result.tar.gz")),
                          "runner_work"))
if(!identical(as.integer(status),0L))stop("Failed to archive results")
message("[W01 Github] Complete: ",summary$scoring_sample_n," scored pairs")
