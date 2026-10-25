#' Dispatch existing W01 R computation to optional GitHub runner
#' @param run_name Existing named evidence-map run.
#' @param map Existing map path.
#' @param repo Target repo with optional-w01.yml workflow.
#' @param allow_public_inputs Explicit acknowledgement if repo is public.
#' @return GitHub transfer tag.
#' @export
dispatch_w01_github <- function(run_name,map="Ostara_collective_action_map",
                                repo="nealhaddaway/evidencelandscaper",
                                allow_public_inputs=FALSE) {
  map<-open_map(map)
  if(!nzchar(Sys.which("gh")))stop("Install and authenticate GitHub CLI (gh auth login).")
  home<-file.path(map,"runs","orchestrated",run_name)
  state_path<-file.path(home,"state.json")
  if(!file.exists(state_path))stop("Run state missing.")
  state<-jsonlite::fromJSON(state_path,simplifyVector=FALSE)
  if(!identical(state$status,"w01_prepared"))
    stop("Dispatch requires saved W01 preparation. Current status: ",state$status,
         ". Never dispatch an already running or scored W01.")
  if(!is.list(state$w01)||!file.exists(state$w01$prepared_manifest))
    stop("Saved W01 source manifest missing.")
  private<-system2("gh",c("repo","view",repo,"--json","isPrivate",
                          "--jq",".isPrivate"),stdout=TRUE,stderr=TRUE)
  if(!identical(attr(private,"status"),NULL) || !length(private) ||
     !private[[1L]]%in%c("true","false"))
    stop("Cannot verify GitHub repository visibility.")
  if(identical(private[[1L]],"false") && !isTRUE(allow_public_inputs))
    stop("Public repository: release assets may expose licensed records. ",
         "Explicitly set allow_public_inputs=TRUE only if redistribution is permitted.")
  m<-jsonlite::fromJSON(state$w01$prepared_manifest,simplifyVector=FALSE)
  if(!identical(m$status,"validated"))stop("W01 sources not validated.")
  src<-vapply(m$sources,as.character,character(1))
  if(any(!file.exists(src)))stop("W01 source files missing.")
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  stage<-tempfile(pattern="w01-transfer-")
  dir.create(file.path(stage,"sources"),recursive=TRUE)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  for(nm in names(src)) {
    dest<-file.path(stage,"sources",paste0(nm,".jsonl"))
    if(!file.copy(src[[nm]],dest))stop("Could not stage W01 source: ",nm)
    if(!identical(sha(src[[nm]]),sha(dest)))stop("Staged source checksum mismatch: ",nm)
  }
  input_hashes<-vapply(src,sha,character(1))
  jsonlite::write_json(as.list(input_hashes),
    file.path(stage,"sources.sha256.json"),auto_unbox=TRUE,pretty=TRUE)
  tag<-paste0("w01-transfer-",substr(state$search_sha256,1L,12L),"-",
              format(Sys.time(),"%Y%m%dT%H%M%S",tz="UTC"))
  input<-file.path(stage,"w01-input.tar.gz")
  oldwd<-getwd()
  setwd(stage)
  tarstatus<-tryCatch(system2("tar",c("-czf","w01-input.tar.gz","sources","sources.sha256.json")),
                      finally=setwd(oldwd))
  if(!identical(as.integer(tarstatus),0L))stop("Failed to package W01 inputs.")
  run_gh<-function(args) {
    res<-system2("gh",args,stdout=TRUE,stderr=TRUE)
    if(!is.null(attr(res,"status")) && attr(res,"status")!=0L)
      stop("GitHub CLI failed: ",paste(res,collapse="\n"))
    invisible(res)
  }
  # Private-release assets are explicitly uploaded only after opt-in.
  run_gh(c("release","create",tag,"--repo",repo,"--target","main",
           "--title",tag,"--notes","W01",
           "--latest=false",input))
  run_gh(c("workflow","run","optional-w01.yml","--repo",repo,
           "-f",paste0("transfer_tag=",tag)))
  state$w01$github<-list(tag=tag,repo=repo,input_sha256=sha(input))
  state$status<-"w01_remote_submitted"
  jsonlite::write_json(state,state_path,auto_unbox=TRUE,pretty=TRUE,null="null")
  message("[W01] Submitted GitHub runner: ",tag,
          ". Follow progress in GitHub Actions; use collect_w01_github() when complete.")
  invisible(tag)
}

#' Retrieve verified W01 scoring output from a GitHub runner
#' @param run_name Existing named evidence-map run.
#' @param map Existing map path.
#' @return Path to downloaded W01 scoring folder.
#' @export
collect_w01_github <- function(run_name,map="Ostara_collective_action_map") {
  map<-open_map(map)
  home<-file.path(map,"runs","orchestrated",run_name)
  sf<-file.path(home,"state.json")
  state<-jsonlite::fromJSON(sf,simplifyVector=FALSE)
  if(!identical(state$status,"w01_remote_submitted"))
    stop("No pending W01 remote submission.")
  tag<-state$w01$github$tag
  repo<-state$w01$github$repo
  dest<-file.path(map,"runs","W01",paste0("github-",tag))
  if(dir.exists(dest))stop("Remote output already exists; refusing overwrite.")
  stage<-tempfile(pattern="w01-download-")
  dir.create(stage)
  on.exit(unlink(stage,recursive=TRUE),add=TRUE)
  result<-system2("gh",c("release","download",tag,"--repo",repo,
     "--pattern","w01-result.tar.gz","--dir",stage),stdout=TRUE,stderr=TRUE)
  if(!is.null(attr(result,"status"))&&attr(result,"status")!=0L)
    stop("Runner output not ready or download failed: ",paste(result,collapse="\n"))
  archive<-file.path(stage,"w01-result.tar.gz")
  contents<-utils::untar(archive,list=TRUE)
  if(!length(contents)||any(startsWith(contents,"/"))||
     any(grepl("(^|/)\\.\\.(/|$)",contents))||
     any(!startsWith(contents,"runner_work/")))
    stop("Unsafe remote archive paths.")
  dir.create(dest,recursive=TRUE)
  utils::untar(archive,exdir=dest)
  scored<-file.path(dest,"runner_work","full_scoring")
  summary<-file.path(scored,"expanded_summary.json")
  if(!file.exists(summary))stop("Remote scoring summary absent; no state transition.")
  x<-jsonlite::fromJSON(summary,simplifyVector=FALSE)
  if(!identical(x$status,"success"))stop("Remote W01 scoring not successful.")
  checks<-file.path(dest,"runner_work","results.sha256.json")
  if(!file.exists(checks))stop("Remote result checksums missing; no state transition.")
  manifest<-jsonlite::fromJSON(checks,simplifyVector=TRUE)
  for(nm in names(manifest)) {
    file<-file.path(dest,"runner_work",nm)
    if(!file.exists(file)||!identical(unname(digest::digest(file=file,algo="sha256",serialize=FALSE)),
          unname(manifest[[nm]])))
      stop("Remote checksum failed for ",nm,"; refusing handover.")
  }
  state$w01$scored<-scored
  state$w01$candidates<-file.path(dest,"runner_work","candidates")
  state$status<-"w01_scored"
  jsonlite::write_json(state,sf,auto_unbox=TRUE,pretty=TRUE,null="null")
  message("[W01] Imported GitHub scoring output: ",scored,
          ". Complete duplicate adjudication through existing RStudio workflow.")
  invisible(scored)
}
