#' Resolve previously used Ostara prompts and ontology without file pickers
#'
#' Only completed W04 screening runs and W07 archives with matching
#' provenance hashes are eligible. Conflicting W04 prompts or W07
#' ontology/prompt pairs cause an explicit stop, not a silent guess.
.orchestrator_reuse_inputs <- function(map, supplied) {
  sha<-function(p) digest::digest(file=p,algo="sha256",serialize=FALSE)
  w04_hash<-function(p)digest::digest(paste(readLines(p,warn=FALSE,encoding="UTF-8"),collapse="\n"),algo="sha256",serialize=FALSE)
  if(is.null(supplied$screening_prompt)) {
    parent<-file.path(map,"runs","W01")
    roots<-if(dir.exists(parent))list.dirs(parent,recursive=FALSE) else character()
    w04<-unlist(lapply(roots,function(r){
      p<-file.path(r,"W04_local")
      if(dir.exists(p))list.dirs(p,recursive=FALSE) else character()
    }),use.names=FALSE)
    w04<-w04[file.exists(file.path(w04,"finalised","summary.json")) &
               file.exists(file.path(w04,"local_manifest.json")) &
               file.exists(file.path(w04,"screening_prompt.txt"))]
    valid<-Filter(function(d) {
      m<-tryCatch(jsonlite::fromJSON(file.path(d,"local_manifest.json")),
                  error=function(e)NULL)
      z<-tryCatch(jsonlite::fromJSON(file.path(d,"finalised","summary.json")),
                  error=function(e)NULL)
      !is.null(m) && !is.null(z) && identical(z$status,"final") &&
        identical(m$prompt_sha256,w04_hash(file.path(d,"screening_prompt.txt")) )
    },w04)
    if(!length(valid))stop("No completed, checksum-verified Ostara W04 screening prompt found.")
    # The finalised run, not an arbitrary older trial, is the authority.
    hashes<-unique(vapply(valid,function(d)w04_hash(file.path(d,"screening_prompt.txt")),character(1)))
    if(length(hashes)!=1L)
      stop("Completed Ostara W04 runs use different prompts; cannot identify a single approved version.")
    supplied$screening_prompt<-file.path(valid[[1L]],"screening_prompt.txt")
    message("[Orchestrator] Reusing verified W04 screening prompt: ",supplied$screening_prompt)
  }
  if(is.null(supplied$w07_prompt)||is.null(supplied$ontology_file)) {
    root<-file.path(map,"runs","W07")
    runs<-if(dir.exists(root))list.dirs(root,recursive=FALSE) else character()
    runs<-runs[file.exists(file.path(runs,"ontology.csv")) &
                 file.exists(file.path(runs,"prompt.txt")) &
                 file.exists(file.path(runs,"manifest.json"))]
    runs<-Filter(function(d){
      m<-tryCatch(jsonlite::fromJSON(file.path(d,"manifest.json")),error=function(e)NULL)
      !is.null(m) &&
        identical(m$ontology_sha256,sha(file.path(d,"ontology.csv"))) &&
        identical(m$prompt_sha256,sha(file.path(d,"prompt.txt")))
    },runs)
    if(!length(runs))
      stop("No checksum-verified archived Ostara W07 ontology and prompt pair exists.")
    # The last developed pair should be used only when its approved codebook
    # signature is unambiguous; earlier experimental v2 codebooks are excluded.
    runs<-Filter(function(d) {
      tab<-tryCatch(utils::read.csv(file.path(d,"ontology.csv"),
        stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM"),error=function(e)NULL)
      !is.null(tab)&&"mechanism_id"%in%names(tab)&&
        nrow(tab)==36L && !anyDuplicated(tab$mechanism_id)
    },runs)
    if(!length(runs))
      stop("No verified 36-code Ostara W07 ontology archive found; refusing to reuse an earlier experimental codebook.")
    pairs<-unique(vapply(runs,function(d)paste(sha(file.path(d,"ontology.csv")),
                                                sha(file.path(d,"prompt.txt")),sep=":"),
                         character(1)))
    if(length(pairs)!=1L)
      stop("Several different verified 36-code W07 prompt/ontology pairs exist; approval cannot be inferred.")
    if(is.null(supplied$ontology_file))supplied$ontology_file<-file.path(runs[[1L]],"ontology.csv")
    if(is.null(supplied$w07_prompt))supplied$w07_prompt<-file.path(runs[[1L]],"prompt.txt")
    message("[Orchestrator] Reusing verified 36-code W07 pair from ",runs[[1L]])
  }
  supplied
}
