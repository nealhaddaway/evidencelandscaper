#' Run a reproducible random pilot of full-text mechanism assessments
#'
#' Samples from ALL available W09 full texts, including publications assessed
#' under earlier mechanism prompts/ontologies. Sampling never consults earlier
#' mechanism inventories. The exact record list, seed, queue identity and
#' prompt/ontology checksums determine an isolated resumable output directory.
#' All result and audit files are written locally, not to GitHub.
#'
#' @param w09 Optional explicit W09 directory; NULL discovers all journal-literature W09 full-text collections in the map.
#' @param map_path Local OSTARA project directory, used when w09=NULL.
#' @param prompt_file Local full-text mechanism inventory prompt (.txt).
#' @param ontology_file Local approved five-column ontology (.csv).
#' @param sample_size Number of unique full texts, default 10.
#' @param seed Fixed integer random seed, default 20261117.
#' @param model Luna model.
#' @return Versioned local run directory, invisibly.
#' @export
run_ostara_random_fulltext_mechanism_pilot <- function(
    w09=NULL,prompt_file,ontology_file,sample_size=10L,seed=20261117L,
    model="gpt-5.6-luna",map_path=NULL) {
  if(is.null(w09)) {
    if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
    map_path<-normalizePath(map_path,winslash="/",mustWork=TRUE)
    root<-file.path(map_path,"runs","W09")
    candidates<-list.dirs(root,recursive=FALSE,full.names=TRUE)
    candidates<-candidates[file.exists(file.path(candidates,"retrieval_queue.csv"))]
    # Exclude independent grey pilot and provisional/subset queues.
    candidates<-candidates[grepl("^(first10|remaining90)-all-included-",basename(candidates))]
    if(!length(candidates))
      stop("No completed journal W09 all-included collections found in ",root)
    counts<-vapply(candidates,function(p){
      q<-utils::read.csv(file.path(p,"retrieval_queue.csv"),stringsAsFactors=FALSE)
      if(!"record_id"%in%names(q)||anyNA(q$record_id)||anyDuplicated(q$record_id))
        stop("Invalid W09 queue: ",p)
      sum(vapply(as.character(q$record_id),function(id)any(file.exists(c(
        file.path(p,"structured",paste0(id,".xml")),
        file.path(p,"structured",paste0(id,".json")),
        file.path(p,"pdfs",paste0(id,".pdf"))))),logical(1)))
    },integer(1))
    candidates<-candidates[counts>0L]
    if(!length(candidates))stop("No available journal full texts found in ",root)
    # The multi-cohort runner below samples the union and routes each cohort
    # through the established per-W09 inventory extractor.
    return(.ostara_random_pilot_across_w09(
      candidates=candidates,prompt_file=prompt_file,
      ontology_file=ontology_file,sample_size=sample_size,seed=seed,
      model=model,map_path=map_path))
  }
  w09<-normalizePath(w09,winslash="/",mustWork=TRUE)
  prompt_file<-normalizePath(prompt_file,winslash="/",mustWork=TRUE)
  ontology_file<-normalizePath(ontology_file,winslash="/",mustWork=TRUE)
  if(length(sample_size)!=1L||is.na(sample_size)||
     sample_size!=as.integer(sample_size)||sample_size<1L)
    stop("sample_size must be a positive integer")
  if(length(seed)!=1L||is.na(seed)||seed!=as.integer(seed)||
     seed<0L)stop("seed must be a nonnegative integer")
  queue_path<-file.path(w09,"retrieval_queue.csv")
  if(!file.exists(queue_path))stop("W09 retrieval queue missing: ",queue_path)
  queue<-utils::read.csv(queue_path,stringsAsFactors=FALSE)
  if(!"record_id"%in%names(queue)||anyNA(queue$record_id)||
     anyDuplicated(queue$record_id))
    stop("W09 retrieval queue has invalid record IDs")
  ids<-sort(as.character(queue$record_id))
  has_source<-vapply(ids,function(id)
    any(file.exists(c(file.path(w09,"structured",paste0(id,".xml")),
                     file.path(w09,"structured",paste0(id,".json")),
                     file.path(w09,"pdfs",paste0(id,".pdf"))))),logical(1))
  eligible<-ids[has_source]
  if(length(eligible)<sample_size)
    stop("Only ",length(eligible)," W09 records have local full texts; need ",
         sample_size)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  # Local RNG scope: never modify the user's current random-number stream.
  had_seed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed<-get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    if(had_seed)assign(".Random.seed",old_seed,envir=.GlobalEnv)
    else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
      rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed)
  selected<-sort(sample(eligible,size=sample_size,replace=FALSE))
  # Version the selection by content hashes; never reuse a prior prompt or
  # ontology's records solely because they have been inventoried before.
  version<-substr(digest::digest(c(
    sha(queue_path),sha(prompt_file),sha(ontology_file),
    as.character(seed),selected,model),algo="sha256"),1L,16L)
  output_base<-file.path(w09,"random_fulltext_mechanism_pilots",
                         paste0("seed-",seed,"-n-",sample_size,"-",version))
  dir.create(output_base,recursive=TRUE,showWarnings=FALSE)
  sample_path<-file.path(output_base,"random_sample.csv")
  sampling<-data.frame(record_id=selected,
    source_file=vapply(selected,function(id) {
      pp<-c(file.path(w09,"structured",paste0(id,".xml")),
            file.path(w09,"structured",paste0(id,".json")),
            file.path(w09,"pdfs",paste0(id,".pdf")))
      normalizePath(pp[file.exists(pp)][[1L]],winslash="/",mustWork=TRUE)
    },character(1)),stringsAsFactors=FALSE)
  if(file.exists(sample_path)) {
    previous<-utils::read.csv(sample_path,stringsAsFactors=FALSE)
    if(!identical(previous,sampling))
      stop("Existing sampled source register does not match")
  } else utils::write.csv(sampling,sample_path,row.names=FALSE)
  manifest_path<-file.path(output_base,"sampling_manifest.json")
  manifest<-list(schema="ostara-random-fulltext-mechanisms-v1",
     seed=as.integer(seed),sample_size=as.integer(sample_size),
     source_w09=w09,queue_sha256=sha(queue_path),
     prompt_sha256=sha(prompt_file),
     ontology_sha256=sha(ontology_file),
     selected_ids=as.list(selected),model=model)
  if(file.exists(manifest_path)) {
    old<-jsonlite::fromJSON(manifest_path,simplifyVector=FALSE)
    if(!identical(old,manifest))stop("Random pilot manifest mismatch")
  } else jsonlite::write_json(manifest,manifest_path,
                              auto_unbox=TRUE,pretty=TRUE)
  message("[FT mechanism pilot] Random sample ",length(selected),
          " from ",length(eligible)," available full texts; seed ",seed,
          ". Prior inventories not excluded.")
  dest<-extract_ostara_all_fulltext_mechanisms(
    w09=w09,record_ids=selected,max_records=0L,model=model,
    output_dir=output_base,ontology_file=ontology_file,
    prompt_file=prompt_file)
  message("[FT mechanism pilot] Results saved locally: ",dest)
  invisible(dest)
}
