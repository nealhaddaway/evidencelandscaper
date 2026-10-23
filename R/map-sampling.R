#' Select a reproducible subset of publication identifiers
#'
#' Sampling always uses unique, sorted record IDs, and stores the selected IDs.
#' This does not alter the full corpus. Uncertain records must be removed from
#' the eligible set upstream before this function is called.
#' @param record_ids Unique candidate identifiers.
#' @param choice List(mode="all"|"n"|"percent", n or percent).
#' @param seed Integer random seed.
#' @param output_csv A new output path; never overwrites previous selections.
#' @return A list with selected record IDs and source population size.
#' @export
sample_evidence_records <- function(record_ids,choice,seed=20261023L,output_csv=NULL) {
  if(!is.character(record_ids)||anyNA(record_ids)||
     any(!nzchar(record_ids))||anyDuplicated(record_ids))
    stop("Record IDs must be unique non-empty strings.")
  record_ids <- sort(record_ids,method="radix")
  selected <- .orchestrator_choice(choice,length(record_ids))
  if(length(seed)!=1L || is.na(seed) || seed!=as.integer(seed))
    stop("seed must be one integer.")
  if(selected$selected==length(record_ids)) ids <- record_ids
  else {
    # Restore caller's random state, including when no state existed.
    existed<-exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
    if(existed) old<-get(".Random.seed",envir=.GlobalEnv)
    on.exit({
      if(existed)assign(".Random.seed",old,envir=.GlobalEnv)
      else if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
        rm(".Random.seed",envir=.GlobalEnv)
    },add=TRUE)
    set.seed(as.integer(seed))
    ids <- sort(sample(record_ids,selected$selected,replace=FALSE),method="radix")
  }
  if(!is.null(output_csv)) {
    if(file.exists(output_csv))stop("Sampling output already exists: ",output_csv)
    dir.create(dirname(output_csv),recursive=TRUE,showWarnings=FALSE)
    utils::write.csv(data.frame(record_id=ids),output_csv,row.names=FALSE)
  }
  list(record_ids=ids,population=length(record_ids),sample_size=length(ids),
       seed=as.integer(seed),mode=choice$mode,
       selection_sha256=digest::digest(ids,algo="sha256"))
}
