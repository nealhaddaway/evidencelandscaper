#' Prepare W09 retrieval from a W08 corpus run, including flagged records
#'
#' Reads the saved W08 assessments without modifying W08. Publications with
#' specific, multiple or unclear cases are eligible. Optionally includes
#' unvalidated quotation failures as flagged retrieval candidates only.
#' @param w08 W08 run directory.
#' @param include_unvalidated Include failed quotation IDs provisionally.
#' @param map_path Evidence-map directory.
#' @return Prepared W09 directory with an auditable selection CSV.
#' @export
prepare_w09_from_w08 <- function(w08,include_unvalidated=TRUE,map_path=NULL) {
  if(is.null(map_path))map_path<-open_map("Ostara_collective_action_map")
  map_path<-normalizePath(map_path,mustWork=TRUE)
  w08<-normalizePath(w08,mustWork=TRUE)
  if(!identical(dirname(dirname(w08)),file.path(map_path,"runs")))
    stop("W08 folder is not in this map's runs directory.")
  p<-file.path(w08,"case_assessments.jsonl")
  if(!file.exists(p))stop("W08 assessments missing.")
  readjl<-function(x) {
    if(!file.exists(x))return(list())
    l<-readLines(x,warn=FALSE,encoding="UTF-8")
    lapply(l[nzchar(trimws(l))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  valid<-readjl(p)
  ids<-vapply(valid,function(x)x$record_id,character(1))
  if(anyDuplicated(ids))stop("Duplicated W08 record IDs.")
  eligible<-Filter(function(z)z$case_evidence_status%in%
    c("specific_case","multiple_cases","unclear"),valid)
  candidates<-vapply(eligible,function(z)z$record_id,character(1))
  reasons<-setNames(vapply(eligible,function(z)z$case_evidence_status,
                          character(1)),candidates)
  if(isTRUE(include_unvalidated)) {
    invalid<-readjl(file.path(w08,"invalid_evidence_responses.jsonl"))
    bad<-unique(vapply(invalid,function(z)z$record_id,character(1)))
    bad<-setdiff(bad,ids)
    for(id in bad)reasons[[id]]<-"unvalidated_W08_quotation"
    candidates<-c(candidates,bad)
  }
  candidates<-sort(unique(candidates))
  if(!length(candidates))stop("No eligible W09 candidates.")
  w09<-prepare_w09_cases(map_path=map_path,record_ids=candidates)
  selection<-data.frame(record_id=candidates,
    w08_status=unname(reasons[candidates]),
    evidence_validation=ifelse(grepl("^unvalidated_",reasons[candidates]),
      "not_verified","validated"),stringsAsFactors=FALSE)
  file<-file.path(w09,"w08_to_w09_selection.csv")
  if(file.exists(file)) {
    old<-utils::read.csv(file,stringsAsFactors=FALSE)
    if(!identical(old,selection))stop("W09 selection changed; refusing overwrite.")
  } else utils::write.csv(selection,file,row.names=FALSE,na="")
  message("[W09] Selected ",nrow(selection)," candidate publications (",
    sum(selection$evidence_validation=="not_verified"),
    " unvalidated W08 quotations).")
  invisible(w09)
}
