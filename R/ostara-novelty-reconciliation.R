#' Reconcile reviewed full-text novelty against W07 abstract suggestions
#'
#' Produces a separate immutable publication-level novelty ledger. This never
#' edits W07 abstract suggestions or promotes candidates to approved codes.
#' @param novelty_csv Human-reviewed novelty decisions from W10.
#' @param ontology_csv The approved W07 ontology.
#' @param output_dir New directory for reconciled output.
#' @return Output paths.
#' @export
reconcile_ostara_novelty <- function(novelty_csv,ontology_csv,output_dir) {
  if(!all(file.exists(c(novelty_csv,ontology_csv))))
    stop("Novelty decisions and approved ontology are required")
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Refusing to overwrite a nonempty novelty output directory")
  x<-utils::read.csv(novelty_csv,stringsAsFactors=FALSE,check.names=FALSE,
                      fileEncoding="UTF-8-BOM")
  onto<-utils::read.csv(ontology_csv,stringsAsFactors=FALSE,check.names=FALSE,
                         fileEncoding="UTF-8-BOM")
  cols<-c("record_id","candidate_id","case_id","w07_candidate_id",
          "human_novelty_decision","closest_approved_mechanism_id",
          "proposed_label","working_definition","evidence_quote",
          "source_anchor","reviewer","review_timestamp")
  missing<-setdiff(cols,names(x))
  if(length(missing))stop("Missing novelty verification fields: ",
                         paste(missing,collapse=", "))
  statuses<-c("confirmed_new","refined_new","covered_by_existing",
              "rejected","uncertain")
  if(anyNA(x[,cols])||any(!x$human_novelty_decision%in%statuses))
    stop("Invalid/missing novelty decisions")
  if(anyDuplicated(x$candidate_id)||any(!nzchar(trimws(x$candidate_id))))
    stop("Novelty candidate identifiers must be unique and nonempty")
  if(any(!nzchar(trimws(x$record_id)))||any(!nzchar(trimws(x$reviewer))))
    stop("Publication and reviewer are mandatory")
  accepted<-x$human_novelty_decision%in%c("confirmed_new","refined_new")
  if(any(accepted & (!nzchar(trimws(x$case_id))|
                      !nzchar(trimws(x$proposed_label))|
                      !nzchar(trimws(x$working_definition))|
                      !nzchar(trimws(x$evidence_quote))|
                      !nzchar(trimws(x$source_anchor)))))
    stop("Confirmed new mechanisms require cases, definitions and evidence")
  covered<-x$human_novelty_decision=="covered_by_existing"
  if(any(covered & !(x$closest_approved_mechanism_id%in%
                     as.character(onto$mechanism_id))))
    stop("Existing-mechanism classification requires an approved ontology code")
  x$effective_novelty_status<-ifelse(accepted,"candidate_for_harmonisation",
    ifelse(covered,"already_in_ontology",
           ifelse(x$human_novelty_decision=="rejected","rejected","unresolved")))
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  out<-file.path(output_dir,"novelty_reconciliation.csv")
  utils::write.csv(x,out,row.names=FALSE,na="")
  hash<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  manifest<-file.path(output_dir,"novelty_manifest.json")
  jsonlite::write_json(list(schema="ostara-novelty-feedback-v1",
     input_sha256=hash(novelty_csv),ontology_sha256=hash(ontology_csv),
     output_sha256=hash(out)),manifest,auto_unbox=TRUE,pretty=TRUE)
  invisible(list(csv=out,manifest=manifest))
}
