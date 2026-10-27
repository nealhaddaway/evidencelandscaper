#' Prepare ontology-harmonisation inputs from human-reviewed W09 novelty
#'
#' The source proposal table is generated without model calls or unapproved
#' ontology assignments. Humans must assign the parent category and
#' classification in the harmonisation worksheet before website publication.
#' @param novelty_reconciliation_csv Output of reconcile_ostara_novelty().
#' @param output_dir New directory for candidate source and harmonisation CSVs.
#' @return Output paths.
#' @export
prepare_ostara_fulltext_candidates <- function(novelty_reconciliation_csv,output_dir) {
  if(!file.exists(novelty_reconciliation_csv))stop("Missing reconciled novelty CSV")
  if(dir.exists(output_dir)&&length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Refusing to overwrite full-text harmonisation outputs")
  x<-utils::read.csv(novelty_reconciliation_csv,stringsAsFactors=FALSE,
                    check.names=FALSE,fileEncoding="UTF-8-BOM")
  needed<-c("record_id","candidate_id","case_id","proposed_label",
            "working_definition","evidence_quote","source_anchor",
            "effective_novelty_status","w07_candidate_id")
  if(!all(needed%in%names(x)))stop("Novelty reconciliation is missing required fields")
  x<-x[x$effective_novelty_status=="candidate_for_harmonisation",,drop=FALSE]
  if(anyDuplicated(x$candidate_id))stop("Duplicated accepted FT candidate")
  sources<-data.frame(candidate_id=x$candidate_id,source_search=rep("case_ft",nrow(x)),
    original_description=x$proposed_label,
    original_gap_reason=x$working_definition,case_id=x$case_id,
    record_id=x$record_id,evidence_quote=x$evidence_quote,
    source_anchor=x$source_anchor,w07_candidate_id=x$w07_candidate_id,
    stringsAsFactors=FALSE)
  # Draft one row per source suggestion. Consolidation requires comparison of
  # all suggestions and the approved ontology; not inferred by label similarity.
  harmonised<-data.frame(proposal_id=paste0("FTH",sprintf("%04d",seq_len(nrow(x)))),
    proposed_label=x$proposed_label,working_definition=x$working_definition,
    hierarchy_action=rep("new_lower_level",nrow(x)),
    source_candidate_ids=x$candidate_id,
    review_status=rep("pending_harmonisation",nrow(x)),
    parent_category_code=rep("",nrow(x)),
    proposed_new_parent_category=rep("",nrow(x)),
    closest_existing_mechanism_code=rep("",nrow(x)),
    closest_existing_mechanism_label=rep("",nrow(x)),
    classification_rationale=rep("",nrow(x)),
    w07_abstract_candidate_ids=x$w07_candidate_id,
    stringsAsFactors=FALSE)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  a<-file.path(output_dir,"fulltext_candidates.csv")
  b<-file.path(output_dir,"fulltext_harmonisation_REVIEW_REQUIRED.csv")
  utils::write.csv(sources,a,row.names=FALSE,na="")
  utils::write.csv(harmonised,b,row.names=FALSE,na="")
  invisible(list(candidates=a,harmonisation_review=b))
}
