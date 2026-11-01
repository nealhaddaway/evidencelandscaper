# Internal schema selector for legacy and client v4 W07 ontology tables.
# Retains original source column names; no synthetic indicators or codes.
.w07_ontology_columns <- function(rows) {
  legacy<-c("category_id","mechanism_id","mechanism_source_label",
            "definition_draft","abstract_indicators_draft",
            "not_sufficient_draft")
  v4<-c("mechanism_id","collective_action_type","high_level_mechanism",
        "fine_level_mechanism","definition")
  if(all(v4 %in% names(rows))) {
    if(nrow(rows)!=28L)
      stop("The versioned ontology must contain exactly 28 mechanisms.")
    for(k in v4) {
      x<-as.character(rows[[k]])
      if(anyNA(x)||any(!nzchar(trimws(x))))
        stop("Missing ontology value in ",k)
    }
    columns<-v4
    format<-"hierarchical_v4"
  } else if(all(legacy %in% names(rows))) {
    columns<-legacy
    format<-"legacy"
  } else stop("Ontology must have either the five hierarchical v4 columns ",
              "or the six original W07 columns.")
  codes<-trimws(as.character(rows$mechanism_id))
  if(anyNA(codes)||any(!nzchar(codes))||anyDuplicated(codes))
    stop("Ontology IDs missing or repeated.")
  list(format=format,required=columns,codes=codes)
}
