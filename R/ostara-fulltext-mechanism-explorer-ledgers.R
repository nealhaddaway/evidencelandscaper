#' Read authoritative full-text mechanism checkpoints for a standalone explorer
#'
#' This is a read-only fallback when an all-database client export was not made.
#' Do not infer unassessed publications are not_identified.
.ostara_mechanism_explorer_from_ledgers <- function(map_path) {
  w09<-file.path(map_path,"runs","W09")
  d<-list(
    first10=file.path(w09,"ft-mechanism-stage-first10",
      "fulltext_mechanism_inventory","v4-566b51e0959d"),
    remaining90=file.path(w09,"ft-mechanism-stage-remaining90",
      "fulltext_mechanism_inventory","v4-4a462b89bacb"))
  expected_schema<-"ostara-fulltext-mechanism-all-v4"
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  readjl<-function(p) {
    v<-readLines(p,warn=FALSE,encoding="UTF-8")
    lapply(v[nzchar(trimws(v))],jsonlite::fromJSON,simplifyVector=FALSE)
  }
  sc<-function(v) {
    if(is.null(v)||!length(v))return("")
    paste(as.character(unlist(v,use.names=FALSE)),collapse=" | ")
  }
  inventory<-list();novelty<-list();pubs<-list();provenance<-list()
  for(cohort in names(d)) {
    directory<-d[[cohort]]
    mp<-file.path(directory,"manifest.json")
    op<-file.path(directory,"ontology.csv")
    lp<-file.path(directory,"mechanism_assessments.jsonl")
    if(any(!file.exists(c(mp,op,lp))))
      stop("Missing approved ",cohort," mechanism checkpoint files: ",directory)
    m<-jsonlite::fromJSON(mp,simplifyVector=FALSE)
    if(!identical(m$schema,expected_schema)||
       !startsWith(sc(m$prompt_sha256),"f52af5566e56")||
       !startsWith(sc(m$approved_ontology_sha256),"f980a7b995c8")||
       !identical(sha(op),sc(m$approved_ontology_sha256)))
      stop("Unrecognised mechanism checkpoint provenance: ",directory)
    ontology<-utils::read.csv(op,check.names=FALSE,
       stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
    cols<-c("mechanism_id","collective_action_type",
      "high_level_mechanism","fine_level_mechanism","definition")
    if(!all(cols%in%names(ontology))||nrow(ontology)!=30L||
       anyDuplicated(ontology$mechanism_id))
      stop("Approved ontology invalid: ",op)
    queue<-file.path(dirname(dirname(directory)),"retrieval_queue.csv")
    # Original queue is stored on the source W09 run, not in the mechanism
    # inventory directory. Manifest.source_w09 is the authoritative locator.
    queue<-file.path(sc(m$source_w09),"retrieval_queue.csv")
    if(!file.exists(queue))
      stop("Source W09 retrieval queue missing for ",cohort,": ",queue)
    qs<-utils::read.csv(queue,check.names=FALSE,
       stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM")
    if(!all(c("record_id","title","doi")%in%names(qs))||
       anyDuplicated(qs$record_id))
      stop("Invalid archived W09 retrieval queue: ",queue)
    ledger<-readjl(lp)
    ids<-vapply(ledger,function(x)sc(x$record_id),character(1))
    if(anyDuplicated(ids)||any(!ids%in%qs$record_id))
      stop("Unexpected publication ID in full-text mechanism ledger: ",lp)
    idx<-match(ids,qs$record_id)
    pubs[[cohort]]<-data.frame(
      record_id=ids,cohort=cohort,title=qs$title[idx],
      doi=qs$doi[idx],stringsAsFactors=FALSE)
    for(rec in ledger) {
      id<-sc(rec$record_id)
      items<-rec$mechanism_inventory
      codes<-vapply(items,function(z)sc(z$mechanism_id),character(1))
      if(length(items)!=30L||anyDuplicated(codes)||
         !setequal(codes,ontology$mechanism_id))
        stop("Incomplete 30-code ledger record: ",id)
      for(z in items) {
        row<-ontology[match(sc(z$mechanism_id),
                            ontology$mechanism_id),cols,drop=FALSE]
        inventory[[length(inventory)+1L]]<-data.frame(
          record_id=id,cohort=cohort,
          mechanism_id=sc(z$mechanism_id),
          collective_action_type=row$collective_action_type,
          high_level_mechanism=row$high_level_mechanism,
          fine_level_mechanism=row$fine_level_mechanism,
          definition=row$definition,status=sc(z$status),
          evidence_quote=sc(z$source_quote),
          evidence_anchor=sc(z$source_anchor),
          evidence_review_required=as.character(
            isTRUE(rec$evidence_review_required)),
          source_run=basename(directory),stringsAsFactors=FALSE)
      }
      for(z in rec$new_novelty) {
        novelty[[length(novelty)+1L]]<-data.frame(
          record_id=id,cohort=cohort,source_stage="full_text",
          proposed_label=sc(z$proposed_label),
          working_definition=sc(z$working_definition),
          closest_existing_mechanism_id=sc(z$closest_approved_mechanism_id),
          support_status=sc(z$support_status),
          evidence_quote=sc(z$source_quote),
          evidence_anchor=sc(z$source_anchor),stringsAsFactors=FALSE)
      }
    }
    provenance[[cohort]]<-data.frame(
      table="mechanisms_and_ft_candidates",
      source_file=lp,sha256=sha(lp),
      records=length(ledger),stringsAsFactors=FALSE)
  }
  inv<-do.call(rbind,inventory)
  pub<-do.call(rbind,pubs)
  if(anyDuplicated(inv[c("record_id","mechanism_id")])||
     anyDuplicated(pub$record_id))
    stop("Mechanism publications overlap across first10 and remaining90")
  candidate_cols<-c("record_id","cohort","source_stage",
    "proposed_label","working_definition","closest_existing_mechanism_id",
    "support_status","evidence_quote","evidence_anchor")
  cand<-if(length(novelty))do.call(rbind,novelty) else
    as.data.frame(setNames(replicate(length(candidate_cols),character(),
       simplify=FALSE),candidate_cols),stringsAsFactors=FALSE)
  list(inv=inv,candidates=cand,pubs=pub,
       provenance=do.call(rbind,provenance),
       source_files=c(
         first10=file.path(d$first10,"mechanism_assessments.jsonl"),
         remaining90=file.path(d$remaining90,"mechanism_assessments.jsonl")))
}
