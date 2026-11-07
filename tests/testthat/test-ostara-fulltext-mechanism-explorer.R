test_that("full-text mechanism explorer preserves all statuses and evidence", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root<-tempfile("mechanism-explorer-")
  dir.create(root)
  export<-file.path(root,"reports","client_databases","fixture")
  dir.create(export,recursive=TRUE)
  ids<-sprintf("M%d.%d.%d",rep(1:3,each=10),rep(1:2,each=5,times=3),1:30)
  stopifnot(length(unique(ids))==30L)
  inventory<-data.frame(
    record_id=rep("record_001",30),cohort=rep("first10",30),
    mechanism_id=ids,
    collective_action_type=rep("Collective coordination",30),
    high_level_mechanism=rep("High-level group",30),
    fine_level_mechanism=paste("Mechanism",seq_len(30)),
    definition=paste("Definition",seq_len(30)),
    status=c("implemented_case","mentioned_only","proposed_only",
             "unclear",rep("not_identified",26)),
    evidence_quote=c("verbatim A","verbatim B","verbatim C","verbatim D",
                     rep("",26)),
    evidence_anchor=c("[PAGE 1]","[PAGE 2]","[PAGE 3]","[PAGE 4]",
                      rep("",26)),
    evidence_review_required=rep("FALSE",30),
    source_run=rep("v4-sample",30))
  proposals<-data.frame(
    record_id="record_001",cohort="first10",source_stage="full_text",
    proposed_label="Proposed novel mechanism",
    working_definition="New coordination idea",
    closest_existing_mechanism_id=ids[[1L]],
    support_status="implemented",evidence_quote="verbatim A",
    evidence_anchor="[PAGE 1]",
    stringsAsFactors=FALSE)
  publications<-data.frame(record_id="record_001",cohort="first10",
    title="Article One",doi="10.1000/example",stringsAsFactors=FALSE)
  provenance<-data.frame(
    table=rep("mechanisms_and_ft_candidates",2),
    source_file=c("first10/mechanism_assessments.jsonl",
                  "remaining90/mechanism_assessments.jsonl"),
    sha256=c(paste(rep("a",64),collapse=""),
             paste(rep("b",64),collapse="")))
  tables<-list(
    ostara_fulltext_mechanisms_with_adjacent_evidence=inventory,
    ostara_candidate_mechanisms_source=proposals,
    ostara_publications=publications,
    ostara_export_source_manifest=provenance)
  for(name in names(tables))
    utils::write.csv(tables[[name]],file.path(export,paste0(name,".csv")),
                     row.names=FALSE,na="")
  out<-export_ostara_fulltext_mechanism_explorer(
    map_path=root,client_export_dir=export)
  expect_true(file.exists(out))
  txt<-paste(readLines(out,encoding="UTF-8",warn=FALSE),collapse="\n")
  expect_true(grepl("verbatim A",txt,fixed=TRUE))
  expect_true(grepl("Article One",txt,fixed=TRUE))
  expect_true(grepl("Proposed novel mechanism",txt,fixed=TRUE))
  expect_true(grepl("not_identified",txt,fixed=TRUE))
  expect_true(file.exists(file.path(dirname(out),"provenance.json")))
  expect_identical(out,export_ostara_fulltext_mechanism_explorer(
    map_path=root,client_export_dir=export))
  broken<-inventory[-1,,drop=FALSE]
  utils::write.csv(broken,
    file.path(export,"ostara_fulltext_mechanisms_with_adjacent_evidence.csv"),
    row.names=FALSE,na="")
  expect_error(export_ostara_fulltext_mechanism_explorer(
    map_path=root,client_export_dir=export),"30 codes")
})
