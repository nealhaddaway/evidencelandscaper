testthat::test_that("novelty status is explicit and historical ontology remains unchanged", {
 td <- tempfile("ostara-novelty-");dir.create(td)
 ontology<-file.path(td,"ontology.csv")
 utils::write.csv(data.frame(mechanism_id="C05.04"),ontology,row.names=FALSE)
 novelty<-file.path(td,"verified_novelty.csv")
 x<-data.frame(
 record_id=c("work-1","work-2"),
 candidate_id=c("FT-1","FT-2"),
 case_id=c("case-1","case-2"),
 w07_candidate_id=c("W07-0001",""),
 human_novelty_decision=c("confirmed_new","covered_by_existing"),
 closest_approved_mechanism_id=c("","C05.04"),
 proposed_label=c("Novel mechanism","Existing mechanism"),
 working_definition=c("Shared operational process","Shared learning"),
 evidence_quote=c("Evidence quotation","Quotation"),
 source_anchor=c("PAGE 4","PAGE 8"),
 reviewer=c("reviewer","reviewer"),
 review_timestamp=c("2026-10-27","2026-10-27"))
 utils::write.csv(x,novelty,row.names=FALSE)
 old<-digest::digest(file=ontology,algo="sha256")
 results<-reconcile_ostara_novelty(novelty,ontology,file.path(td,"out"))
 y<-utils::read.csv(results$csv)
 testthat::expect_equal(y$effective_novelty_status,
    c("candidate_for_harmonisation","already_in_ontology"))
 testthat::expect_equal(digest::digest(file=ontology,algo="sha256"),old)
 testthat::expect_error(reconcile_ostara_novelty(novelty,ontology,file.path(td,"out")),"overwrite")
})
