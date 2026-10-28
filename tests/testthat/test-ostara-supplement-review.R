testthat::test_that("supplement review retains zero-abstract mechanisms and case evidence", {
 td<-tempfile("ostara-supplement-");dir.create(td)
 review<-file.path(td,"review");dir.create(review)
 base<-data.frame(record_id="work-1",mechanism_id="C01.01",
   abstract_assigned=TRUE,case_ids="work-1-case-001")
 utils::write.csv(base,file.path(review,"mechanism_review.csv"),row.names=FALSE)
 prior<-data.frame(record_id=character(),candidate_id=character(),
   case_id=character(),w07_candidate_id=character(),human_novelty_decision=character(),
   closest_approved_mechanism_id=character(),proposed_label=character(),
   working_definition=character(),evidence_quote=character(),source_anchor=character(),
   reviewer=character(),review_timestamp=character())
 utils::write.csv(prior,file.path(review,"novelty_review.csv"),row.names=FALSE)
 jsonlite::write_json(list(w07_run=file.path(td,"W07")),
   file.path(review,"manifest.json"),auto_unbox=TRUE)
 sup<-file.path(td,"supplement");dir.create(sup)
 z<-list(record_id="work-1",eligible_case_ids=list("work-1-case-001"),
    mechanism_inventory=list(
      list(mechanism_id="C01.01",status="mentioned_only",case_ids=list(),
           source_quote="Mentioned",source_anchor="[PAGE 2]"),
      list(mechanism_id="C01.02",status="implemented_case",
           case_ids=list("work-1-case-001"),
           source_quote="Collaboration",source_anchor="[PAGE 3]")),
    abstract_novelty_validation=list(),new_novelty=list())
 writeLines(as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
   file.path(sup,"mechanism_assessments.jsonl"))
 jsonlite::write_json(list(eligible_record_ids=list("work-1")),
   file.path(sup,"manifest.json"),auto_unbox=TRUE)
 out<-prepare_ostara_supplement_review(review,sup)
 rows<-utils::read.csv(out$mechanisms)
 testthat::expect_equal(nrow(rows),2L)
 testthat::expect_equal(rows$w09_assessment,c("mentioned_only","implemented_case"))
 testthat::expect_false(any(rows$review_complete))
 testthat::expect_false(any(rows$eligible_case_verified))
 testthat::expect_equal(rows$case_ids[[2]],"work-1-case-001")
 testthat::expect_error(prepare_ostara_supplement_review(review,sup),
   "already exists")
})
