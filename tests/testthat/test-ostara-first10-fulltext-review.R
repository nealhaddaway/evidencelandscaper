testthat::test_that("First10 full-text review exports separate evidence-anchored CSVs", {
  td<-tempfile("ft-human-review-")
  dir.create(td,recursive=TRUE)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  mechanism<-file.path(td,"fulltext_mechanism_inventory","v4-example")
  insights<-file.path(td,"fulltext_insights","v1-example")
  dir.create(mechanism,recursive=TRUE)
  dir.create(insights,recursive=TRUE)
  j<-function(x,p)jsonlite::write_json(x,p,auto_unbox=TRUE,pretty=TRUE)
  jl<-function(x,p)writeLines(vapply(x,function(z)
    as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
    character(1)),p)
  j(list(schema="ostara-fulltext-mechanism-all-v4"),
    file.path(mechanism,"manifest.json"))
  utils::write.csv(data.frame(record_id=c("a","b"),
    availability=c("available","full_text_missing"),
    source_path=c("a.pdf",""),source_sha256=c("sha","")),
    file.path(mechanism,"source_inventory.csv"),row.names=FALSE)
  jl(list(list(record_id="a",mechanism_inventory=list(list(
    mechanism_id="M1.1.1",status="implemented_case",
    source_quote="A joint monitoring arrangement operated",
    source_anchor="[SECTION 1]",reason="Implemented")),
    new_novelty=list(),
    case_presence_assessment=list(case_status="no_case",
      candidate_case_count=0L,candidate_cases=list(),
      assessment_reason="No distinguishable case"))),
    file.path(mechanism,"mechanism_assessments.jsonl"))
  j(list(schema="ostara-first10-ft-insights-v1",
       source_mechanism_run=mechanism,
       selected_no_abstract_case_ids=list("a","b")),
    file.path(insights,"manifest.json"))
  jl(list(list(record_id="a",status="insights_identified",
    assessment_reason="A relevant finding",insights=list(list(
      insight_type="enabler",summary="Joint monitoring reduces duplication",
      context="Local actors",evidence_basis="observed",
      source_quote="joint monitoring reduced costs",source_anchor="[SECTION 3]",
      reason="Direct finding")))),
    file.path(insights,"insights_assessments.jsonl"))
  x<-evidencelandscaper::prepare_ostara_first10_fulltext_review(
    mechanism,insights)
  testthat::expect_true(all(file.exists(unlist(x)[1:5])))
  me<-utils::read.csv(x$mechanisms,stringsAsFactors=FALSE)
  testthat::expect_equal(me$mechanism_id,"M1.1.1")
  testthat::expect_false(me$review_complete[[1L]])
  ins<-utils::read.csv(x$insights,stringsAsFactors=FALSE)
  testthat::expect_equal(ins$insight_type,"enabler")
  inv<-utils::read.csv(x$inventory,stringsAsFactors=FALSE)
  testthat::expect_equal(inv$insights_coded,c(TRUE,FALSE))
  testthat::expect_equal(inv$abstract_no_case_insights_eligible,c(TRUE,TRUE))
  testthat::expect_identical(
    evidencelandscaper::prepare_ostara_first10_fulltext_review(
      mechanism,insights)$review_dir,x$review_dir)
})
