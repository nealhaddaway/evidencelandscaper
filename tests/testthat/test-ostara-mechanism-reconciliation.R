testthat::test_that("Ostara verified full-text assignments supersede without rewriting W07", {
  td <- tempfile("ostara-mechanism-test-")
  dir.create(td)
  ontology <- file.path(td,"ontology.csv")
  utils::write.csv(data.frame(category_id="C01",
                              mechanism_id=c("C01.01","C01.02"),
                              mechanism_source_label=c("One","Two")),
                   ontology,row.names=FALSE)
  w07 <- file.path(td,"coding.jsonl")
  jsonlite::write_json(list(record_id="work-1",mechanisms=list(
    list(mechanism_id="C01.01"))), w07,auto_unbox=TRUE)
  decisions <- file.path(td,"verified.csv")
  input <- data.frame(
    record_id=c("work-1","work-1"),
    mechanism_id=c("C01.01","C01.02"),
    human_decision=c("rejected","confirmed_case"),
    review_complete=c(TRUE,TRUE),
    eligible_case_verified=c(TRUE,TRUE),
    case_ids=c("","CASE-1"),
    evidence_quote=c("","Explicit participation"),
    source_anchor=c("","PAGE 2"),
    reviewer=c("reviewer","reviewer"),
    review_timestamp=c("2026-10-27","2026-10-27"))
  utils::write.csv(input,decisions,row.names=FALSE)
  before <- digest::digest(file=w07,algo="sha256")
  result <- reconcile_ostara_mechanisms(
    w07,decisions,ontology,file.path(td,"out"))
  effective <- utils::read.csv(result[["effective"]])
  testthat::expect_equal(effective$status[effective$mechanism_id=="C01.01"],
                         "fulltext_rejected")
  testthat::expect_equal(effective$status[effective$mechanism_id=="C01.02"],
                         "fulltext_added")
  testthat::expect_equal(digest::digest(file=w07,algo="sha256"),before)
  testthat::expect_error(reconcile_ostara_mechanisms(
    w07,decisions,ontology,file.path(td,"out")),"overwrite")
})
