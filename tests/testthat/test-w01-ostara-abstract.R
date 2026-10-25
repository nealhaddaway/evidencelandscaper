test_that("Ostara exact abstract hash promotes only nonblank matches", {
  td<-tempfile("ostara-abs-");dir.create(td)
  on.exit(unlink(td,recursive=TRUE),add=TRUE)
  scored<-file.path(td,"rescored_sample.csv")
  meta<-file.path(td,"normalised_metadata.csv")
  data.table::fwrite(data.frame(idx=1:5,
    abstract_hash=c("hashA","hashA",NA,"hashB","hashB")),meta)
  data.table::fwrite(data.frame(record_i=c(1,1,3,4),record_j=c(2,3,4,5),
    rescored_classification=c("review","review","unresolved","unresolved"),
    rescored_rule=c("base","base","base","base"),
    review_route=c("manual_review","manual_review","non_duplicate","non_duplicate"),
    manual_review_needed=c(TRUE,TRUE,FALSE,FALSE),
    decision_changed=FALSE),scored)
  evidencelandscaper:::.apply_ostara_exact_abstract_policy(scored,meta)
  a<-data.table::fread(scored)
  expect_equal(as.character(a$rescored_classification),
    c("duplicate","review","unresolved","duplicate"))
  expect_equal(sum(a$rescored_rule=="ostara_identical_nonempty_abstract"),2L)
  expect_true(file.exists(file.path(td,"rescored_sample.before_ostara_abstract_policy.csv")))
  expect_message(evidencelandscaper:::.apply_ostara_exact_abstract_policy(scored,meta),
    "Reusing verified")
})
