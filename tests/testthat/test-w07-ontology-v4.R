testthat::test_that("W07 accepts the client's 28-code native ontology", {
  codes<-paste0("M",rep(1:6,length.out=28),".1.",seq_len(28))
  new<-data.frame(
    mechanism_id=codes,
    collective_action_type=rep("Collective market shaping",28),
    high_level_mechanism=rep("Operational incentive",28),
    fine_level_mechanism=paste("Intervention",seq_len(28)),
    definition=paste("Definition of intervention",seq_len(28)),
    stringsAsFactors=FALSE)
  meta<-evidencelandscaper:::.w07_ontology_columns(new)
  testthat::expect_identical(meta$format,"hierarchical_v4")
  testthat::expect_identical(meta$codes,codes)
  testthat::expect_identical(meta$required,names(new))
  testthat::expect_error(
    evidencelandscaper:::.w07_ontology_columns(new[-1,]),
    "exactly 28")
  broken<-new
  broken$definition[[2]]<-"  "
  testthat::expect_error(
    evidencelandscaper:::.w07_ontology_columns(broken),
    "Missing ontology value")
  broken<-new
  broken$mechanism_id[[2]]<-broken$mechanism_id[[1]]
  testthat::expect_error(
    evidencelandscaper:::.w07_ontology_columns(broken),
    "repeated")
})

testthat::test_that("W07 legacy ontology contract remains supported", {
  old<-data.frame(
    category_id="C01",
    mechanism_id="C01.01",
    mechanism_source_label="Example",
    definition_draft="A definition",
    abstract_indicators_draft="Some indicator",
    not_sufficient_draft="Insufficient evidence",
    stringsAsFactors=FALSE)
  meta<-evidencelandscaper:::.w07_ontology_columns(old)
  testthat::expect_identical(meta$format,"legacy")
  testthat::expect_identical(meta$required,names(old))
  testthat::expect_identical(meta$codes,"C01.01")
  testthat::expect_error(
    evidencelandscaper:::.w07_ontology_columns(
      data.frame(mechanism_id="M1.1.1",definition="Yes")),
    "five hierarchical")
})

testthat::test_that("W07 engine still parses after v4 output checking", {
  testthat::expect_silent(parse(
    file=file.path("R","w07-trial.R")))
})
