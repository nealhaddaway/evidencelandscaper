testthat::test_that("W09 mechanisms are exported only for W10 eligible cases", {
  td<-tempfile("ostara-review-");dir.create(td)
  w09<-file.path(td,"w09");w10<-file.path(td,"w10")
  dir.create(w09);dir.create(w10)
  z<-list(record_id="work-1",
    abstract_mechanism_validation=list(
      list(mechanism_id="C05.04",status="confirmed",
           source_quote="Actual shared learning",source_anchor="[PAGE 4]")),
    ontology_gaps=list(
      list(description="New collaboration",source_quote="Mechanism evidence",
           source_anchor="[PAGE 5]")),
    cases=list(
      list(additional_mechanisms=list(list(mechanism_id="C01.03",
        source_quote="Added mechanism",source_anchor="[PAGE 6]"))),
      list(additional_mechanisms=list(list(mechanism_id="C06.04",
        source_quote="Not eligible",source_anchor="[PAGE 7]")))))
  writeLines(as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
             file.path(w09,"fulltext_case_registry_normalised.jsonl"))
  a<-list(
   list(observation_id="work-1-case-001",overall="eligible"),
   list(observation_id="work-1-case-002",overall="ineligible"))
  writeLines(vapply(a,function(v)as.character(jsonlite::toJSON(
    v,auto_unbox=TRUE)),character(1)),file.path(w10,"w10_decisions.jsonl"))
  paths<-prepare_ostara_mechanism_review(w09,w10,
    output_dir=file.path(td,"review"))
  me<-utils::read.csv(paths$mechanisms)
  no<-utils::read.csv(paths$novelty)
  testthat::expect_true(all(c("C05.04","C01.03")%in%me$mechanism_id))
  testthat::expect_false("C06.04"%in%me$mechanism_id)
  testthat::expect_equal(nrow(no),1L)
  testthat::expect_true(all(!me$review_complete))
  testthat::expect_error(prepare_ostara_mechanism_review(w09,w10,
    output_dir=file.path(td,"review")),"already exists")
})
