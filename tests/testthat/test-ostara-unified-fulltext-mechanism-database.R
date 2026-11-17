testthat::test_that("unified mechanism pilot keeps one output database", {
  core<-testthat::test_path("..","..","R","ostara-fulltext-mechanism-database.R")
  runner<-testthat::test_path("..","..","R","ostara-random-fulltext-mechanism-pilot.R")
  testthat::expect_silent(parse(file=core))
  testthat::expect_silent(parse(file=runner))
  a<-paste(readLines(core,warn=FALSE),collapse="\n")
  b<-paste(readLines(runner,warn=FALSE),collapse="\n")
  for(term in c("mechanism_database.jsonl","mechanism_inventory",
                "new_novelty","case_presence","candidate_cases",
                "publication_status","prompt_sha256","ontology_sha256",
                "Duplicate record: ","source_ledgers")) {
    testthat::expect_match(a,term,fixed=TRUE)
  }
  testthat::expect_match(b,"consolidate_ostara_fulltext_mechanism_pilot(",fixed=TRUE)
  testthat::expect_false(grepl("httr2::",a,fixed=TRUE))
  testthat::expect_true("consolidate_ostara_fulltext_mechanism_pilot" %in%
                        getNamespaceExports("evidencelandscaper"))
})
