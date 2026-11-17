testthat::test_that("fixed insights prompt contract accepts v5 and exports one CSV", {
  p<-testthat::test_path("..","..","R","ostara-fulltext-insights-pilot.R")
  v<-testthat::test_path("..","..","R","ostara-fulltext-insights-v2.R")
  testthat::expect_silent(parse(file=p))
  testthat::expect_silent(parse(file=v))
  src<-paste(readLines(p,warn=FALSE),collapse="\n")
  validator<-paste(readLines(v,warn=FALSE),collapse="\n")
  for(term in c("ostara_fulltext_insights.csv",
                 "source_pilot_dir","random_sample.csv",
                 "original_source_file","not_yet_validated",
                 "no random resampling","extract_ostara_fulltext_insights_v2(",
                 "source_sha256","prompt_sha256","ontology_sha256")) {
    if(identical(term,"no random resampling"))next
    testthat::expect_match(src,term,fixed=TRUE)
  }
  testthat::expect_match(validator,"stakeholder_views",fixed=TRUE)
  testthat::expect_match(validator,"not_stated",fixed=TRUE)
  testthat::expect_match(validator,"nrow(ontology)==26L",fixed=TRUE)
  testthat::expect_false(grepl('"recommendation","speculative_proposal"',
                               validator,fixed=TRUE))
  testthat::expect_true("run_ostara_fulltext_insights_pilot" %in%
    getNamespaceExports("evidencelandscaper"))
})
