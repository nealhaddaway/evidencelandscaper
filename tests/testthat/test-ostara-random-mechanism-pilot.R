testthat::test_that("random full-text pilot and v5 ontology are version isolated", {
  p<-testthat::test_path("..","..","R","ostara-random-fulltext-mechanism-pilot.R")
  f<-testthat::test_path("..","..","R","ostara-fulltext-mechanisms-all.R")
  testthat::expect_silent(parse(file=p))
  testthat::expect_silent(parse(file=f))
  s<-paste(readLines(p,warn=FALSE),collapse="\n")
  core<-paste(readLines(f,warn=FALSE),collapse="\n")
  testthat::expect_match(s,"sample(eligible,size=sample_size,replace=FALSE)",fixed=TRUE)
  testthat::expect_match(s,"sha(prompt_file),sha(ontology_file)",fixed=TRUE)
  testthat::expect_match(s,"Prior inventories not excluded",fixed=TRUE)
  testthat::expect_match(s,".ostara_random_pilot_across_w09(",fixed=TRUE)
  testthat::expect_match(s,"random_fulltext_mechanism_pilots",fixed=TRUE)
  testthat::expect_match(s,"first10|remaining90",fixed=TRUE)
  testthat::expect_match(s,"sample.int(nrow(pool),sample_size,replace=FALSE)",fixed=TRUE)
  testthat::expect_match(s,"Conflicting full texts for duplicate W09 record ID",fixed=TRUE)
  testthat::expect_match(s,"extract_ostara_all_fulltext_mechanisms(",fixed=TRUE)
  testthat::expect_false(grepl("previously_inventoried",s,fixed=TRUE))
  testthat::expect_match(core,'client_v5_columns<-c("mechanism_id","category","function","mechanism","definition")',fixed=TRUE)
  testthat::expect_match(core,"nrow(ont)==26L",fixed=TRUE)
  testthat::expect_match(core,"nrow(ont)==30L",fixed=TRUE)
  testthat::expect_true("run_ostara_random_fulltext_mechanism_pilot" %in%
                          getNamespaceExports("evidencelandscaper"))
})

testthat::test_that("same-record mechanism replay does not resample and preserves prior version", {
  p<-testthat::test_path("..","..","R","ostara-random-fulltext-mechanism-pilot.R")
  testthat::expect_silent(parse(file=p))
  src<-paste(readLines(p,warn=FALSE),collapse="\n")
  for(term in c(
    "rerun_ostara_mechanism_pilot_same_records <- function(",
    "fixed_sample=sample",
    "source_pilot_dir",
    "no random resampling",
    "Source bytes differ from prior validated pilot",
    "sha(prompt_file),sha(ontology_file)",
    "ostara_fulltext_mechanisms.csv"
  )) {
    if(identical(term,"ostara_fulltext_mechanisms.csv"))next
    testthat::expect_match(src,term,fixed=TRUE)
  }
  testthat::expect_true("rerun_ostara_mechanism_pilot_same_records" %in%
    getNamespaceExports("evidencelandscaper"))
})
