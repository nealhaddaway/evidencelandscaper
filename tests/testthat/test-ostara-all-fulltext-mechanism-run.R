testthat::test_that("all-available mechanism runner uses one fulltext queue and one CSV", {
  f<-testthat::test_path("..","..","R","ostara-all-fulltext-mechanism-run.R")
  testthat::expect_silent(parse(file=f))
  src<-paste(readLines(f,warn=FALSE),collapse="\n")
  for(term in c(
    "run_ostara_all_available_fulltext_mechanisms <- function(",
    "file.symlink(f,dest)",
    "ostara_fulltext_mechanisms.csv",
    "fulltexts_missing.csv",
    "source_pool.csv",
    "extract_ostara_all_fulltext_mechanisms(",
    "max_records=max_records",
    "sha(prompt_file),sha(ontology_file)",
    "Prior pilot records remain eligible",
    "anyDuplicated(universe$record_id)",
    "source_sha256",
    "not_yet_validated",
    "source_fulltexts"
  ))testthat::expect_match(src,term,fixed=TRUE)
  testthat::expect_false(grepl("sample.int(",src,fixed=TRUE))
  testthat::expect_true("run_ostara_all_available_fulltext_mechanisms" %in%
    getNamespaceExports("evidencelandscaper"))
})
