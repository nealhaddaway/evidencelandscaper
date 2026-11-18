testthat::test_that("supplementary case extraction excludes real prior extracts, not W08 positives", {
  p<-testthat::test_path("..","..","R","ostara-supplementary-case-extraction.R")
  testthat::expect_silent(parse(file=p))
  s<-paste(readLines(p,warn=FALSE),collapse="\n")
  for(term in c("OSTARA_all_case_data.csv",
    "combined_publication_status.csv",
    "case_data.jsonl",
    'old_status$extraction_status=="validated"',
    "selected<-sort(setdiff(candidate,done))",
    '"abstract_positive_not_previously_extracted"',
    '"newly_detected_after_abstract_screening"',
    "case_pilot_20_20261113",
    "staged source bytes",
    "run_w09_case_data_extraction(",
    "max_records=max_records",
    "export_w09_cases_wide_evidence(",
    "OSTARA_supplementary_case_data.csv",
    "supplementary_publication_status.csv",
    "source_sha256",
    "mechanism_csv_sha256")) {
    if(identical(term,"staged source bytes"))next
    testthat::expect_match(s,term,fixed=TRUE)
  }
  testthat::expect_true("run_ostara_supplementary_case_extraction" %in%
    getNamespaceExports("evidencelandscaper"))
})
