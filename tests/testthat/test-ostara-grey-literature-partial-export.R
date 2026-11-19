testthat::test_that("partial grey report is offline and exports case and status files", {
  path<-testthat::test_path("..","..","R","ostara-grey-literature-partial-export.R")
  testthat::expect_silent(parse(file=path))
  s<-paste(readLines(path,warn=FALSE),collapse="\n")
  for(term in c(
    "export_ostara_grey_literature_partial <- function(",
    "grey_sources_manifest.json",
    "case_data.jsonl",
    "case_schema.csv",
    "grey_cases_extracted_so_far.csv",
    "grey_publication_status_so_far.csv",
    "grey_partial_extraction_report.html",
    "check_zero_case_classification",
    "readable_full_text",
    "not_yet_validated",
    "invalid_responses.jsonl",
    "writeBin(as.raw(c(239L,187L,191L)),con)",
    "source_sha256"
  ))testthat::expect_match(s,term,fixed=TRUE)
  testthat::expect_false(grepl("req_perform(",s,fixed=TRUE))
  testthat::expect_true("export_ostara_grey_literature_partial" %in%
                          getNamespaceExports("evidencelandscaper"))
})
