testthat::test_that("grey zero-case audit is parseable, offline and counts only completed", {
  p<-testthat::test_path("..","..","R","ostara-grey-zero-case-filetype-audit.R")
  testthat::expect_silent(parse(file=p))
  src<-paste(readLines(p,warn=FALSE),collapse="\n")
  for(x in c(
    "audit_ostara_grey_zero_cases_by_filetype <- function(",
    "export_ostara_grey_literature_partial(",
    "original file type",
    'statuses$file_type<-tolower(as.character(statuses$source_format))',
    'completed<-statuses$extraction_status=="validated"',
    'zero_case_rate_pct=if(n_completed)',
    "validated_zero_cases",
    "unreadable",
    "not_yet_validated",
    "zero_case_sources_to_review.csv",
    "zero_case_filetype_audit_all_records.csv",
    "zero_case_rates_by_filetype.csv",
    "zero_case_filetype_audit.html",
    "zero_case_rate_by_filetype.png"
  ))testthat::expect_match(src,x,fixed=TRUE)
  testthat::expect_false(grepl("req_perform(",src,fixed=TRUE))
  testthat::expect_true(
    "audit_ostara_grey_zero_cases_by_filetype" %in%
      getNamespaceExports("evidencelandscaper"))
})
