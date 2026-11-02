testthat::test_that("Partial W08 Shiny export explicitly excludes unvalidated records", {
  p<-testthat::test_path("..","..","R","w08-colleague-review.R")
  testthat::expect_silent(parse(file=p))
  formals<-formals(evidencelandscaper::export_w08_colleague_review)
  testthat::expect_identical(formals$allow_partial,FALSE)
  txt<-paste(readLines(p,warn=FALSE),collapse="\n")
  testthat::expect_match(txt,"PILOT_PARTIAL",fixed=TRUE)
  testthat::expect_match(txt,"setequal(missing,unresolved)",fixed=TRUE)
  testthat::expect_match(txt,"unresolved_publications.csv",fixed=TRUE)
  testthat::expect_match(txt,"No model assessment was fabricated.",fixed=TRUE)
  testthat::expect_match(txt,"summary$responses_sha256",fixed=TRUE)
  testthat::expect_match(txt,"source_w04_sha256",fixed=TRUE)
  testthat::expect_match(txt,"partial_run=partial",fixed=TRUE)
  testthat::expect_error(evidencelandscaper::export_w08_colleague_review(
    w08=tempdir(),destination_zip="x.zip",allow_partial=NA),
    "allow_partial must be TRUE or FALSE")
})
