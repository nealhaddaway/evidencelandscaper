testthat::test_that("W08 selected review supports partial human-authoritative unclear queue", {
  f <- evidencelandscaper::export_w08_colleague_review
  testthat::expect_true("review_statuses" %in% names(formals(f)))
  testthat::expect_error(
    f(w08=tempdir(), destination_zip=file.path(tempdir(),"invalid.zip"),
      allow_partial=TRUE, review_statuses="invalid"), "review_statuses"
  )
  p <- testthat::test_path("..","..","R","w08-colleague-review.R")
  txt <- paste(readLines(p,warn=FALSE),collapse="\n")
  testthat::expect_match(txt,".w04_latest_authoritative",fixed=TRUE)
  testthat::expect_match(txt,"review_assessments",fixed=TRUE)
  testthat::expect_match(txt,"unresolved_publications.csv",fixed=TRUE)
})
testthat::test_that("W08 portable Shiny saves adjudicated case status separately", {
  p<-system.file("w08","review_bundle_app.R",package="evidencelandscaper")
  testthat::expect_true(file.exists(p))
  txt<-paste(readLines(p,warn=FALSE),collapse="\n")
  testthat::expect_match(txt,"final_case_status",fixed=TRUE)
  testthat::expect_match(txt,"no_identifiable_case",fixed=TRUE)
})
