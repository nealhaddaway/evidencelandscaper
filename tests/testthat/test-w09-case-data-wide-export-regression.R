testthat::test_that("case-data wide exporter sets answer for every question", {
  p<-testthat::test_path("..","..","R","w09-case-data-wide-export.R")
  testthat::expect_silent(parse(file=p))
  s<-paste(readLines(p,warn=FALSE),collapse="\n")
  testthat::expect_match(s,'answer<-if(length(vals))',fixed=TRUE)
  testthat::expect_match(s,'row<-c(row,answer,paste(ev,collapse=',fixed=TRUE)
  testthat::expect_match(s,'is_factor_field<-grepl(',fixed=TRUE)
  testthat::expect_false(grepl('readable inside one CSV cell.\\\\n',s))
})
