testthat::test_that("grey literature individual file selection is explicit", {
  p<-testthat::test_path("..","..","R",
                         "ostara-grey-literature-fulltexts.R")
  q<-testthat::test_path("..","..","R",
                         "ostara-grey-literature-selected-files.R")
  testthat::expect_silent(parse(file=p))
  testthat::expect_silent(parse(file=q))
  base<-paste(readLines(p,warn=FALSE),collapse="\n")
  wrapper<-paste(readLines(q,warn=FALSE),collapse="\n")
  for(term in c("input_files=NULL","if(!is.null(input_files))",
                 "input_paths<-vapply(input_files,norm,character(1))",
                 "if(anyDuplicated(files))",
                 "ordering<-order(files)"))
    testthat::expect_match(base,term,fixed=TRUE)
  testthat::expect_match(wrapper,"input_files=files",fixed=TRUE)
  testthat::expect_match(wrapper,"run_extraction=run_extraction",fixed=TRUE)
  testthat::expect_true(
    "run_ostara_grey_literature_selected_files" %in%
       getNamespaceExports("evidencelandscaper"))
})
