testthat::test_that("W08 case-count rules allow multiple unclear candidates", {
  validate <- evidencelandscaper:::.w08_validate_case_count
  cases <- list(list(case_description="Case A"),
                list(case_description="Case B"))
  testthat::expect_silent(validate("unclear",list(),"record-1"))
  testthat::expect_silent(validate("unclear",cases[1],"record-1"))
  testthat::expect_silent(validate("unclear",cases,"record-1"))
  testthat::expect_silent(validate("unclear",c(cases,cases),"record-1"))
  testthat::expect_silent(validate("specific_case",cases[1],"record-1"))
  testthat::expect_silent(validate("multiple_cases",cases,"record-1"))
  testthat::expect_silent(validate("no_identifiable_case",list(),"record-1"))
  testthat::expect_error(validate("specific_case",cases,"record-1"),
                         "status/case-count")
  testthat::expect_error(validate("specific_case",list(),"record-1"),
                         "status/case-count")
  testthat::expect_error(validate("multiple_cases",cases[1],"record-1"),
                         "status/case-count")
  testthat::expect_error(validate("no_identifiable_case",cases[1],"record-1"),
                         "status/case-count")
  testthat::expect_error(validate("wrong_status",list(),"record-1"),
                         "invalid case status")
})

testthat::test_that("W08 default prompt and implementation reflect unclear candidates", {
  prompt <- system.file("w08","case_identification_prompt_v3.txt",
                        package="evidencelandscaper")
  testthat::expect_true(nzchar(prompt))
  lines<-paste(readLines(prompt,warn=FALSE),collapse="\n")
  testthat::expect_match(lines,"unclear: zero, one or multiple",fixed=TRUE)
  fn<-paste(deparse(evidencelandscaper::trial_w08_cases),collapse="\n")
  testthat::expect_match(fn,".w08_validate_case_count",fixed=TRUE)
  testthat::expect_match(fn,"case_identification_prompt_v3.txt",fixed=TRUE)
  testthat::expect_match(fn,"unclear any number",fixed=TRUE)
})
