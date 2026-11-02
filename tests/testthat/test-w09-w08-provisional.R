testthat::test_that("W09 W08 case-positive handover parses and requires immutable sources", {
  p<-testthat::test_path("..","..","R","w09-w08-provisional.R")
  testthat::expect_silent(parse(file=p))
  fn<-evidencelandscaper::prepare_w09_w08_case_fulltexts
  testthat::expect_identical(names(formals(fn)),c("w08","include_unclear"))
  s<-paste(readLines(p,warn=FALSE),collapse="\n")
  testthat::expect_match(s,"W08 status or checksum mismatch.",fixed=TRUE)
  testthat::expect_match(s,"No case-positive W08 publications.",fixed=TRUE)
  testthat::expect_match(s,"unresolved_w08_records.csv",fixed=TRUE)
  testthat::expect_match(s,"length(z$cases)>0L",fixed=TRUE)
  testthat::expect_match(s,"source_w04_layer_sha256",fixed=TRUE)
  testthat::expect_match(s,"canonical_w02_sha256",fixed=TRUE)
  testthat::expect_match(s,"w08_case_candidates.csv",fixed=TRUE)
  testthat::expect_error(fn(w08=tempdir(),include_unclear=NA),
                         "W08 directory must be under")
})

testthat::test_that("case candidate selection respects unclear evidence flag", {
  assessments<-list(
    list(record_id="a",case_evidence_status="specific_case",
         cases=list(list(case_name="A"))),
    list(record_id="b",case_evidence_status="no_identifiable_case",cases=list()),
    list(record_id="c",case_evidence_status="unclear",
         cases=list(list(case_name="Possible C"))))
  selected<-Filter(function(z)length(z$cases)>0L,assessments)
  testthat::expect_identical(vapply(selected,`[[`,character(1),"record_id"),c("a","c"))
  excluding_unclear<-Filter(function(z)length(z$cases)>0L&&
     !identical(z$case_evidence_status,"unclear"),assessments)
  testthat::expect_identical(vapply(excluding_unclear,`[[`,character(1),"record_id"),"a")
})
