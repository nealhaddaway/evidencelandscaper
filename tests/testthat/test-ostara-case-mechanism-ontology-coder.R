testthat::test_that("v5 case mechanism coding accepts exact fields and exports offline", {
  p<-testthat::test_path("..","..","R","ostara-case-mechanism-ontology-coder.R")
  testthat::expect_silent(parse(file=p))
  d<-tempfile("ostara-mechanisms-")
  dir.create(d)
  on.exit(unlink(d,recursive=TRUE),add=TRUE)
  ontology<-data.frame(mechanism_id=paste0("M",seq_len(26)),
    category=rep("Category",26),`function`=rep("Function",26),
    mechanism=paste0("Mechanism ",seq_len(26)),
    definition=rep("A documented collective-action mechanism.",26),
    stringsAsFactors=FALSE)
  oc<-file.path(d,"ontology.csv")
  utils::write.csv(ontology,oc,row.names=FALSE)
  cases<-data.frame(record_id=c("work-01","work-02"),
    case_id=c("work-01::case_001","work-02::case_001"),
    "13. What are the mechanisms and activities through which the case operationalises collective action?"=
      c("joint monitoring","buyers share price commitments"),
    "13. What are the mechanisms and activities through which the case operationalises collective action? [evidence]"=
      c("[PAGE 1]: joint monitoring","[PAGE 2]: buyers share price commitments"),
    check.names=FALSE,stringsAsFactors=FALSE)
  cc<-file.path(d,"cases.csv")
  utils::write.csv(cases,cc,row.names=FALSE)
  output<-file.path(d,"output")
  result<-code_ostara_case_mechanisms_v5(cases_csv=cc,
     ontology_csv=oc,output_dir=output,run_extraction=FALSE)
  testthat::expect_equal(result$total,2L)
  testthat::expect_equal(result$completed,0L)
  testthat::expect_true(file.exists(result$wide_csv))
  testthat::expect_true(file.exists(result$long_csv))
  exported<-utils::read.csv(result$wide_csv,check.names=FALSE,
     fileEncoding="UTF-8-BOM",stringsAsFactors=FALSE)
  testthat::expect_identical(as.character(exported$case_id),cases$case_id)
  testthat::expect_true(all(exported$coding_status=="not_yet_processed"))
  repeat_run<-code_ostara_case_mechanisms_v5(cc,oc,output,
    run_extraction=FALSE)
  testthat::expect_equal(repeat_run$total,result$total)
  testthat::expect_error(
    code_ostara_case_mechanisms_v5(cc,oc,output,batch_size=16L,
      run_extraction=FALSE),"provenance changed")
})
