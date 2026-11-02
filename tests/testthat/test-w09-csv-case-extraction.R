testthat::test_that("W09 CSV-driven case extraction has separate mechanism-free contract", {
  p<-testthat::test_path("..","..","R","w09-csv-case-extraction.R")
  testthat::expect_silent(parse(file=p))
  f<-evidencelandscaper::run_w09_case_data_extraction
  testthat::expect_identical(names(formals(f)),
    c("w09","prompt_file","schema_file","record_ids","max_records","model","max_pages"))
  testthat::expect_identical(eval(formals(f)$model),"gpt-5.6-luna")
  src<-paste(readLines(p,warn=FALSE),collapse="\n")
  for(x in c("case_schema.csv","field_mapping.csv","case_data.jsonl",
      "source_documents.jsonl","invalid_responses.jsonl",
      "Omitting ","mechanism ontology row","w09_text_payload",
      "check_result(x,id,payload$text)","response_id=response$id",
      "max_records=1L","No cases: cases=[]")) {
    testthat::expect_match(src,x,fixed=TRUE)
  }
  testthat::expect_false(grepl("ontology.csv",src,fixed=TRUE))
  testthat::expect_false(grepl("w07_run",src,fixed=TRUE))
  testthat::expect_error(f(w09=tempdir(),prompt_file="missing.txt",
                          schema_file="missing.csv",record_ids="x",
                          model="gpt-5.6"),"gpt-5\\.6-luna only")
})
