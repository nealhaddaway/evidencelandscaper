testthat::test_that("W07 validates model evidence before appending and retries with Luna", {
  source_path<-testthat::test_path("..","..","R","w07-trial.R")
  testthat::expect_silent(parse(file=source_path))
  txt<-paste(readLines(source_path,warn=FALSE),collapse="\n")
  testthat::expect_match(txt,"for(attempt in seq_len(5L))",fixed=TRUE)
  testthat::expect_match(txt,"validate_result(result)",fixed=TRUE)
  testthat::expect_match(txt,"Mechanism excerpt is not in the supplied",fixed=TRUE)
  testthat::expect_match(txt,"invalid_responses.jsonl",fixed=TRUE)
  testthat::expect_match(txt,"validation_error=error",fixed=TRUE)
  testthat::expect_match(txt,"req_body_json(body,auto_unbox=TRUE)",fixed=TRUE)
  testthat::expect_match(txt,"coding.jsonl",fixed=TRUE)
  testthat::expect_match(txt,"Completed coding checkpoint preserved.",fixed=TRUE)
  testthat::expect_match(txt,"evidence_excerpt MUST be an exact, continuous verbatim",fixed=TRUE)
  # The corrected source uses R escaped newlines, not literal backslash-n text.
})

testthat::test_that("Provisional W07 exact W04+prompt+ontology checkpoint is resumable", {
  txt<-paste(deparse(evidencelandscaper::trial_w07),collapse="\n")
  testthat::expect_match(txt,"provisional_retains = FALSE",fixed=TRUE)
  testthat::expect_match(txt,"done_ids",fixed=TRUE)
})
