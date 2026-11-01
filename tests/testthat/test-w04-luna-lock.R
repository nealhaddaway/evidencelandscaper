testthat::test_that("New W04 screening is Luna-locked and model-segregated", {
  f<-paste(deparse(evidencelandscaper::run_w04),collapse="\n")
  testthat::expect_match(f,'model = "gpt-5.6-luna"',fixed=TRUE)
  testthat::expect_match(f,'W04 model is locked to gpt-5.6-luna',fixed=TRUE)
  testthat::expect_match(f,'"-model-"',fixed=TRUE)
  script<-system.file("w04","workflow_04_luna_consensus.R",
                      package="evidencelandscaper")
  testthat::expect_true(nzchar(script))
  testthat::expect_silent(parse(file=script))
  txt<-paste(readLines(script,warn=FALSE),collapse="\n")
  testthat::expect_match(txt,'W04 API model is locked to gpt-5.6-luna',fixed=TRUE)
  testthat::expect_match(txt,'model_transition.json',fixed=TRUE)
  testthat::expect_match(txt,'pass_models=list(',fixed=TRUE)
  testthat::expect_match(paste(deparse(evidencelandscaper::continue_w04_with_luna),
                               collapse="\n"),
                         'pass1.jsonl',fixed=TRUE)
})

testthat::test_that("First pass must be complete to transition and no writes occur", {
  folder<-tempfile("w04-luna-missing-")
  dir.create(folder)
  on.exit(unlink(folder,recursive=TRUE),add=TRUE)
  testthat::expect_error(
    continue_w04_with_luna(
      source_w04=folder,
      scored=folder,
      prompt_file=folder,
      record_ids_csv=folder),
    "path|file|first-pass|checkpoint|prompt|missing"
  )
})
