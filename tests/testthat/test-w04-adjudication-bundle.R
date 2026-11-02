testthat::test_that("Portable W04 adjudication ZIP is native-import compatible", {
  testthat::skip_if(Sys.which("zip")=="","ZIP executable unavailable")
  wd<-tempfile("w04-adjudication-")
  dir.create(wd)
  on.exit(unlink(wd,recursive=TRUE,force=TRUE),add=TRUE)
  w04<-file.path(wd,"selected-test")
  dir.create(file.path(w04,"consensus"),recursive=TRUE)
  consensus<-file.path(w04,"consensus")
  layer<-file.path(consensus,"workflow04_consensus_layer.jsonl")
  writeLines('{"record_id":"work-1","screening":{"decision":"uncertain"}}',layer)
  jsonlite::write_json(list(status="HUMAN_REVIEW_REQUIRED",
    final_unresolved=1,model="gpt-5.6-luna",prompt_sha256=paste0(rep("a",64),collapse="")),
    file.path(consensus,"summary.json"),auto_unbox=TRUE)
  queue<-file.path(w04,"human_review_queue.jsonl")
  case<-list(schema="local-w04-review-case-v1",review_case_id="w04-work-1",
    record_id="work-1",
    bibliographic=list(title="Example title",abstract="Example abstract",
                       authors="Person",year="2026",journal="Journal",
                       keywords="",doi=""),
    model=list(decision="uncertain",agreement="unresolved",
      screening_input_sha256=paste0(rep("b",64),collapse=""),
      prompt_sha256=paste0(rep("a",64),collapse="")),
    model_reasons=list(list(pass=1,decision="retain",reason="test")))
  writeLines(as.character(jsonlite::toJSON(case,auto_unbox=TRUE,null="null")),
             queue,useBytes=TRUE)
  testthat::local_mocked_bindings(
    prepare_w04_review=function(w04,include_all=FALSE) {
      testthat::expect_false(include_all)
      queue
    },.package="evidencelandscaper")
  dest<-file.path(wd,"adjudication.zip")
  testthat::expect_identical(export_w04_adjudication_bundle(w04,dest),dest)
  testthat::expect_true(file.exists(dest))
  testthat::expect_error(export_w04_adjudication_bundle(w04,dest),"exists")
  unpack<-file.path(wd,"unpacked");dir.create(unpack)
  utils::unzip(dest,exdir=unpack)
  bundle<-file.path(unpack,"w04_adjudication_bundle")
  testthat::expect_silent(parse(file=file.path(bundle,"app.R")))
  app<-paste(readLines(file.path(bundle,"app.R")),collapse="\n")
  testthat::expect_match(app,'local-w04-review-decision-v1',fixed=TRUE)
  testthat::expect_match(app,'record_input_sha256',fixed=TRUE)
  testthat::expect_match(app,'model_prompt_sha256',fixed=TRUE)
  testthat::expect_match(app,'human_review_queue.jsonl',fixed=TRUE)
  testthat::expect_false(grepl('commandArgs(trailingOnly=TRUE)',app,fixed=TRUE))
  sha<-function(x)digest::digest(file=x,algo="sha256",serialize=FALSE)
  testthat::expect_identical(
    sha(queue),sha(file.path(bundle,"data","human_review_queue.jsonl")))
  meta<-jsonlite::fromJSON(file.path(bundle,"manifest.json"))
  testthat::expect_equal(meta$unresolved_records,1)
  testthat::expect_identical(meta$decision_schema,"local-w04-review-decision-v1")
  # Native import verifies the review_case_id, queue bytes, and fingerprints.
  # The portable app must use the exact queue bytes to satisfy that contract.
  testthat::expect_false(file.exists(file.path(bundle,"decisions.jsonl")))
})
