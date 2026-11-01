testthat::test_that("W08 portable client review preserves completed 2-case and zero-case assessments", {
  testthat::skip_if(Sys.which("zip")=="","ZIP program not available")
  td<-tempfile("w08-client-test-")
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  json<-function(z,p)jsonlite::write_json(z,p,auto_unbox=TRUE,pretty=TRUE)
  jl<-function(z,p)writeLines(vapply(z,function(x)as.character(
    jsonlite::toJSON(x,auto_unbox=TRUE,null="null")),character(1)),
    p,useBytes=TRUE)
  root<-file.path(td,"runs","W01","prepare-test")
  w04<-file.path(root,"W04_local","selected-test")
  dir.create(file.path(w04,"finalised"),recursive=TRUE)
  dir.create(file.path(root,"W02_main","finalised"),recursive=TRUE)
  canonical<-file.path(root,"W02_main","finalised","canonical.jsonl")
  jl(list(
    list(identity=list(record_id="a"),canonical=list(
      title="Cocoa alliance study",abstract="Actors coordinated in two areas.",
      keywords="cocoa",year="2026")),
    list(identity=list(record_id="b"),canonical=list(
      title="Landscape review",abstract="No particular case identified.",
      year="2025"))),canonical)
  final<-file.path(w04,"finalised","workflow04_final_screening_layer.jsonl")
  jl(list(list(record_id="a",screening=list(decision="retain")),
          list(record_id="b",screening=list(decision="retain"))),final)
  json(list(status="final",final_layer_sha256=sha(final),
            retain=2L,reconciled_abstracts=FALSE),
       file.path(w04,"finalised","summary.json"))
  w08<-file.path(td,"runs","W08","sample-2-test")
  dir.create(w08,recursive=TRUE)
  writeLines("Case identification custom prompt",file.path(w08,"prompt.txt"))
  writeLines("field,type",file.path(w08,"fields.csv"))
  assessments<-list(
    list(record_id="a",case_evidence_status="unclear",
      fulltext_priority="medium",assessment_reason="Two ambiguous candidates",
      cases=list(
        list(case_name="Cocoa A",case_description="Local first group",
             case_location="Region A",supporting_evidence="Actors coordinated",
             confidence="low",collective_action_confidence="low",
             collective_action_basis="May be collective",case_kind="other_unclear",
             implementation_status="unclear"),
        list(case_name="Cocoa B",case_description="Local second group",
             case_location="Region B",supporting_evidence="two areas",
             confidence="low",collective_action_confidence="low",
             collective_action_basis="May be collective",case_kind="other_unclear",
             implementation_status="unclear"))),
    list(record_id="b",case_evidence_status="no_identifiable_case",
      fulltext_priority="low",assessment_reason="No described initiative",
      cases=list()))
  responses<-file.path(w08,"case_assessments.jsonl")
  jl(assessments,responses)
  json(list(status="prepared",source_w04=w04,
      source_w04_sha256=sha(final),canonical_w02_sha256=sha(canonical),
      prompt_sha256=sha(file.path(w08,"prompt.txt")),
      schema_sha256=sha(file.path(w08,"fields.csv")),
      selected_record_ids=list("a","b"),sample_size=2L),
      file.path(w08,"manifest.json"))
  json(list(status="PILOT_COMPLETE",responses_sha256=sha(responses)),
       file.path(w08,"summary.json"))
  output<-file.path(td,"review.zip")
  testthat::expect_identical(export_w08_colleague_review(w08,output),output)
  testthat::expect_true(file.exists(output))
  testthat::expect_error(export_w08_colleague_review(w08,output),"exists")
  unpack<-file.path(td,"unpack")
  dir.create(unpack)
  utils::unzip(output,exdir=unpack)
  bundle<-file.path(unpack,"w08_review_bundle")
  testthat::expect_silent(parse(file=file.path(bundle,"app.R")))
  manifest<-jsonlite::fromJSON(file.path(bundle,"manifest.json"),
                               simplifyVector=FALSE)
  testthat::expect_equal(manifest$candidate_cases,2)
  testthat::expect_identical(manifest$queue_sha256,
       sha(file.path(bundle,"data","review_queue.jsonl")))
  rows<-lapply(readLines(file.path(bundle,"data","review_queue.jsonl")),
               jsonlite::fromJSON,simplifyVector=FALSE)
  testthat::expect_length(rows,2L)
  testthat::expect_length(rows[[1]]$detection$cases,2L)
  testthat::expect_identical(rows[[1]]$bibliographic$abstract,
                              "Actors coordinated in two areas.")
  testthat::expect_length(rows[[2]]$detection$cases,0L)
  testthat::expect_false(file.exists(file.path(bundle,"decisions.jsonl")))
  # A partial W08 run is not a valid portable review population.
  json(list(status="PILOT_PARTIAL",responses_sha256=sha(responses)),
       file.path(w08,"summary.json"))
  testthat::expect_error(export_w08_colleague_review(
     w08,file.path(td,"partial.zip")),"incomplete")
})
