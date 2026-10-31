testthat::test_that("eligibility-first W09 cases feed mechanism extraction without W10", {
  td<-tempfile("ef-mechanisms-")
  map<-file.path(td,"map")
  dir.create(map,recursive=TRUE)
  writeLines("name: map",file.path(map,"map.yml"))
  w09<-file.path(map,"runs","W09","run")
  w07<-file.path(map,"runs","W07","full-170-example")
  gate<-file.path(w09,"eligibility")
  extraction<-file.path(w09,"v2","approved")
  for(p in c(w09,w07,gate,extraction,
             file.path(map,"runs","orchestrated","search_a")))
    dir.create(p,recursive=TRUE,showWarnings=FALSE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  writeLines("record_id\nwork-1",file.path(w09,"retrieval_queue.csv"))
  ontology<-file.path(w07,"ontology.csv")
  utils::write.csv(data.frame(mechanism_id=c("C01.01","C01.02"),
                             label=c("First","Second")),ontology,row.names=FALSE)
  coding<-file.path(w07,"coding.jsonl")
  cat(as.character(jsonlite::toJSON(
    list(record_id="work-1",ontology_gap=FALSE,
         mechanisms=list(list(mechanism_id="C01.01"))),
    auto_unbox=TRUE)),"\n",file=coding)
  ledger<-file.path(gate,"candidate_case_decisions.jsonl")
  writeLines('{"record_id":"work-1","cases":[{"case_id":"approved-001"}]}',
             ledger)
  final<-file.path(gate,"final_case_eligibility.csv")
  utils::write.csv(data.frame(record_id="work-1",case_id="approved-001",
                              decision="eligible"),final,row.names=FALSE)
  jsonlite::write_json(list(w09=w09,ledger_sha256=sha(ledger)),
                       file.path(gate,"manifest.json"),auto_unbox=TRUE)
  jsonlite::write_json(list(original_ledger_sha256=sha(ledger),
                            final_audit_sha256=sha(final)),
                       file.path(gate,"final_review_manifest.json"),auto_unbox=TRUE)
  registry<-file.path(extraction,"fulltext_case_registry_normalised.jsonl")
  cat(as.character(jsonlite::toJSON(list(record_id="work-1",
    cases=list(list(eligibility_case_id="approved-001",
                    case_name="An approved case",
                    case_description="Documented activity")),
    ontology_gaps=list()),auto_unbox=TRUE)),"\n",file=registry)
  jsonlite::write_json(list(eligibility_ledger_sha256=sha(final),
     w07_run=w07,ontology_sha256=sha(ontology),coding_sha256=sha(coding)),
     file.path(extraction,"manifest.json"),auto_unbox=TRUE)
  state<-list(status="extracted",preextraction=list(run=gate,
     ledger_sha256=sha(ledger),final_sha256=sha(final)),
     w09=list(run=w09,extraction_dir=extraction),w07=list(run=w07))
  statefile<-file.path(map,"runs","orchestrated","search_a","state.json")
  jsonlite::write_json(state,statefile,auto_unbox=TRUE)
  review<-prepare_ostara_mechanism_review_from_run("search_a",map)
  testthat::expect_true(file.exists(file.path(review,"manifest.json")))
  testthat::expect_identical(
    prepare_ostara_mechanism_review_from_run("search_a",map),review)
  baseline<-utils::read.csv(file.path(review,"mechanism_review.csv"))
  testthat::expect_equal(baseline$mechanism_id,"C01.01")
  testthat::expect_false(baseline$review_complete[[1L]])
  prior_key<-Sys.getenv("OPENAI_API_KEY",unset=NA_character_)
  Sys.unsetenv("OPENAI_API_KEY")
  on.exit(if(is.na(prior_key)) Sys.unsetenv("OPENAI_API_KEY")
          else Sys.setenv(OPENAI_API_KEY=prior_key),add=TRUE)
  testthat::expect_error(
    extract_ostara_fulltext_mechanisms(review,max_records=0L),
    "OPENAI_API_KEY required")
  prompt<-system.file("w09","fulltext_mechanisms_supplement_v1.txt",
                       package="evidencelandscaper")
  output<-file.path(review,"fulltext_mechanism_supplement",
    paste0("v1-",substr(sha(prompt),1,12),"-",substr(sha(ontology),1,12),
           "-gpt-6-luna"))
  testthat::expect_true(file.exists(file.path(output,"manifest.json")))
  # Simulate an already checkpointed complete extraction: zero network calls.
  mock<-list(record_id="work-1",eligible_case_ids=list("approved-001"),
   mechanism_inventory=list(
      list(mechanism_id="C01.01",status="not_identified",case_ids=list()),
      list(mechanism_id="C01.02",status="implemented_case",
           case_ids=list("approved-001"),source_quote="Documented mechanism",
           source_anchor="[PAGE 2]")),
   abstract_novelty_validation=list(),new_novelty=list())
  cat(as.character(jsonlite::toJSON(mock,auto_unbox=TRUE,null="null")),
      "\n",file=file.path(output,"mechanism_assessments.jsonl"))
  testthat::expect_identical(
    extract_ostara_fulltext_mechanisms(review,max_records=0L),output)
  worksheets<-prepare_ostara_supplement_review(review,output)
  observed<-utils::read.csv(worksheets$mechanisms)
  testthat::expect_equal(nrow(observed),2L)
  testthat::expect_equal(observed$case_ids[[2L]],"approved-001")
  testthat::expect_false(any(observed$review_complete))
  # Changing approved decisions must make a new review impossible.
  utils::write.csv(data.frame(record_id="work-1",case_id="approved-001",
                              decision="ineligible"),final,row.names=FALSE)
  testthat::expect_error(
    prepare_ostara_mechanism_review_from_run("search_a",map),
    "provenance mismatch|changed")
})
