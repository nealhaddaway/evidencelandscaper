testthat::test_that("Standalone insights validator preserves evidence integrity", {
  f<-evidencelandscaper:::.w09_validate_insights
  text<-"[SECTION 1] Farmers reported that shared monitoring reduced costs."
  row<-list(record_id="a",status="insights_identified",
    assessment_reason="Documented insight",
    insights=list(list(insight_type="enabler",
      summary="Shared monitoring reduced costs",
      context="Farmers",evidence_basis="stakeholder_report",
      source_quote="shared monitoring reduced costs",
      source_anchor="[SECTION 1]",reason="Reported from participants")))
  testthat::expect_silent(f(row,"a",text))
  bad<-row
  bad$insights[[1]]$source_quote<-"Invented quote"
  testthat::expect_error(f(bad,"a",text),"quotation")
  bad<-row
  bad$insights[[1]]$source_anchor<-"[PAGE 22]"
  testthat::expect_error(f(bad,"a",text),"anchor")
  bad<-row
  bad$insights[[1]]$case_ids<-list("invented")
  testthat::expect_error(f(bad,"a",text),"case or ontology IDs")
  empty<-list(record_id="a",status="no_relevant_insights",
    assessment_reason="No findings",insights=list())
  testthat::expect_silent(f(empty,"a",text))
  empty$status<-"insights_identified"
  testthat::expect_error(f(empty,"a",text),"status/count")
})

testthat::test_that("First-10% abstract-no-case insights route uses checksum-verified human overrides", {
  td<-tempfile("first10-insight-route-")
  dir.create(td,recursive=TRUE)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  jl<-function(x,p)writeLines(vapply(x,function(z)
    as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
    character(1)),p)
  json<-function(x,p)jsonlite::write_json(x,p,auto_unbox=TRUE,pretty=TRUE)
  root<-file.path(td,"runs","W09","first10-all")
  w04<-file.path(td,"runs","W01","block","W04_local","selected-test")
  w08<-file.path(td,"runs","W08","sample-test")
  dir.create(root,recursive=TRUE)
  dir.create(w08,recursive=TRUE)
  json(list(schema="w09-first10-all-included-v1",source_w04=w04,
    source_w04_layer_sha256="human-w04-hash",
    selected_record_ids=list("a","b","c")),
    file.path(root,"manifest.json"))
  writeLines("Prompt",file.path(w08,"prompt.txt"))
  writeLines("field,type",file.path(w08,"fields.csv"))
  rows<-list(
    list(record_id="a",case_evidence_status="unclear",cases=list()),
    list(record_id="b",case_evidence_status="no_identifiable_case",cases=list()),
    list(record_id="c",case_evidence_status="specific_case",
      cases=list(list(case_name="Project",case_description="Community programme"))))
  assessments<-file.path(w08,"case_assessments.jsonl")
  jl(rows,assessments)
  json(list(source_w04=w04,source_w04_sha256="human-w04-hash",
    prompt_sha256=sha(file.path(w08,"prompt.txt")),
    schema_sha256=sha(file.path(w08,"fields.csv")),
    selected_record_ids=list("a","b","c")),file.path(w08,"manifest.json"))
  json(list(status="PILOT_COMPLETE",responses_sha256=sha(assessments)),
    file.path(w08,"summary.json"))
  f<-evidencelandscaper:::.w09_first10_abstract_case_decisions
  base<-f(root,w08=w08)
  testthat::expect_equal(base$no_case_ids,"b")
  testthat::expect_equal(base$unresolved_ids,"a")
  human<-file.path(w08,"human_adjudication","v1-test")
  dir.create(human,recursive=TRUE)
  effective<-file.path(human,"effective_case_classifications.csv")
  utils::write.csv(data.frame(record_id=c("a","b","c"),
    model_case_status=c("unclear","no_identifiable_case","specific_case"),
    final_case_status=c("no_identifiable_case","no_identifiable_case",
                        "specific_case"),
    decision_origin=c("human_adjudicated","W08_model","W08_model")),
    effective,row.names=FALSE)
  json(list(source_w08_sha256=sha(assessments),
    effective_case_classifications_sha256=sha(effective)),
    file.path(human,"manifest.json"))
  final<-f(root,w08=w08)
  testthat::expect_equal(final$no_case_ids,c("a","b"))
  testthat::expect_length(final$unresolved_ids,0L)
  testthat::expect_true(nzchar(final$human_sha256))
  json(list(source_w08_sha256="bad",
    effective_case_classifications_sha256=sha(effective)),
    file.path(human,"manifest.json"))
  testthat::expect_error(f(root,w08=w08),"checksum mismatch")
})

testthat::test_that("First10 full-text orchestration is Luna only", {
  args<-formals(evidencelandscaper::run_ostara_first10_fulltext_inventory)
  testthat::expect_identical(as.character(args$model),"gpt-5.6-luna")
  testthat::expect_error(
    evidencelandscaper::run_ostara_first10_fulltext_inventory(
      model="gpt-5.6-sol"),"Only Luna models")
})
