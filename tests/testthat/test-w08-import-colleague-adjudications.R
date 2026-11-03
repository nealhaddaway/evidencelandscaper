testthat::test_that("W08 colleague review is imported with exact queue and immutable effective decisions", {
  testthat::skip_if(Sys.which("zip")=="","Portable ZIP is required")
  td<-tempfile("w08-human-import-")
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  jl<-function(rows,p)writeLines(vapply(rows,function(x)as.character(
    jsonlite::toJSON(x,auto_unbox=TRUE,null="null")),character(1)),p)
  json<-function(x,p)jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE)
  root<-file.path(td,"runs","W01","first10")
  w04<-file.path(root,"W04_local","selected-first10")
  snap<-file.path(w04,"human_authoritative","snapshots","snapshot01")
  dir.create(snap,recursive=TRUE)
  dir.create(file.path(w04,"consensus"),recursive=TRUE)
  dir.create(file.path(root,"W02_main","finalised"),recursive=TRUE)
  canonical<-file.path(root,"W02_main","finalised","canonical.jsonl")
  jl(lapply(c("a","b","c"),function(id)
    list(identity=list(record_id=id),
      canonical=list(title=paste("Research on",id),
                     doi=paste0("10.1234/",id),
                     abstract=paste("Coordination in",id),
                     keywords="collective action",year="2026"))),canonical)
  records<-lapply(c("a","b","c"),function(id)
    list(record_id=id,screening=list(decision="retain")))
  consensus<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  effective<-file.path(snap,"workflow04_effective_screening_layer.jsonl")
  jl(records,consensus);jl(records,effective)
  json(list(schema="w04-authoritative-snapshot-v1",
       source_consensus_sha256=sha(consensus),import_sha256=list("a"),
       status="final",run_type="selected",records=3L,
       retain=3L,exclude=0L,layer_sha256=sha(effective)),
       file.path(snap,"summary.json"))
  w08<-file.path(td,"runs","W08","sample-3-first10")
  dir.create(w08,recursive=TRUE)
  writeLines("Test W08 case prompt",file.path(w08,"prompt.txt"))
  writeLines("field,type",file.path(w08,"fields.csv"))
  assessment<-list(
    list(record_id="a",case_evidence_status="unclear",
      cases=list(list(case_name="A",case_description="One potential arrangement")),
      fulltext_priority="medium",assessment_reason="Unclear"),
    list(record_id="b",case_evidence_status="unclear",
      cases=list(),fulltext_priority="low",assessment_reason="Unclear"),
    list(record_id="c",case_evidence_status="no_identifiable_case",
      cases=list(),fulltext_priority="low",assessment_reason="None"))
  responses<-file.path(w08,"case_assessments.jsonl")
  jl(assessment,responses)
  json(list(source_w04=w04,source_w04_layer="human_authoritative",
    source_w04_sha256=sha(effective),canonical_w02_sha256=sha(canonical),
    prompt_sha256=sha(file.path(w08,"prompt.txt")),
    schema_sha256=sha(file.path(w08,"fields.csv")),
    selected_record_ids=list("a","b","c"),sample_size=3L),
    file.path(w08,"manifest.json"))
  json(list(status="PILOT_COMPLETE",responses_sha256=sha(responses)),
       file.path(w08,"summary.json"))
  zip<-file.path(td,"review.zip")
  evidencelandscaper::export_w08_colleague_review(w08,zip,
                                review_statuses="unclear")
  unpack<-file.path(td,"unpack")
  dir.create(unpack)
  utils::unzip(zip,exdir=unpack)
  queue<-file.path(unpack,"w08_review_bundle","data","review_queue.jsonl")
  h<-sha(queue)
  event<-function(id,status,cases)list(
     schema="ostara-w08-colleague-decision-v1",
     record_id=id,queue_sha256=h,reviewer="Diana",
     decision=if(status=="no_identifiable_case")"reject" else "accept",
     final_case_status=status,case_decisions=cases,
     alternative_cases="",notes="Reviewed",
     created_at_utc="2026-11-03T12:00:00Z")
  history<-file.path(td,"history.jsonl")
  jl(list(event("a","specific_case",list("")),
          event("b","no_identifiable_case",list())),history)
  imported<-evidencelandscaper::import_w08_colleague_adjudications(
    decisions_file=history,w08=w08,map_path=td,
    review_queue_file=queue)
  effective_csv<-file.path(imported,"effective_case_classifications.csv")
  testthat::expect_true(file.exists(effective_csv))
  data<-utils::read.csv(effective_csv,stringsAsFactors=FALSE)
  testthat::expect_equal(data$final_case_status,
       c("specific_case","no_identifiable_case","no_identifiable_case"))
  testthat::expect_equal(data$decision_origin,
       c("human_adjudicated","human_adjudicated","W08_model"))
  testthat::expect_identical(
    evidencelandscaper::import_w08_colleague_adjudications(
      decisions_file=history,w08=w08,map_path=td),imported)
  w09<-evidencelandscaper::prepare_w09_no_case_fulltexts_first10(
    w08=w08,map_path=td)
  q<-utils::read.csv(file.path(w09,"retrieval_queue.csv"),
                      stringsAsFactors=FALSE)
  testthat::expect_equal(q$record_id,c("b","c"))
  selection<-utils::read.csv(file.path(w09,
    "w08_abstract_no_case_selection.csv"),stringsAsFactors=FALSE)
  testthat::expect_equal(selection$decision_origin,
    c("human_adjudicated","W08_model"))
  # Tampering with a decision must not be silently accepted.
  altered<-file.path(td,"altered.jsonl")
  writeLines(c(readLines(history,warn=FALSE),
    as.character(jsonlite::toJSON(event("outside","unclear",list()),
                                 auto_unbox=TRUE,null="null"))),altered)
  testthat::expect_error(
    evidencelandscaper::import_w08_colleague_adjudications(
     decisions_file=altered,w08=w08,map_path=td,
     review_queue_file=queue),"not in the matching review queue")
  bad_queue<-file.path(td,"other_queue.jsonl")
  writeLines("{}",bad_queue)
  testthat::expect_error(
    evidencelandscaper::import_w08_colleague_adjudications(
     decisions_file=history,w08=w08,map_path=td,
     review_queue_file=bad_queue),"checksum does not match")
})
