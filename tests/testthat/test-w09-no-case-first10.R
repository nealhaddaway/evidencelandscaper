testthat::test_that("first 10% W08 no-case queue excludes unclear and unvalidated", {
  td<-tempfile("w09-first10-no-case-")
  dir.create(td,recursive=TRUE)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  sha<-function(f)digest::digest(file=f,algo="sha256",serialize=FALSE)
  json<-function(x,f)jsonlite::write_json(x,f,auto_unbox=TRUE,pretty=TRUE)
  jl<-function(rows,f)writeLines(vapply(rows,function(x)
    as.character(jsonlite::toJSON(x,auto_unbox=TRUE,null="null")),
    character(1)),f)
  w01<-file.path(td,"runs","W01","first10-test")
  w04<-file.path(w01,"W04_local","selected-example")
  snap<-file.path(w04,"human_authoritative","snapshots","one")
  dir.create(snap,recursive=TRUE)
  dir.create(file.path(w04,"consensus"),recursive=TRUE)
  dir.create(file.path(w01,"W02_main","finalised"),recursive=TRUE)
  canonical<-file.path(w01,"W02_main","finalised","canonical.jsonl")
  jl(lapply(c("a","b","c"),function(id)list(identity=list(record_id=id),
    canonical=list(title=paste("Title",id),doi=paste0("10.1234/",id),
                   abstract=paste("Abstract",id)))),canonical)
  rows<-lapply(c("a","b","c"),function(id)
    list(record_id=id,screening=list(decision="retain")))
  consensus<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  effective<-file.path(snap,"workflow04_effective_screening_layer.jsonl")
  jl(rows,consensus);jl(rows,effective)
  json(list(schema="w04-authoritative-snapshot-v1",
     source_consensus_sha256=sha(consensus),
     import_sha256=list("example"),status="final",records=3L,retain=3L,
     exclude=0L,run_type="selected",layer_sha256=sha(effective)),
     file.path(snap,"summary.json"))
  w08<-file.path(td,"runs","W08","sample-3-test")
  dir.create(w08,recursive=TRUE)
  writeLines("Case prompt",file.path(w08,"prompt.txt"))
  writeLines("field,type",file.path(w08,"fields.csv"))
  responses<-file.path(w08,"case_assessments.jsonl")
  jl(list(
    list(record_id="a",case_evidence_status="no_identifiable_case",
      cases=list(),assessment_reason="No identified case"),
    list(record_id="b",case_evidence_status="unclear",
      cases=list(),assessment_reason="Ambiguous evidence")),responses)
  json(list(source_w04=w04,source_w04_layer="human_authoritative",
    source_w04_sha256=sha(effective),canonical_w02_sha256=sha(canonical),
    prompt_sha256=sha(file.path(w08,"prompt.txt")),
    schema_sha256=sha(file.path(w08,"fields.csv")),
    selected_record_ids=list("a","b","c"),sample_size=3L),
    file.path(w08,"manifest.json"))
  json(list(status="PILOT_PARTIAL",responses_sha256=sha(responses),
    validated_records=2L,unresolved_record_ids=list("c")),
    file.path(w08,"summary.json"))
  target<-evidencelandscaper::prepare_w09_no_case_fulltexts_first10(
    w08=w08,map_path=td)
  q<-utils::read.csv(file.path(target,"retrieval_queue.csv"),
    stringsAsFactors=FALSE)
  testthat::expect_equal(as.character(q$record_id),"a")
  testthat::expect_true(file.exists(file.path(target,"manifest.json")))
  testthat::expect_true(dir.exists(file.path(target,"pdfs")))
  excluded<-utils::read.csv(file.path(target,"not_selected_unresolved.csv"),
                             stringsAsFactors=FALSE)
  testthat::expect_equal(excluded$record_id,c("b","c"))
  testthat::expect_equal(excluded$status,c("unclear","unvalidated"))
  testthat::expect_identical(
    evidencelandscaper::prepare_w09_no_case_fulltexts_first10(
      w08=w08,map_path=td),target)
  previous<-Sys.getenv("OPENALEX_API_KEY",unset=NA_character_)
  Sys.unsetenv("OPENALEX_API_KEY")
  on.exit(if(is.na(previous))Sys.unsetenv("OPENALEX_API_KEY")
          else Sys.setenv(OPENALEX_API_KEY=previous),add=TRUE)
  testthat::expect_error(
    evidencelandscaper::retrieve_w09_no_case_fulltexts_first10(
      w08=w08,map_path=td),"OPENALEX_API_KEY")
  testthat::expect_true(file.exists(file.path(target,"retrieval_queue.csv")))
  # Auditable fail-closed provenance: no silent expansion to unclear records.
  json(list(status="PILOT_PARTIAL",responses_sha256="invalid",
    validated_records=2L,unresolved_record_ids=list("c")),
    file.path(w08,"summary.json"))
  testthat::expect_error(
    evidencelandscaper::prepare_w09_no_case_fulltexts_first10(
      w08=w08,map_path=td),"checksum mismatch")
})
