testthat::test_that("All first-10-percent W04 retains enter an auditable W09 queue", {
  td<-tempfile("first10-all-")
  dir.create(td)
  on.exit(unlink(td,recursive=TRUE,force=TRUE),add=TRUE)
  sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
  jl<-function(x,p)writeLines(vapply(x,function(z)
    as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
    character(1)),p)
  json<-function(x,p)jsonlite::write_json(x,p,pretty=TRUE,auto_unbox=TRUE)
  root<-file.path(td,"runs","W01","selected-block")
  w04<-file.path(root,"W04_local","selected-10-percent")
  snap<-file.path(w04,"human_authoritative","snapshots","one")
  dir.create(snap,recursive=TRUE)
  dir.create(file.path(w04,"consensus"),recursive=TRUE)
  dir.create(file.path(root,"W02_main","finalised"),recursive=TRUE)
  records<-lapply(c("a","b","c","d"),function(id)
    list(record_id=id,screening=list(
      decision=if(id=="d")"exclude" else "retain")))
  consensus<-file.path(w04,"consensus","workflow04_consensus_layer.jsonl")
  final<-file.path(snap,"workflow04_effective_screening_layer.jsonl")
  jl(records,consensus);jl(records,final)
  json(list(schema="w04-authoritative-snapshot-v1",
      source_consensus_sha256=sha(consensus),import_sha256=list("import1"),
      status="final",run_type="selected",records=4L,retain=3L,
      exclude=1L,layer_sha256=sha(final)),
    file.path(snap,"summary.json"))
  canonical<-file.path(root,"W02_main","finalised","canonical.jsonl")
  jl(lapply(c("a","b","c","d"),function(id)list(
    identity=list(record_id=id),canonical=list(
      title=paste0("Publication ",id),doi=paste0("10.1234/",id)))),
    canonical)
  old<-file.path(td,"runs","W09","case-positive-existing")
  dir.create(file.path(old,"pdfs"),recursive=TRUE)
  utils::write.csv(data.frame(
      record_id="a",title="Publication a",doi="10.1234/a"),
    file.path(old,"retrieval_queue.csv"),row.names=FALSE)
  # The offline retrieval test only validates the PDF signature/size.
  writeBin(c(charToRaw("%PDF-1.4\n"),
             as.raw(rep(32L,120L))),
    file.path(old,"pdfs","a.pdf"))
  qdir<-evidencelandscaper::prepare_w09_first10_all_included(
    map_path=td,w04_run=w04)
  q<-utils::read.csv(file.path(qdir,"retrieval_queue.csv"),
    stringsAsFactors=FALSE)
  testthat::expect_equal(q$record_id,c("a","b","c"))
  testthat::expect_false("d"%in%q$record_id)
  mf<-jsonlite::fromJSON(file.path(qdir,"manifest.json"),
                          simplifyVector=FALSE)
  testthat::expect_equal(mf$selected_count,3L)
  testthat::expect_identical(mf$case_status_filter,"none")
  testthat::expect_identical(
    evidencelandscaper::prepare_w09_first10_all_included(
      map_path=td,w04_run=w04),qdir)
  # Reuse existing case-positive file without ANY internet access. One
  # additional missing ID is deliberately retained in the cohort.
  # With no key, the bulk downloader refuses network access rather than
  # claiming that the 2 outstanding records are downloaded.
  oldkey<-Sys.getenv("OPENALEX_API_KEY",unset=NA_character_)
  Sys.unsetenv("OPENALEX_API_KEY")
  on.exit(if(is.na(oldkey))Sys.unsetenv("OPENALEX_API_KEY")
          else Sys.setenv(OPENALEX_API_KEY=oldkey),add=TRUE)
  testthat::expect_error(
    evidencelandscaper::retrieve_w09_first10_all_included(
      map_path=td,w04_run=w04),
    "OPENALEX_API_KEY")
  # Exercise the actual downloader without any network: all queued records
  # supplied in prior W09 runs with strictly matching ID/title/DOI.
  for(id in c("b","c")) {
    dir.create(file.path(old,"structured"),showWarnings=FALSE)
    writeBin(c(charToRaw("%PDF-1.4\n"),
               as.raw(rep(32L,120L))),
      file.path(old,"pdfs",paste0(id,".pdf")))
  }
  utils::write.csv(data.frame(
    record_id=c("a","b","c"),
    title=paste0("Publication ",c("a","b","c")),
    doi=paste0("10.1234/",c("a","b","c"))),
    file.path(old,"retrieval_queue.csv"),row.names=FALSE)
  Sys.setenv(OPENALEX_API_KEY="dummy-key-unused")
  report<-evidencelandscaper::retrieve_w09_first10_all_included(
    map_path=td,w04_run=w04)
  t<-utils::read.csv(report,stringsAsFactors=FALSE)
  testthat::expect_equal(nrow(t),3L)
  testthat::expect_true(all(t$source=="existing_matching_W09_archive"))
  testthat::expect_true(all(t$openalex_status==
    "existing_file_reused_without_network"))
  testthat::expect_true(all(file.exists(t$file)))
  missing<-utils::read.csv(file.path(qdir,"fulltexts_still_missing.csv"),
    stringsAsFactors=FALSE)
  testthat::expect_equal(nrow(missing),0L)
  testthat::expect_identical(
    evidencelandscaper::retrieve_w09_first10_all_included(
      map_path=td,w04_run=w04),report)
  # Check that no duplicate retrieval requests were made on resume.
  log<-readLines(file.path(qdir,"w09_openalex_content_retrieval.jsonl"),
                 warn=FALSE)
  testthat::expect_length(log,3L)
})
