testthat::test_that("W07 portable client ZIP preserves mechanisms, abstracts and provenance", {
 testthat::skip_if(Sys.which("zip")=="","ZIP executable unavailable")
 td<-tempfile("ostara-w07-review-")
 dir.create(td)
 root<-file.path(td,"runs","W01","narrow")
 w04<-file.path(root,"W04_local","full-original")
 dir.create(file.path(w04,"finalised"),recursive=TRUE)
 dir.create(file.path(root,"W02_main","finalised"),recursive=TRUE)
 w07<-file.path(td,"runs","W07","trial-2-v4")
 dir.create(w07,recursive=TRUE)
 sha<-function(p)digest::digest(file=p,algo="sha256",serialize=FALSE)
 writejl<-function(x,p)writeLines(vapply(x,function(z)
  as.character(jsonlite::toJSON(z,auto_unbox=TRUE,null="null")),
  character(1)),p,useBytes=TRUE)
 canonical<-file.path(root,"W02_main","finalised","canonical.jsonl")
 records<-list(
  list(identity=list(record_id="id-1"),canonical=list(
   title="Study one",abstract="Abstract of first study",
   authors="Researcher A",year=2026)),
  list(identity=list(record_id="id-2"),canonical=list(
   title="Study two",abstract="Abstract of second study",
   authors="Researcher B",year=2025)))
 writejl(records,canonical)
 coding<-file.path(w07,"coding.jsonl")
 results<-list(
  list(record_id="id-1",coding_status="coded",
       mechanisms=list(list(mechanism_id="M1.1.1",
                            evidence="Coordinated supply agreements",
                            confidence="high",
                            implementation_status="implemented",
                            evidence_source="abstract",
                            evidence_excerpt="Abstract of first study")),
       ontology_gap=FALSE,coding_notes=""),
  list(record_id="id-2",coding_status="no_supported_mechanism",
       mechanisms=list(),ontology_gap=FALSE,coding_notes="No evidence"))
 writejl(results,coding)
 ontology<-file.path(w07,"ontology.csv")
 utils::write.csv(data.frame(
  mechanism_id=c("M1.1.1","M1.1.2"),
  collective_action_type="Market",
  high_level_mechanism="Agreements",
  fine_level_mechanism=c("A","B"),
  definition=c("First mechanism","Second mechanism")),ontology,row.names=FALSE)
 jsonlite::write_json(list(
    source_w04=w04,canonical_w02_sha256=sha(canonical),
    ontology_sha256=sha(ontology),prompt_sha256=paste(rep("a",64),collapse=""),
    selected_record_ids=list("id-1","id-2"),
    source_w04_sha256=paste(rep("b",64),collapse="")),
  file.path(w07,"manifest.json"),auto_unbox=TRUE)
 jsonlite::write_json(list(status="TRIAL_COMPLETE",coding_sha256=sha(coding)),
   file.path(w07,"summary.json"),auto_unbox=TRUE)
 jsonlite::write_json(list(reconciled_abstracts=FALSE),
   file.path(w04,"finalised","summary.json"),auto_unbox=TRUE)
 output<-file.path(td,"review.zip")
 testthat::expect_identical(export_w07_colleague_review(w07,output),output)
 testthat::expect_true(file.exists(output))
 testthat::expect_error(export_w07_colleague_review(w07,output),"exists")
 unzip_dir<-file.path(td,"unzip")
 dir.create(unzip_dir)
 utils::unzip(output,exdir=unzip_dir)
 b<-file.path(unzip_dir,"w07_review_bundle")
 testthat::expect_true(file.exists(file.path(b,"app.R")))
 testthat::expect_silent(parse(file=file.path(b,"app.R")))
 meta<-jsonlite::fromJSON(file.path(b,"manifest.json"),
                           simplifyVector=FALSE)
 q<-file.path(b,"data","review_queue.jsonl")
 testthat::expect_identical(meta$queue_sha256,sha(q))
 cases<-lapply(readLines(q),jsonlite::fromJSON,simplifyVector=FALSE)
 testthat::expect_length(cases,2)
 testthat::expect_identical(cases[[1]]$coding$mechanisms[[1]]$mechanism_id,
                            "M1.1.1")
 testthat::expect_identical(cases[[2]]$coding$coding_status,
                            "no_supported_mechanism")
 testthat::expect_match(cases[[1]]$bibliographic$abstract,"first study")
 testthat::expect_false(file.exists(file.path(b,"decisions.jsonl")))
})
