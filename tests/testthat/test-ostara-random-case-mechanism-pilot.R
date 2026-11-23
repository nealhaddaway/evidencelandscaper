test_that("random OSTARA pilot samples from case sheet first and resolves existing W09 PDFs", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  skip_if_not_installed("httr2")
  skip_if_not_installed("pdftools")
  root<-tempfile("ostara-random-work-pilot-")
  dir.create(root)
  map<-file.path(root,"Ostara_collective_action_map")
  dir.create(map)
  w09<-file.path(map,"runs","W09","existing-retrieval-run")
  dir.create(file.path(w09,"pdfs"),recursive=TRUE)
  ids<-sprintf("work-%02d",1:11)
  cases<-data.frame(record_id=ids,case_id=paste0(ids,"::case_001"),
    "01. Case name"=paste("Case",ids),
    check.names=FALSE,stringsAsFactors=FALSE)
  cases_csv<-file.path(root,"all_case_data - Retrospective.csv")
  utils::write.csv(cases,cases_csv,row.names=FALSE,na="")
  ontology<-data.frame(mechanism_id=sprintf("M%02d",seq_len(26)),
    category="Governance",function="Coordination",
    mechanism=paste("Mechanism",seq_len(26)),
    definition=paste("Definition",seq_len(26)),stringsAsFactors=FALSE)
  ontology_csv<-file.path(root,"ontology.csv")
  utils::write.csv(ontology,ontology_csv,row.names=FALSE,na="")
  utils::write.csv(data.frame(record_id=ids,
    title=paste("Article",ids),doi=rep("",length(ids))),
    file.path(w09,"retrieval_queue.csv"),row.names=FALSE,na="")
  for(id in ids) {
    grDevices::pdf(file.path(w09,"pdfs",paste0(id,".pdf")))
    graphics::plot.new()
    graphics::text(.5,.5,paste("Collective action mechanism in",id))
    grDevices::dev.off()
  }
  out<-file.path(root,"pilot")
  result<-run_ostara_random_case_mechanism_pilot(map,cases_csv,
    ontology_csv,out,seed=123L,run_extraction=FALSE)
  chosen<-utils::read.csv(result$selection,
    stringsAsFactors=FALSE,colClasses="character")
  expect_equal(nrow(chosen),10L)
  expect_equal(length(unique(chosen$record_id)),10L)
  expect_true(all(chosen$record_id%in%ids))
  audit<-utils::read.csv(result$source_audit,stringsAsFactors=FALSE)
  expect_equal(nrow(audit),10L)
  expect_true(all(audit$availability=="located"))
  expect_true(all(audit$ext=="pdf"))
  expect_true(file.exists(result$pilot$selection))
  expect_equal(result$completed,0L)
  again<-run_ostara_random_case_mechanism_pilot(map,cases_csv,
    ontology_csv,out,seed=123L,run_extraction=FALSE)
  expect_identical(readLines(result$selection),readLines(again$selection))
  expect_error(run_ostara_random_case_mechanism_pilot(map,cases_csv,
    ontology_csv,out,seed=124L,run_extraction=FALSE),"changed")
})

test_that("missing prior full texts are audited without resampling or paid calls", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root<-tempfile("ostara-random-work-missing-")
  dir.create(root)
  map<-file.path(root,"Ostara_collective_action_map")
  w09<-file.path(map,"runs","W09","empty-prior-run")
  dir.create(w09,recursive=TRUE)
  ids<-sprintf("work-%02d",1:10)
  cases<-data.frame(record_id=ids,case_id=paste0(ids,"::case_001"),
    "01. Case name"=paste("Case",ids),check.names=FALSE)
  cases_csv<-file.path(root,"cases.csv")
  utils::write.csv(cases,cases_csv,row.names=FALSE)
  ontology<-data.frame(mechanism_id=sprintf("M%02d",1:26),
    category="A",function="B",mechanism="C",definition="D")
  ontology_csv<-file.path(root,"ontology.csv")
  utils::write.csv(ontology,ontology_csv,row.names=FALSE)
  utils::write.csv(data.frame(record_id=ids,title=ids,doi=""),
    file.path(w09,"retrieval_queue.csv"),row.names=FALSE)
  result<-run_ostara_random_case_mechanism_pilot(map,cases_csv,
    ontology_csv,file.path(root,"pilot"),run_extraction=FALSE)
  expect_equal(nrow(result$missing),10L)
  expect_null(result$pilot)
  expect_true(file.exists(result$selection))
  expect_true(file.exists(result$source_audit))
})
