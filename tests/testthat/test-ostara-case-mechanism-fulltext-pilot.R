test_that("case-mechanism pilot freezes ten case-positive publications without API calls", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  skip_if_not_installed("httr2")
  skip_if_not_installed("pdftools")
  root<-tempfile("ostara-case-mech-pilot-")
  dir.create(root)
  source_dir<-file.path(root,"fulltexts")
  dir.create(source_dir)
  ids<-sprintf("work-%02d",1:11)
  rows<-do.call(rbind,lapply(seq_along(ids),function(i) {
    n<-if(i<=5L)2L else 1L
    data.frame(record_id=rep(ids[[i]],n),
      case_id=paste0(ids[[i]],"::case_",sprintf("%03d",seq_len(n))),
      "01. Case name"=paste0("Case ",i,"-",seq_len(n)),
      check.names=FALSE,stringsAsFactors=FALSE)
  }))
  cases_csv<-file.path(root,"all_case_data - Retrospective.csv")
  utils::write.csv(rows,cases_csv,row.names=FALSE,na="")
  ontology<-data.frame(mechanism_id=sprintf("M%02d",1:26),
    category="Coordination",function="Organisation",
    mechanism=paste0("Mechanism ",1:26),
    definition=paste0("Operational mechanism ",1:26),
    stringsAsFactors=FALSE)
  ontology_csv<-file.path(root,"ontology.csv")
  utils::write.csv(ontology,ontology_csv,row.names=FALSE,na="")
  for(id in ids)writeLines(
    c("[PAGE 1]","Named case and collective action"),
    file.path(source_dir,paste0(id,".txt")))
  fm<-data.frame(record_id=ids,
    fulltext_path=file.path("fulltexts",paste0(ids,".txt")))
  manifest_csv<-file.path(root,"fulltexts.csv")
  utils::write.csv(fm,manifest_csv,row.names=FALSE,na="")
  out<-file.path(root,"pilot")
  x<-run_ostara_case_mechanism_fulltext_pilot(
    cases_csv,ontology_csv,manifest_csv,out,run_extraction=FALSE)
  expect_equal(x$completed,0L)
  selection<-utils::read.csv(x$selection,stringsAsFactors=FALSE)
  expect_equal(nrow(selection),10L)
  expect_equal(selection$record_id[1:5],ids[1:5])
  expect_true(all(selection$case_count[1:5]==2L))
  expect_true(file.exists(file.path(out,"manifest.json")))
  expect_true(file.exists(file.path(out,"prompt.txt")))
  expect_true(file.exists(file.path(out,"ontology.csv")))
  expect_equal(nrow(utils::read.csv(x$attributions)),0L)
  y<-run_ostara_case_mechanism_fulltext_pilot(
    cases_csv,ontology_csv,manifest_csv,out,run_extraction=FALSE)
  expect_identical(x$selection,y$selection)
  expect_error(run_ostara_case_mechanism_fulltext_pilot(
    cases_csv,ontology_csv,manifest_csv,out,record_ids=rev(ids[1:10]),
    run_extraction=FALSE),"changed")
  fm$fulltext_path[[1]]<-"missing.txt"
  utils::write.csv(fm,manifest_csv,row.names=FALSE,na="")
  expect_error(run_ostara_case_mechanism_fulltext_pilot(
    cases_csv,ontology_csv,manifest_csv,out,run_extraction=FALSE))
})
