test_that("Commodity explorer keeps all cases and evidence while merging synonyms", {
  skip_if_not_installed("countrycode")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root <- tempfile("ostara-commodity-fixture-")
  dir.create(root)
  client <- file.path(root,"reports","client_databases","sample")
  dir.create(client,recursive=TRUE)
  raw <- data.frame(
    record_id=c("recordA","recordB","recordC"),
    cohort=c("first10","remaining90","remaining90"),
    case_id=c("recordA::case_001","recordB::case_001",
              "recordC::case_001"),
    assessment_reason=c("eligible","eligible","eligible"),
    case_boundary_uncertainty=c("","",""),
    check.names=FALSE,stringsAsFactors=FALSE)
  set_field <- function(dat,n,q,values,evidence,status="reported") {
    k<-paste0("field_",sprintf("%03d",n)," :: ",q)
    dat[[k]]<-values
    dat[[paste0(k," [evidence]")]]<-evidence
    dat[[paste0(k," [reporting status]")]]<-status
    dat
  }
  raw<-set_field(raw,1,"Case name",
    c("Village A","Village B","Village C"),
    c("[PAGE 2]: A","[PAGE 5]: B","[PAGE 4]: C"))
  raw<-set_field(raw,2,"Which commodity/commodities is the collective action addressing?",
    c("cacao | cocoa","cacao/cocoa | soy",""),
    c("[PAGE 2]: cacao","[PAGE 5]: cocoa",""),
    c("reported","reported","not_reported"))
  raw<-set_field(raw,3,"Where did the case take place (country)?",
    c("GHA","CIV",""),
    c("[PAGE 2]: Ghana","[PAGE 5]: Côte d'Ivoire",""),
    c("reported","reported","not_reported"))
  raw<-set_field(raw,4,"Supply chain stages",
    c("production","processing",""),
    c("[PAGE 2]: production","[PAGE 5]: processing",""),
    c("reported","reported","not_reported"))
  raw<-set_field(raw,5,"Case description",
    c("Producer collective","Farm cooperative",""),
    c("[PAGE 2]: Producer","[PAGE 5]: Farm",""),
    c("reported","reported","not_reported"))
  pubs<-data.frame(
    record_id=c("recordA","recordB","recordC"),
    cohort=c("first10","remaining90","remaining90"),
    title=c("Publication A","Publication B","Publication C"),
    doi=c("10.1234/a","10.1234/b",""),
    stringsAsFactors=FALSE)
  utils::write.csv(raw,file.path(client,
    "ostara_cases_with_adjacent_evidence.csv"),row.names=FALSE,na="")
  utils::write.csv(pubs,file.path(client,"ostara_publications.csv"),
                   row.names=FALSE,na="")
  html<-export_ostara_commodity_case_explorer(
    map_path=root,client_export_dir=client)
  expect_true(file.exists(html))
  txt<-paste(readLines(html,warn=FALSE,encoding="UTF-8"),
             collapse="\n")
  expect_true(grepl("Producer collective",txt,fixed=TRUE))
  expect_true(grepl("[PAGE 2]: cacao",txt,fixed=TRUE))
  expect_true(grepl("Explore full case",txt,fixed=TRUE))
  expect_true(grepl("Ghana",txt,fixed=TRUE))
  expect_true(grepl("Cocoa",txt,fixed=TRUE))
  expect_false(grepl("mapboxgl",txt,fixed=TRUE))
  inv<-utils::read.csv(file.path(dirname(html),"commodity_inventory.csv"),
                       stringsAsFactors=FALSE)
  expect_equal(inv$cases[inv$commodity=="Cocoa"],2)
  expect_equal(inv$cases[inv$commodity=="Soybeans"],1)
  audit<-utils::read.csv(file.path(dirname(html),"case_coverage_audit.csv"),
                         stringsAsFactors=FALSE)
  expect_equal(nrow(audit),3L)
  expect_equal(sum(audit$commodity_count==0),1L)
  html2<-export_ostara_commodity_case_explorer(
    map_path=root,client_export_dir=client)
  expect_identical(html2,html)
})
