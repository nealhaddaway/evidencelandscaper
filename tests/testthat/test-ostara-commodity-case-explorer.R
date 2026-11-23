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


test_that("retrospective commodity explorer merges palm and uses atlas case groups", {
  skip_if_not_installed("countrycode")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root<-tempfile("ostara-retrospective-commodity-")
  dir.create(root)
  csv<-file.path(root,"all_case_data - Retrospective.csv")
  x<-data.frame(record_id=c("work-a","work-b"),
    case_id=c("work-a::case_001","work-b::case_001"),
    assessment_reason=c("eligible","eligible"),check.names=FALSE)
  x[["01. Case name"]]<-c("Palm site A","Palm site B")
  x[["02. What is the status? "]]<-c("active","active")
  x[["06. Which commodity/commodities is the collective action addressing?"]]<-
    c("palm; sugarcane","palm oil; sugar")
  x[["40. Was this source/publication written, funded or commissioned by an organisation taking part in the case?"]]<-c("yes","no")
  base<-grep("^[0-9]{2}\\. ",names(x),value=TRUE)
  for(nm in base) {
    x[[paste0(nm," [evidence]")]]<-"[PAGE 1]: source statement"
    x[[paste0(nm," [reporting status]")]]<-"reported"
  }
  utils::write.csv(x,csv,row.names=FALSE,na="")
  html<-export_ostara_commodity_case_explorer(map_path=root,cases_csv=csv,
    atlas_url="evidence_atlas.html")
  expect_true(file.exists(html))
  content<-paste(readLines(html,warn=FALSE,encoding="UTF-8"),collapse="\n")
  inv<-utils::read.csv(file.path(dirname(html),"commodity_inventory.csv"))
  expect_equal(inv$commodity,c("Palm oil","Sugar"))
  expect_equal(inv$cases,c(2L,2L))
  expect_false("Sugarcane"%in%inv$commodity)
  expect_true(grepl("Saccharum",content,fixed=TRUE))
  expect_false(grepl("Adolphe Millot",content,fixed=TRUE))
  expect_false(grepl('"Palm"',content,fixed=TRUE))
  expect_false(grepl("40. Was this source",content,fixed=TRUE))
  expect_false(grepl("Extraction cohort:",content,fixed=TRUE))
  expect_true(grepl('"Description","Activities","Objectives and Impact","Enablers","Barriers"',content,fixed=TRUE))
  expect_true(grepl('showEvidence=false',content,fixed=TRUE))
  expect_true(grepl('width:min(900px,100vw)',content,fixed=TRUE))
  expect_true(grepl('formatFieldAnswer(val)',content,fixed=TRUE))
  expect_true(grepl('"field_number":6',content,fixed=TRUE))
  expect_true(grepl('evidence_atlas.html',content,fixed=TRUE))
  expect_true(grepl('Palm site A',content,fixed=TRUE))
  expect_true(grepl('Palm site B',content,fixed=TRUE))
  expect_false(grepl('Case work-a::case_001',content,fixed=TRUE))
  expect_false(grepl('Case work-b::case_001',content,fixed=TRUE))
  embedded<-sub('(?s)^.*?<script id="data" type="application/json">',"",content,perl=TRUE)
  embedded<-sub("(?s)</script>.*$","",embedded,perl=TRUE)
  payload<-jsonlite::fromJSON(embedded,simplifyVector=FALSE)
  expect_equal(vapply(payload$cases,function(c)c$name,character(1)),
               c("Palm site A","Palm site B"))
})


test_that("commodity explorer colours case pills by known facet on cards and popups", {
  tpl<-testthat::test_path("..","..","inst","ostara",
                          "commodity_explorer_template.html")
  html<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  for(facet in c("commodity","country","stage","status","level","interest"))
    expect_true(grepl(paste0(".tag-",facet,"{background:"),html,fixed=TRUE))
  expect_true(grepl('tag.className="tag tag-"+facet;',html,fixed=TRUE))
  expect_true(grepl("addTags(pills,c);",html,fixed=TRUE))
  expect_true(grepl('addTags($("detail-tags"),c);',html,fixed=TRUE))
  expect_true(grepl('value,facet:"commodity"',html,fixed=TRUE))
  expect_true(grepl('value,facet:"country"',html,fixed=TRUE))
  expect_true(grepl('value,facet:"stage"',html,fixed=TRUE))
  expect_true(grepl('value,facet:"status"',html,fixed=TRUE))
})
