testthat::test_that("full-text atlas offers zooming, clustering, and multi-location cases", {
  root<-testthat::test_path("..","..")
  tpl<-file.path(root,"inst","atlas","ostara_fulltext_prototype.html")
  generator<-file.path(root,"R","ostara-fulltext-atlas-prototype.R")
  locator<-file.path(root,"R","ostara-fulltext-atlas-geocoding.R")
  for(f in c(generator,locator))testthat::expect_silent(parse(file=f))
  t<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  g<-paste(readLines(generator,warn=FALSE),collapse="\n")
  l<-paste(readLines(locator,warn=FALSE),collapse="\n")
  for(term in c('id="zoomIn"','id="zoomOut"','id="zoomHome"',
                'id="mapchoices"',"function zoomMap(",
                "function clustersFor(","function showMapChoices(",
                "allMappedPoints(shown)","entries.length>1"))
    testthat::expect_match(t,term,fixed=TRUE)
  testthat::expect_match(g,"coords<-lapply(k,function(j)",fixed=TRUE)
  testthat::expect_match(g,"plotted_points=nrow(geo)",fixed=TRUE)
  testthat::expect_match(l,"for(code in codes)",fixed=TRUE)
  testthat::expect_match(l,"mapped=mapped_count>0L",fixed=TRUE)
})


testthat::test_that("retrospective atlas source and requested presentation are supported", {
  root<-testthat::test_path("..","..")
  generator<-file.path(root,"R","ostara-fulltext-atlas-prototype.R")
  tpl<-file.path(root,"inst","atlas","ostara_fulltext_prototype.html")
  code<-paste(readLines(generator,warn=FALSE,encoding="UTF-8"),
              collapse="\n")
  html<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),
              collapse="\n")
  testthat::expect_silent(parse(file=generator))
  for(term in c("cases_csv=NULL","numbered<-grep(",
                'cases$cohort<-"retrospective"',
                "files<-cases_csv","plotted_points=nrow(geo)"))
    testthat::expect_match(code,term,fixed=TRUE)
  for(term in c('showEvidence=false','id="geographySummary"',
                'href="commodity_case_explorer.html"',
                'width:min(760px,calc(100% - 24px))',
                "22)\n    unite(i,j);"))
    testthat::expect_match(html,term,fixed=TRUE)
  testthat::expect_false(grepl('c.cohort==="first10"',html,fixed=TRUE))
  testthat::expect_match(html,"const totalUnmapped=",fixed=TRUE)
})


testthat::test_that("retrospective field adapter handles trailing spaces", {
  path<-testthat::test_path("..","..","R","ostara-fulltext-atlas-prototype.R")
  script<-paste(readLines(path,warn=FALSE),collapse="\n")
  testthat::expect_match(script,'trimws(f)',fixed=TRUE)
  testthat::expect_match(script,'ev<-matched_column("evidence")',fixed=TRUE)
  testthat::expect_match(script,'status<-matched_column("reporting status")',fixed=TRUE)
  # Representative headers from the user's actual 40-field source.
  columns<-c("02. What is the status? ",
    "02. What is the status?  [evidence]",
    "02. What is the status?  [reporting status]")
  pick<-function(f,suffix)which(endsWith(columns,paste0(" [",suffix,"]"))&
    trimws(sub(" \\[(evidence|reporting status)\\]$","",columns))==
      trimws(f))
  testthat::expect_equal(pick(columns[[1]],"evidence"),2L)
  testthat::expect_equal(pick(columns[[1]],"reporting status"),3L)
})


testthat::test_that("trackpad zoom uses gentle proportional increments", {
  tpl<-testthat::test_path("..","..","inst","atlas",
                          "ostara_fulltext_prototype.html")
  text<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  testthat::expect_match(text,"Math.exp(-limited*0.0012)",fixed=TRUE)
  testthat::expect_false(grepl("e.deltaY<0?1.35:1/1.35",text,fixed=TRUE))
})


testthat::test_that("case detail accordion follows Excel row colour groups", {
  tpl<-testthat::test_path("..","..","inst","atlas",
                          "ostara_fulltext_prototype.html")
  html<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  expected<-c(
    "Case and context"="#EAD1DC",
    "Collective-action arrangements"="#C9DAF8",
    "Objectives and outcomes"="#D9EAD3",
    "Enablers"="#D9D2E9",
    "Barriers"="#E6B8AF")
  for(i in seq_along(expected)) {
    testthat::expect_match(html,names(expected)[[i]],fixed=TRUE)
    testthat::expect_match(html,unname(expected[[i]]),fixed=TRUE)
  }
  for(term in c("if(n>=1&&n<=11)","if(n>=12&&n<=13)",
                "if(n>=14&&n<=21)","if(n>=22&&n<=30)",
                "if(n>=31&&n<=39)","if(n===40)",
                "group-'+groupSlug[name]"))
    testthat::expect_match(html,term,fixed=TRUE)
  testthat::expect_false(grepl('name==="Overview and geography"',html,fixed=TRUE))
})


testthat::test_that("multi-location atlas keeps all uniquely resolved sites and colours them", {
  root<-testthat::test_path("..","..")
  tpl<-file.path(root,"inst","atlas","ostara_fulltext_prototype.html")
  geo<-file.path(root,"R","ostara-fulltext-atlas-geocoding.R")
  testthat::expect_silent(parse(file=geo))
  html<-paste(readLines(tpl,warn=FALSE,encoding="UTF-8"),collapse="\n")
  geocode<-paste(readLines(geo,warn=FALSE,encoding="UTF-8"),collapse="\n")
  for(term in c(".marker.multi-location{fill:#2581c4}",
                "case-choice.multi-location{background:#e0f2ff",
                "entries.push({c,coord,x,y,multiLocation:locations.length>1})",
                'e.multiLocation?" multi-location":""',
                "allMulti=entries.every(e=>e.multiLocation)"))
    testthat::expect_match(html,term,fixed=TRUE)
  for(term in c("for(nm in localities)","for(hit in locality_hits)",
                "for(code in setdiff(codes,country_located))",
                "unresolved_localities=paste(unresolved,collapse=",
                "mapped_point_count=mapped_count"))
    testthat::expect_match(geocode,term,fixed=TRUE)
})
