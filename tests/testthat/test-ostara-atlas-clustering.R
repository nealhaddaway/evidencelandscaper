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
