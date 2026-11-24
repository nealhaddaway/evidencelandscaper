test_that("advanced W00 proximity is translated and Lens phrase changes audited", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root <- tempfile("proximity-root")
  dir.create(root)
  map <- create_map("proximity_test", root)
  strategy <- file.path(map, "inputs", "search", "strategy.json")
  query <- '((research* OR stud*) W/5 (collaborat* OR participat*)) AND "climat* chang*"'
  jsonlite::write_json(list(advanced_boolean=query), strategy, auto_unbox=TRUE)
  result <- plan_w00(map, strategy, sources=c("lens","scopus","wos","openalex","pubmed","ebsco_greenfile"))
  p <- jsonlite::fromJSON(result$search_plan, simplifyVector=FALSE)
  q <- p$source_queries
  expect_match(q$scopus, "W/5", fixed=TRUE)
  expect_match(q$wos, "NEAR/5", fixed=TRUE)
  expect_match(q$ebsco_greenfile, "N5", fixed=TRUE)
  for (src in c("lens","openalex","pubmed")) {
    expect_false(grepl("W/5", q[[src]], fixed=TRUE))
    expect_match(q[[src]], "(research* OR stud*) AND (collaborat* OR participat*)", fixed=TRUE)
    expect_true(any(grepl("broadened", unlist(p$translation_notes[[src]]))))
  }
  expect_match(q$lens, "(climat* AND chang*)", fixed=TRUE)
  expect_false(grepl('"climat* chang*"', q$lens, fixed=TRUE))
  expect_identical(jsonlite::fromJSON(strategy)$advanced_boolean, query)
})
