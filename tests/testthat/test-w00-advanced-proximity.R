test_that("advanced W00 uses native pairwise proximity and explicit Lens forms", {
  root <- tempfile("proximity-root")
  dir.create(root)
  map <- create_map("proximity_test", root)
  strategy <- file.path(map, "inputs", "search", "strategy.json")
  query <- '((research* OR stud*) W/5 (collaborat* OR participat*)) AND "climat* chang*"'
  cfg <- list(advanced_boolean=query,
    lens_word_forms=list("research*"=c("research","researching"),"stud*"=c("study","studies"),
      "collaborat*"=c("collaboration","collaborative"),"participat*"=c("participation","participatory")),
    lens_phrase_forms=list("climat* chang*"=c("climate change","climatic changes")))
  jsonlite::write_json(cfg, strategy, auto_unbox=TRUE)
  result <- plan_w00(map, strategy, sources=c("lens","scopus","wos","openalex","pubmed","ebsco_greenfile"))
  p <- jsonlite::fromJSON(result$search_plan, simplifyVector=FALSE)
  q <- p$source_queries
  expect_match(q$scopus, "W/5", fixed=TRUE)
  expect_match(q$wos, "NEAR/5", fixed=TRUE)
  expect_true(startsWith(q$wos,"TS="))
  expect_match(q$ebsco_greenfile, "N5", fixed=TRUE)
  expect_match(q$openalex, '("research*" and "collaborat*" within 5 words of each other)', fixed=TRUE)
  expect_match(q$openalex, '("stud*" and "participat*" within 5 words of each other)', fixed=TRUE)
  expect_match(q$openalex, "type is not [dataset](dataset)", fixed=TRUE)
  expect_match(q$lens, '"research participatory"~5', fixed=TRUE)
  expect_match(q$lens, '"climate change" OR "climatic changes"', fixed=TRUE)
  expect_match(q$lens, "AND NOT publication_type:dataset", fixed=TRUE)
  expect_match(q$pubmed, "(research* OR stud*) AND (collaborat* OR participat*)", fixed=TRUE)
  expect_true(any(grepl("broadened", unlist(p$translation_notes$pubmed))))
  expect_identical(jsonlite::fromJSON(strategy)$advanced_boolean, query)
  cfg$lens_word_forms <- NULL
  jsonlite::write_json(cfg, strategy, auto_unbox=TRUE)
  expect_error(plan_w00(map, strategy, sources="lens"), "lens_word_forms")
  cfg$exclude_datasets <- FALSE
  jsonlite::write_json(cfg, strategy, auto_unbox=TRUE)
  q2 <- jsonlite::fromJSON(plan_w00(map,strategy,sources="openalex")$search_plan)$source_queries$openalex
  expect_false(grepl("type is not",q2,fixed=TRUE))
})
