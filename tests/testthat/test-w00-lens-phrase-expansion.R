test_that("Lens expands terminal quoted stars, other sources preserve them", {
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("lens_phrase_test", root)
  strategy <- list(search_version = "lens-phrase-v1",
    concepts = list(action = list(terms = c('"collective action"',
      '"multi-stakeholder initiative*"')),
      chain = list(terms = c('"food supply chain*"'))),
    expression = "action AND chain")
  file <- file.path(map, "inputs", "search", "v1.json")
  jsonlite::write_json(strategy, file, auto_unbox = TRUE, pretty = TRUE)
  plan <- plan_w00(map, sources = c("lens", "scopus", "wos", "openalex"))
  q <- jsonlite::fromJSON(plan$search_plan)$source_queries
  expect_true(grepl('"food supply chain" OR "food supply chains"', q$lens, fixed = TRUE))
  expect_true(grepl('"multi-stakeholder initiative" OR "multi-stakeholder initiatives"', q$lens, fixed = TRUE))
  expect_false(grepl('"food supply chain*"', q$lens, fixed = TRUE))
  expect_true(all(vapply(q[c("scopus", "wos", "openalex")],
    function(x) grepl('"food supply chain*"', x, fixed = TRUE), logical(1))))
})
