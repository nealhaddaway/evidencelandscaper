test_that("Map strategy auto-discovery includes Scopus, WoS and EBSCO", {
  skip_if_not_installed("jsonlite")
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("all_sources", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  file.copy(strategy, file.path(map, "inputs", "search", "v1.json"))
  plan <- plan_w00(map)
  queries <- jsonlite::fromJSON(plan$search_plan)$source_queries
  expect_true(all(c("lens", "scopus", "wos", "openalex",
                    "ebsco_greenfile", "ebsco_medline") %in% names(queries)))
  expect_false(any(c("cba", "pqdt") %in% names(queries)))
})
