test_that("EBSCO database configuration is discoverable without API calls", {
  cfg <- system.file("w00", "config/workflow00_ebsco_sources.json",
                     package = "evidencelandscaper")
  expect_true(file.exists(cfg))
  sources <- names(jsonlite::fromJSON(cfg, simplifyVector = FALSE)$sources)
  expect_true(all(c("ebsco_medline", "ebsco_greenfile") %in% sources))
  expect_error(run_w00_ebsco(list(), "not_a_database"),
               "Unknown configured EBSCO source")
})
test_that("EBSCO requires credentials before local retrieval", {
  skip_if(nzchar(Sys.getenv("EBSCO_EHOST_UID")) &&
          nzchar(Sys.getenv("EBSCO_EHOST_PWD")))
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("ebsco_test", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  plan <- plan_w00(map, strategy, sources = "ebsco_greenfile")
  expect_error(run_w00(plan, 10L), "EBSCO_EHOST_UID")
})
