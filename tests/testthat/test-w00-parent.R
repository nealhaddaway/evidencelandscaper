test_that("W00 parent refuses unimplemented sources before API calls", {
  skip_if_not_installed("jsonlite")
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("parent_test", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  p <- plan_w00(map, strategy, sources = "scopus")
  expect_error(run_w00(p), "not yet implemented for: scopus")
  expect_false(file.exists(file.path(p$run_dir, "w00_retrieval_manifest.json")))
})
test_that("W00 parent checks source list and limit without API calls", {
  expect_error(run_w00(list()), "valid existing")
})
