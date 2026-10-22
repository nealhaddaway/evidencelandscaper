test_that("Manual RIS sources never use API retrieval", {
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("manual_only", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  plan <- plan_w00(map, strategy, sources = "cba")
  expect_error(run_w00(plan), "Manual RIS upload required")
  expect_false(file.exists(file.path(plan$run_dir, "w00_retrieval_manifest.json")))
})
