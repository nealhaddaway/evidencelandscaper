test_that("Consolidation rejects missing sources without creating output", {
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("consolidation", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  plan <- plan_w00(map, strategy, sources = "lens")
  expect_error(consolidate_w00(plan),
               "No completed automatic retrieval or accepted manual RIS imports")
  expect_false(dir.exists(file.path(plan$run_dir, "consolidated")))
})
