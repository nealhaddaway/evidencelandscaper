test_that("W00 scoping refuses manual RIS sources", {
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("scoping", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  plan <- plan_w00(map, strategy, sources = "cba")
  expect_error(scope_w00(plan), "manual RIS")
  expect_false(dir.exists(file.path(plan$run_dir, "scoping")))
})
