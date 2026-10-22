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

test_that("Map-based consolidation discovers accepted RIS run", {
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("autodiscover", root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  plan <- plan_w00(map, strategy, sources = "lens")
  ris <- tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "TI  - Test paper", "ER  - "), ris)
  imp <- import_ris(map, ris, "pqdt", "ProQuest Dissertations & Theses",
                    "2026-10-22", "test search")
  accept_w00_ris(plan, imports = c(pqdt = imp))
  manifest <- consolidate_w00(map)
  expect_true(file.exists(manifest))
  data <- jsonlite::fromJSON(manifest)
  expect_equal(data$record_count, 1)
  expect_error(consolidate_w00(map), "already exists")
})
