test_that("Universal RIS import preserves raw and creates local handoff", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("ris_test", root = root)
  ris <- tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "TI  - Sample greenhouse article",
               "AU  - Example, Jane", "PY  - 2026",
               "AB  - Greenhouse methods in Spain", "DO  - 10.1234/demo",
               "ER  - "), ris)
  output <- import_ris(map, ris, source = "pqdt",
      database = "ProQuest Dissertations & Theses",
      search_date = "2026-10-22", search_string = "greenhouse AND Spain")
  expect_true(file.exists(file.path(output, "raw", basename(ris))))
  expect_true(file.exists(file.path(output, "handoff", "records.jsonl")))
  manifest <- jsonlite::fromJSON(file.path(output, "manifest.json"))
  expect_identical(manifest$status, "success")
  expect_equal(manifest$records$unique_records_for_handover, 1)
})
