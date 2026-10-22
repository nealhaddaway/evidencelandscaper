test_that("W00 retains earlier consolidation when manual registry changes", {
  root <- tempfile("w00-regression-")
  dir.create(root)
  map <- create_map("revision", root)
  run <- file.path(map, "runs", "W00", "20261022T141901-test")
  dir.create(file.path(run, "plan"), recursive=TRUE)
  jsonlite::write_json(list(run_type="full"),
    file.path(run,"plan","search_plan.json"), auto_unbox=TRUE)
  dest <- file.path(run,"sources","lens")
  dir.create(dest, recursive=TRUE)
  jsonlite::write_json(list(status="success",unique_records_written=1),
    file.path(dest,"manifest.json"),auto_unbox=TRUE)
  writeLines('{"identity":{"lens_id":"test"}}',
             file.path(dest,"records.jsonl"))
  jsonlite::write_json(list(status="success",
    sources=list(lens=list(status="success",records=1))),
    file.path(run,"w00_full_retrieval_manifest.json"),auto_unbox=TRUE)
  first <- consolidate_w00(map)
  expect_equal(jsonlite::fromJSON(first)$record_count,1)
  # This test isolates the immutable-revision selection and does not
  # fabricate a successful RIS import or bypass its checksum checks.
  jsonlite::write_json(list(schema="evidencelandscaper-w00-manual-acceptance-v1",
    run_id=basename(run),imports=list()),
    file.path(run,"accepted_manual_imports.json"),auto_unbox=TRUE)
  second <- consolidate_w00(map)
  expect_false(identical(first,second))
  expect_true(file.exists(first))
  expect_true(file.exists(second))
  expect_equal(jsonlite::fromJSON(second)$record_count,1)
  expect_identical(consolidate_w00(map),second)
})
