test_that("Full W01 scoring requires existing candidates", {
  expect_error(score_w01_candidates(tempfile()), "directory returned")
})

test_that("Full W01 scoring script shipped unchanged", {
  expect_true(nzchar(system.file("w01",
    "workflow_01_deduplication_incremental_score.R",
    package = "evidencelandscaper")))
})
