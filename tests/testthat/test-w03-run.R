test_that("W03 requires finalised W02 input", {
  root <- tempfile("w03-")
  dir.create(root)
  expect_error(run_w03(root), "Finalised W02 canonical input missing")
})
test_that("Upstream W03 implementation is packaged", {
  expect_true(nzchar(system.file("w03",
    "workflow_03_publication_status.R",package="evidencelandscaper")))
})
