test_that("W02 canonical preparation requires final W01 clusters", {
  d <- tempfile("w02-test-")
  dir.create(d)
  expect_error(prepare_w02(d), "Missing finalised W01 input")
})
test_that("W02-main and ScienceDirect adapters are installed", {
  expect_true(nzchar(system.file("w02","workflow_02_metadata_enrichment.R",
                                package="evidencelandscaper")))
  expect_true("add_w02_sciencedirect" %in%
              getNamespaceExports("evidencelandscaper"))
})
