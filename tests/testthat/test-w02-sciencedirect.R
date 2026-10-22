test_that("ScienceDirect enrichment requires final W01 outputs", {
  root <- tempfile("w02-missing-")
  dir.create(root)
  expect_error(enrich_sciencedirect_abstracts(root, max_requests=0L),
               "Finalised W01")
})
test_that("ScienceDirect enrichment does not require credentials for validation", {
  expect_true("enrich_sciencedirect_abstracts" %in%
    getNamespaceExports("evidencelandscaper"))
})
