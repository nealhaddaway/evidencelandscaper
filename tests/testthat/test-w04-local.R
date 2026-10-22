test_that("W04 local functions require genuine finalised inputs", {
  d <- tempfile("w04-")
  dir.create(d)
  p <- tempfile(fileext=".txt")
  writeLines("test screening",p)
  expect_error(prepare_w04(d,p),"Finalised W02")
  expect_error(run_w04(d,p,max_records=0),"Missing finalised W02/W03")
})
test_that("Original W04 consensus engine is packaged", {
  expect_true(nzchar(system.file("w04",
   "workflow_04_luna_consensus.R",package="evidencelandscaper")))
})
