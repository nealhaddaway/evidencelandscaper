test_that("remaining-90% W09 full-text preparation is installed", {
  expect_true(is.function(evidencelandscaper::prepare_w09_remaining90_all_included))
  expect_true(is.function(evidencelandscaper::retrieve_w09_remaining90_all_included))
  expect_true("w04_run" %in% names(formals(
    evidencelandscaper::prepare_w09_remaining90_all_included)))
})

test_that("remaining-90% W09 functions require the exact W04 cohort", {
  source_file <- system.file(package="evidencelandscaper")
  expect_true(nzchar(source_file))
  expect_error(
    evidencelandscaper::prepare_w09_remaining90_all_included(
      map_path=tempdir(), w04_run=NULL),
    regexp="Supply the exact remaining-90% W04 run"
  )
})
