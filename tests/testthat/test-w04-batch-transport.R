test_that("W04 batch transport files parse and are packaged", {
  engine <- system.file("w04", "workflow_04_luna_consensus.R",
                        package="evidencelandscaper")
  helper <- system.file("w04", "workflow_04_batch.R",
                        package="evidencelandscaper")
  expect_true(nzchar(engine))
  expect_true(nzchar(helper))
  expect_silent(parse(file=engine))
  expect_silent(parse(file=helper))
})

test_that("W04 keeps explicit batching defaults and model lock", {
  expect_true("execution" %in% names(formals(evidencelandscaper::run_w04)))
  expect_true("batch_chunk_size" %in%
                names(formals(evidencelandscaper::run_w04)))
  expect_true("execution" %in%
                names(formals(evidencelandscaper::run_w04_remaining_90)))
})
