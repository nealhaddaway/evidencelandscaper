test_that("Lens sample validates arguments before contacting the API", {
  expect_error(run_w00_lens(list(), 10), "existing plan_w00")
  expect_error(run_w00_lens(list(), 0), "existing plan_w00")
})
