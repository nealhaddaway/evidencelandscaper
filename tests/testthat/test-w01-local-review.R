test_that("Portable W01 bundle retains only assigned cases", {
  root <- tempfile("w01-review-")
  dir.create(root)
  queue <- file.path(root,"queue.jsonl")
  case <- function(id) jsonlite::toJSON(list(
    schema="living-evidence-map-workflow01-duplicate-adjudication-case-v1",
    review_case_id=id,record_i=list(title="A"),record_j=list(title="B")),
    auto_unbox=TRUE)
  writeLines(c(case("case1"),case("case2")),queue)
  bundle <- file.path(root,"bundle")
  package_w01_review(queue,bundle,case_ids="case2")
  out <- readLines(file.path(bundle,"assigned_cases.jsonl"))
  expect_length(out,1L)
  expect_identical(jsonlite::fromJSON(out[[1]])$review_case_id,"case2")
  expect_true(file.exists(file.path(bundle,"app.R")))
  expect_true(file.exists(file.path(bundle,"launch.R")))
  expect_error(package_w01_review(queue,bundle), "already exists")
})
test_that("Imported W01 reviewer layers validate identity and checksum", {
  root <- tempfile("w01-review-")
  dir.create(root)
  queue <- file.path(root,"queue.jsonl")
  writeLines(jsonlite::toJSON(list(
    schema="living-evidence-map-workflow01-duplicate-adjudication-case-v1",
    review_case_id="case1",record_i=list(title="A"),record_j=list(title="B")),
    auto_unbox=TRUE),queue)
  bundle <- package_w01_review(queue,file.path(root,"bundle"))
  state <- file.path(bundle,"reviewer_state")
  dir.create(state)
  jsonlite::write_json(list(reviewer="Test Reviewer"),file.path(state,"reviewer.json"),
                       auto_unbox=TRUE)
  manifest <- jsonlite::fromJSON(file.path(bundle,"bundle_manifest.json"))
  jsonlite::write_json(list(schema="local-w01-decision-v1",
    review_case_id="case1",reviewer="Test Reviewer",
    decision="not_duplicate",rationale="Distinct works",
    queue_sha256=manifest$assigned_queue_sha256),
    file.path(state,"decisions.jsonl"),auto_unbox=TRUE)
  imported <- import_w01_review(bundle,file.path(root,"layers"))
  expect_true(file.exists(imported))
  expect_equal(jsonlite::fromJSON(imported)$case_count,1L)
  expect_error(import_w01_review(bundle,file.path(root,"layers")),"Already imported")
})
