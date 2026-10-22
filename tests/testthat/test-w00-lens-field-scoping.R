test_that("Lens two-concept AND stays within one field", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("digest")
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("lens_fields", root = root)
  strategy <- system.file("w00", "example-strategy.json",
                          package = "evidencelandscaper")
  plan <- plan_w00(map, strategy, sources = "lens", mode = "full")
  query <- jsonlite::fromJSON(plan$search_plan)$source_queries$lens
  expect_match(query, "title:\\(greenhouse", fixed = FALSE)
  expect_match(query, "title:(Almeria OR Spain)", fixed = TRUE)
  expect_match(query, "abstract:(Almeria OR Spain)", fixed = TRUE)
  expect_match(query, "keyword:(Almeria OR Spain)", fixed = TRUE)
  expect_false(grepl("OR keyword:(greenhouse* OR protected cultivation)) AND",
                     query, fixed = TRUE))
})
