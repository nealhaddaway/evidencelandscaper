test_that("Full W00 consolidation reads Scopus and WoS raw page records", {
  root <- tempfile("evidence-maps-")
  dir.create(root)
  map <- create_map("full_test", root)
  run <- file.path(map, "runs", "W00", "20261022T150000-test")
  dir.create(file.path(run, "plan"), recursive = TRUE)
  jsonlite::write_json(list(run_type = "full"),
                       file.path(run, "plan", "search_plan.json"),
                       auto_unbox = TRUE)
  specs <- list(
    scopus = list(n = 2L, field = "entries_retrieved",
                  page = list("search-results" = list(entry = list(
                    list(eid = "2-s2.0-A"), list(eid = "2-s2.0-B"))))),
    wos = list(n = 1L, field = "records_retrieved",
               page = list(metadata = list(total = 1L),
                           hits = list(list(uid = "WOS:001")))),
    openalex = list(n = 1L, field = "unique_records_exported",
                    page = list(id = "https://openalex.org/W1")),
    lens = list(n = 1L, field = "unique_records_written",
                page = list(identity = list(lens_id = "0001")))
  )
  src_manifest <- list()
  for (src in names(specs)) {
    spec <- specs[[src]]
    dest <- file.path(run, "sources", src)
    dir.create(dest, recursive = TRUE)
    meta <- list(status = "success")
    meta[[spec$field]] <- spec$n
    jsonlite::write_json(meta, file.path(dest, "manifest.json"),
                         auto_unbox = TRUE)
    if (src %in% c("scopus", "wos")) {
      dir.create(file.path(dest, "raw"))
      jsonlite::write_json(spec$page,
        file.path(dest, "raw", "response_000001.json"),
        auto_unbox = TRUE)
    } else {
      file <- if (src == "openalex") "unique_works.jsonl" else "records.jsonl"
      writeLines(jsonlite::toJSON(spec$page, auto_unbox = TRUE),
                 file.path(dest, file))
    }
    src_manifest[[src]] <- list(status = "success", records = spec$n)
  }
  jsonlite::write_json(list(status = "success", sources = src_manifest),
                       file.path(run, "w00_full_retrieval_manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  m <- consolidate_w00(map)
  data <- jsonlite::fromJSON(m)
  expect_equal(data$record_count, 5)
  expect_equal(data$source_count, 4)
  expect_equal(data$sources$scopus$records, 2)
  expect_equal(data$sources$wos$records, 1)
  expect_false(data$w01_ready)
  expect_identical(consolidate_w00(map), m)
})
