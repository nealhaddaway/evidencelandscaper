test_that("W01 preparation rejects missing completed full W00", {
  root <- tempfile("maps-")
  dir.create(root)
  map <- create_map("empty_w01", root)
  expect_error(prepare_w01(map), "No successful full W00")
})

test_that("W01-main adapters are shipped", {
  scripts <- c("workflow_01s_scopus_sidecar_adapter.R",
    "workflow_01u_openalex_sidecar_adapter.R",
    "workflow_01w_wos_starter_sidecar_adapter.R",
    "workflow_01x_europe_pmc_sidecar_adapter.R",
    "workflow_01_generic_ris_to_sidecar.R",
    "workflow_01_deduplication_build_union.R",
    "workflow_01_deduplication_incremental_candidates.R")
  expect_true(all(vapply(scripts, function(s)
    nzchar(system.file("w01", s, package="evidencelandscaper")),
    logical(1))))
})
