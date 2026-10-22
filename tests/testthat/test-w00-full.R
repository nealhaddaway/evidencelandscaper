test_that("Full W00 requires a valid plan before API calls", {
  expect_error(run_w00_full(list()), "valid map name or W00 plan")
})

test_that("Full W00 includes original full-harvest adapters", {
  scripts <- c("workflow_00_lens_orchestrated_search.R",
               "workflow_00c_openalex_ingestion.R",
               "workflow_00b_scopus_ingestion.R",
               "workflow_00e_wos_starter_ingestion.R",
               "workflow_00g_ebsco_ingestion.R",
               "workflow_00d_agricola_ingestion.R",
               "workflow_00f_europe_pmc_source_ingestion.R")
  expect_true(all(nzchar(vapply(scripts, function(s)
    system.file("w00", s, package="evidencelandscaper"), character(1)))))
})
