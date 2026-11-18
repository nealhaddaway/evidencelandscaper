testthat::test_that("unified insights extraction uses exact user prompt, v5 ontology and source-only pool", {
  p<-testthat::test_path("..","..","R","ostara-fulltext-insights-unified.R")
  testthat::expect_silent(parse(file=p))
  s<-paste(readLines(p,warn=FALSE),collapse="\n")
  for(term in c(
    "run_ostara_all_fulltext_insights <- function(",
    "use_exact_prompt=TRUE",
    "source_pool.csv",
    "pool_manifest.json",
    "nrow(ont)!=26L",
    "file.symlink(",
    "ostara_fulltext_insights.csv",
    "not_yet_validated",
    "insights_assessments.jsonl",
    "sha(prompt_file),sha(ontology_file)",
    "max_records=max_records"
  ))testthat::expect_match(s,term,fixed=TRUE)
  testthat::expect_true("run_ostara_all_fulltext_insights" %in%
    getNamespaceExports("evidencelandscaper"))
})
