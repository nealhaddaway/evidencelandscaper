testthat::test_that("full-corpus novelty proposals export as flat offline CSV", {
  path<-testthat::test_path("..","..","R",
                            "ostara-all-fulltext-mechanism-run.R")
  testthat::expect_silent(parse(file=path))
  x<-paste(readLines(path,warn=FALSE),collapse="\n")
  for(token in c(
    "export_ostara_fulltext_candidate_mechanisms <- function(",
    "export_ostara_fulltext_candidate_mechanisms(base)",
    "mechanism_assessments.jsonl",
    "new_novelty.csv",
    "working_definition",
    "closest_approved_mechanism_id",
    "source_quote",
    "source_anchor",
    "source_sha256",
    "for(k in seq_along(z$new_novelty))",
    "nrow(out)"
  ))testthat::expect_match(x,token,fixed=TRUE)
  testthat::expect_true(
    "export_ostara_fulltext_candidate_mechanisms" %in%
      getNamespaceExports("evidencelandscaper"))
})
