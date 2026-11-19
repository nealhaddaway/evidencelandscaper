testthat::test_that("complete included bibliography exporter validates both final cohorts", {
  path<-testthat::test_path("..","..","R",
                            "ostara-included-bibliography-export.R")
  testthat::expect_silent(parse(file=path))
  src<-paste(readLines(path,warn=FALSE),collapse="\n")
  for(word in c(
    "export_ostara_all_included_bibliography <- function(",
    "first10-all-included-",
    "remaining90-all-included-",
    "workflow04_effective_screening_layer.jsonl",
    'decisions=="retain"',
    "selected_record_ids",
    "canonical.jsonl",
    "canonical_w02_sha256",
    "anyDuplicated(all$record_id)",
    "OSTARA_all_included_bibliographic_records.csv",
    "export_manifest.json",
    "validated_exactly_against_W04=TRUE",
    "writeBin(as.raw(c(239L,187L,191L)),con)"
  ))testthat::expect_match(src,word,fixed=TRUE)
  testthat::expect_false(grepl("req_perform(",src,fixed=TRUE))
  testthat::expect_true("export_ostara_all_included_bibliography" %in%
                        getNamespaceExports("evidencelandscaper"))
})
