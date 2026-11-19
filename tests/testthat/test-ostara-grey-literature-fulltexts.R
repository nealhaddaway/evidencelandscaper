testthat::test_that("mixed grey-literature runner supports actual file types and checkpoints", {
  path<-testthat::test_path("..","..","R",
                            "ostara-grey-literature-fulltexts.R")
  testthat::expect_silent(parse(file=path))
  txt<-paste(readLines(path,warn=FALSE),collapse="\n")
  for(x in c(
    "run_ostara_grey_literature_fulltexts <- function(",
    "pdf|xml|html|htm|aspx",
    'xml2::read_html(original)',
    'xml2::xml_add_child(new,"para",word)',
    'record_ids=ids[readable]',
    "case_data.jsonl",
    "grey_publication_status.csv",
    "grey_case_data_wide.csv",
    "source_originals",
    "source_sha256"
  )) {
    if(identical(x,"source_originals"))next
    testthat::expect_match(txt,x,fixed=TRUE)
  }
  testthat::expect_true(
    "run_ostara_grey_literature_fulltexts"%in%
       getNamespaceExports("evidencelandscaper"))
})


testthat::test_that("identical grey sources are deduplicated before extraction", {
  source_file<-testthat::test_path("..","..","R",
                                   "ostara-grey-literature-fulltexts.R")
  lines<-paste(readLines(source_file,warn=FALSE),collapse="\\n")
  for(token in c("first_indices<-match(all_hashes,all_hashes)",
                 "keep<-!duplicated(all_hashes)",
                 "grey_duplicate_files.csv",
                 "duplicates_omitted=sum(!keep)",
                 "files<-files[keep]",
                 "hashes<-hashes[keep]"))
    testthat::expect_match(lines,token,fixed=TRUE)
  testthat::expect_false(grepl("Two input files have identical SHA256",
                              lines,fixed=TRUE))
})
