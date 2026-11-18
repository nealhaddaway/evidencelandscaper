testthat::test_that("grey PDF collection reuses named local PDFs and checkpoints", {
  dir<-tempfile("ostara-grey-local-")
  dir.create(dir)
  on.exit(unlink(dir,recursive=TRUE),add=TRUE)
  pdf<-file.path(dir,"Previously Saved.pdf")
  writeBin(charToRaw("%PDF-1.4\n% test document\n"),pdf)
  log<-data.frame("Case"="Demonstration","Title"="A report with a title",
    "Link"="","Name of PDF as saved"="Previously Saved.pdf",
    "PDF link"="",check.names=FALSE)
  input<-file.path(dir,"Grey literature database - Evidence log.csv")
  utils::write.csv(log,input,row.names=FALSE)
  first<-download_ostara_grey_literature_pdfs(csv_file=input)
  testthat::expect_equal(first$saved,1L)
  testthat::expect_true(file.exists(first$csv))
  result<-utils::read.csv(first$csv,stringsAsFactors=FALSE,
                           check.names=FALSE,fileEncoding="UTF-8-BOM")
  testthat::expect_identical(result[["Download source"]][[1L]],"local_pdf")
  testthat::expect_match(result[["Downloaded PDF filename"]][[1L]],
                         "^0001_a_report_with_a_title\\.pdf$")
  testthat::expect_true(file.exists(file.path(first$pdf_directory,
                     result[["Downloaded PDF filename"]][[1L]])))
  second<-download_ostara_grey_literature_pdfs(csv_file=input)
  testthat::expect_equal(second$saved,1L)
  testthat::expect_identical(second$csv,first$csv)
  testthat::expect_false("Downloaded PDF filename"%in%names(log))
})

testthat::test_that("grey PDF function has one-file-picker and strict link priority", {
  p<-testthat::test_path("..","..","R",
                         "ostara-grey-literature-downloads.R")
  testthat::expect_silent(parse(file=p))
  txt<-paste(readLines(p,warn=FALSE),collapse="\n")
  testthat::expect_match(txt,"file.choose()",fixed=TRUE)
  testthat::expect_match(txt,'for(field in c("PDF link","Link"))',fixed=TRUE)
  testthat::expect_match(txt,'"Name of PDF as saved"',fixed=TRUE)
  testthat::expect_match(txt,'"Downloaded PDF filename"',fixed=TRUE)
  testthat::expect_match(txt,'"needs_review"',fixed=TRUE)
  testthat::expect_true("download_ostara_grey_literature_pdfs" %in%
                        getNamespaceExports("evidencelandscaper"))
})
