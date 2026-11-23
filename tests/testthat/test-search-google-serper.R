test_that("Serper pagination exports unique target URLs and provenance", {
  calls <- integer()
  mock <- function(page, query, num, gl, hl, api_key) {
    calls <<- c(calls, page)
    if (page == 1L) {
      return(list(organic = list(
        list(position = 1, title = "First", link = "https://example.org/a.pdf",
             snippet = "A"),
        list(position = 2, title = "Second", link = "https://example.org/b.pdf",
             snippet = "B")
      )))
    }
    if (page == 2L) {
      return(list(organic = list(
        list(position = 3, title = "Repeated", link = "https://example.org/b.pdf"),
        list(position = 4, title = "Third", link = "https://example.org/c.pdf")
      )))
    }
    list(organic = list())
  }
  path <- tempfile(fileext = ".csv")
  result <- search_google_serper(
    '"collective action" oil and gas filetype:PDF',
    max_results = 6, results_per_page = 2, output_csv = path,
    pause_seconds = 0, .fetch_page = mock
  )
  expect_identical(calls, c(1L, 2L, 3L))
  expect_equal(nrow(result), 3L)
  expect_equal(result$url, paste0("https://example.org/",
                                   c("a", "b", "c"), ".pdf"))
  expect_equal(result$page, c(1L, 1L, 2L))
  expect_equal(nrow(utils::read.csv(path)), 3L)
})

test_that("Serper stops when a page repeats URLs and rejects invalid input", {
  mock <- function(page, query, num, gl, hl, api_key) {
    list(organic = list(list(title = "Same", link = "https://example.org/a.pdf")))
  }
  output <- tempfile(fileext = ".csv")
  result <- search_google_serper(
    "test", max_results = 30, output_csv = output,
    pause_seconds = 0, .fetch_page = mock
  )
  expect_equal(nrow(result), 1L)
  expect_error(search_google_serper("", .fetch_page = mock), "non-empty")
  expect_error(search_google_serper("test", results_per_page = 11,
                                    .fetch_page = mock), "between 1 and 10")
  expect_error(search_google_serper("test", api_key = "",
                                    output_csv = output), "SERPER_API_KEY")
})
