#' Search Google via Serper and export paginated results
#'
#' Retrieves organic Google search results using the Serper.dev API, preserving
#' query, page, ranking, date, title, destination URL, and snippet. Stops at the
#' requested limit or when no new destination URLs are returned. The result is
#' deduplicated by exact target URL, retaining the first occurrence.
#'
#' A request for 500 results is a ceiling, not a guarantee that Google will
#' expose 500 distinct results. Only organic results are captured; PDF validity
#' and actual document downloads are not checked.
#'
#' @param query A single non-empty Google search string.
#' @param max_results Maximum number of organic results to request (default 500).
#' @param results_per_page Number of results requested per page (1 to 10).
#' @param output_csv Destination CSV path; defaults to the working directory.
#' @param api_key Serper API key, normally loaded from SERPER_API_KEY.
#' @param gl Two-letter Google country code (default "gb").
#' @param hl Google interface language (default "en").
#' @param pause_seconds Delay between requests (default 0.5).
#' @param .fetch_page Optional function(page, query, num, gl, hl, api_key) for
#'   deterministic offline testing; normal users should leave this as NULL.
#'
#' @return Invisibly, a data.frame of unique retrieved URLs. The output CSV
#'   includes query, search_date, page, position, title, url, and snippet.
#' @export
#' @examples
#' \dontrun{
#' search_google_serper(
#'   '"collective action" oil and gas filetype:PDF',
#'   max_results = 500
#' )
#' }
search_google_serper <- function(
    query,
    max_results = 500L,
    results_per_page = 10L,
    output_csv = file.path(getwd(), "google_serper_results.csv"),
    api_key = Sys.getenv("SERPER_API_KEY"),
    gl = "gb",
    hl = "en",
    pause_seconds = 0.5,
    .fetch_page = NULL) {

  if (!is.character(query) || length(query) != 1L ||
      is.na(query) || !nzchar(trimws(query))) {
    stop("query must be one non-empty string.", call. = FALSE)
  }
  if (!is.numeric(max_results) || length(max_results) != 1L ||
      is.na(max_results) || !is.finite(max_results) ||
      max_results < 1 || max_results != floor(max_results)) {
    stop("max_results must be a positive whole number.", call. = FALSE)
  }
  if (!is.numeric(results_per_page) || length(results_per_page) != 1L ||
      is.na(results_per_page) || !is.finite(results_per_page) ||
      results_per_page < 1 || results_per_page > 10 ||
      results_per_page != floor(results_per_page)) {
    stop("results_per_page must be a whole number between 1 and 10.",
         call. = FALSE)
  }
  if (!is.character(output_csv) || length(output_csv) != 1L ||
      is.na(output_csv) || !nzchar(output_csv)) {
    stop("output_csv must be a non-empty file path.", call. = FALSE)
  }
  if (!is.numeric(pause_seconds) || length(pause_seconds) != 1L ||
      is.na(pause_seconds) || pause_seconds < 0 || !is.finite(pause_seconds)) {
    stop("pause_seconds must be a non-negative number.", call. = FALSE)
  }
  if (is.null(.fetch_page) && (!is.character(api_key) ||
      length(api_key) != 1L || is.na(api_key) || !nzchar(trimws(api_key)))) {
    stop("SERPER_API_KEY is missing. Set it in the current R session.",
         call. = FALSE)
  }
  if (!is.null(.fetch_page) && !is.function(.fetch_page)) {
    stop(".fetch_page must be a function.", call. = FALSE)
  }

  if (is.null(.fetch_page)) {
    .fetch_page <- function(page, query, num, gl, hl, api_key) {
      response <- httr2::request("https://google.serper.dev/search") |>
        httr2::req_headers("X-API-KEY" = api_key) |>
        httr2::req_body_json(list(
          q = query, page = page, num = num, gl = gl, hl = hl,
          autocorrect = FALSE
        )) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_perform()
      httr2::resp_body_json(response, simplifyVector = FALSE)
    }
  }

  field <- function(item, name, default = "") {
    x <- item[[name]]
    if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
      return(default)
    }
    as.character(x[[1L]])
  }

  seen <- character()
  pages <- list()
  max_pages <- ceiling(max_results / results_per_page)
  search_date <- format(Sys.Date(), "%Y-%m-%d")

  for (page in seq_len(max_pages)) {
    requested <- min(results_per_page,
                     max_results - (page - 1L) * results_per_page)
    message("Serper: requesting page ", page, " of ", max_pages)

    # Fail explicitly if an API request fails. Do not label a partial
    # collection as a completed systematic search.
    data <- .fetch_page(
      page = page, query = query, num = requested,
      gl = gl, hl = hl, api_key = api_key
    )
    organic <- data$organic
    if (is.null(organic) || length(organic) == 0L) break

    rows <- lapply(seq_along(organic), function(i) {
      item <- organic[[i]]
      pos <- suppressWarnings(as.integer(field(item, "position", NA_character_)))
      if (is.na(pos)) pos <- (page - 1L) * results_per_page + i
      data.frame(
        query = query,
        search_date = search_date,
        page = as.integer(page),
        position = pos,
        title = field(item, "title"),
        url = field(item, "link"),
        snippet = field(item, "snippet"),
        stringsAsFactors = FALSE
      )
    })
    page_df <- do.call(rbind, rows)
    page_df <- page_df[nzchar(page_df$url), , drop = FALSE]
    if (nrow(page_df) == 0L) break

    fresh <- !duplicated(page_df$url) & !(page_df$url %in% seen)
    if (!any(fresh)) {
      message("Serper: no new target URLs on page ", page, "; stopping.")
      break
    }
    pages[[length(pages) + 1L]] <- page_df
    seen <- union(seen, page_df$url)

    if (page < max_pages && pause_seconds > 0) Sys.sleep(pause_seconds)
  }

  if (!length(pages)) {
    stop("Serper returned no organic results; no CSV was written.",
         call. = FALSE)
  }

  raw <- do.call(rbind, pages)
  results <- raw[!duplicated(raw$url), , drop = FALSE]
  results <- head(results, max_results)
  rownames(results) <- NULL

  dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(results, output_csv, row.names = FALSE,
                   fileEncoding = "UTF-8")

  message(
    "Serper complete: ", nrow(raw), " organic rows; ",
    nrow(results), " unique URLs; ",
    length(pages), " result pages with new URLs. Saved: ",
    normalizePath(output_csv, mustWork = TRUE)
  )
  invisible(results)
}
