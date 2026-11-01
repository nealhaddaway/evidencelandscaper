# Internal W08 publication status and candidate-count validation.
# Unclear identifies potentially distinguishable candidates, including multiple
# candidates from one publication; it does not confirm their eligibility.
.w08_validate_case_count <- function(status, cases, record_id = "") {
  allowed <- c("specific_case", "multiple_cases",
               "no_identifiable_case", "unclear")
  if(!is.character(status) || length(status) != 1L ||
     is.na(status) || !status %in% allowed)
    stop("W08 invalid case status: ", record_id)
  if(!is.list(cases))
    stop("W08 cases must be an array: ", record_id)
  k <- length(cases)
  if((status == "specific_case" && k != 1L) ||
     (status == "multiple_cases" && k < 2L) ||
     (status == "no_identifiable_case" && k != 0L))
    stop("W08 status/case-count inconsistency: ", record_id)
  # status == "unclear" permits zero, one, or multiple candidate cases.
  invisible(TRUE)
}
