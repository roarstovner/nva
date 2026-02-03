# Mock response helpers for nva package tests
# These utilities use httr2's built-in mocking capabilities

#' Load a JSON fixture file
#'
#' @param filename Name of the fixture file (without path)
#' @return Parsed JSON as a list
load_fixture <- function(filename) {

  path <- testthat::test_path("fixtures", filename)
  jsonlite::read_json(path)
}

#' Create a mock JSON response
#'
#' @param body A list that will be serialized to JSON
#' @param status_code HTTP status code (default 200)
#' @param url URL for the response (default NVA API base)
#' @return An httr2 response object
mock_json_response <- function(body, status_code = 200L,
                                url = "https://api.nva.unit.no") {
  httr2::response_json(
    status_code = as.integer(status_code),
    url = url,
    method = "GET",
    body = body
  )
}

#' Create a mock error response
#'
#' @param status_code HTTP status code
#' @param message Error message
#' @param url URL for the response
#' @return An httr2 response object
mock_error_response <- function(status_code, message = "Error",
                                 url = "https://api.nva.unit.no") {

  httr2::response_json(
    status_code = as.integer(status_code),
    url = url,
    method = "GET",
    body = list(message = message)
  )
}

#' Create a mock function from a fixture file
#'
#' @param fixture_name Name of the fixture file to return
#' @param status_code HTTP status code (default 200)
#' @return A mock function suitable for with_mocked_responses()
mock_from_fixture <- function(fixture_name, status_code = 200L) {
  function(req) {
    body <- load_fixture(fixture_name)
    mock_json_response(body, status_code = status_code, url = req$url)
  }
}

#' Create a mock function that matches URL patterns to fixtures
#'
#' @param ... Named arguments where names are URL patterns (regex) and
#'   values are fixture filenames
#' @return A mock function suitable for with_mocked_responses()
mock_by_url <- function(...) {
  mappings <- list(...)
  function(req) {
    url <- req$url
    for (pattern in names(mappings)) {
      if (grepl(pattern, url)) {
        body <- load_fixture(mappings[[pattern]])
        return(mock_json_response(body, url = url))
      }
    }
    # Return 404 for unmatched URLs
    mock_error_response(404L, "Not found", url = url)
  }
}

#' Run code with a mocked NVA API response
#'
#' Convenience wrapper around httr2::with_mocked_responses()
#'
#' @param mock A mock function or list of responses
#' @param code Code to execute
#' @return Result of evaluating code
with_mock_nva <- function(mock, code) {
  httr2::with_mocked_responses(mock, code)
}

#' Locally mock NVA API responses for the duration of a test
#'
#' @param mock A mock function or list of responses
#' @param env Environment for scoping (default: caller's environment)
local_mock_nva <- function(mock, env = parent.frame()) {
  httr2::local_mocked_responses(mock, env = env)
}

#' Create standard empty search result
#'
#' @param total_size Total number of results (default 0)
#' @return A list representing an empty search response
empty_search_result <- function(total_size = 0L) {
  list(
    hits = list(),
    totalHits = as.integer(total_size),
    size = 0L,
    offset = 0L
  )
}

#' Create a search result with items
#'
#' @param hits List of hit items
#' @param total_size Total number of results
#' @param offset Starting offset
#' @return A list representing a search response
search_result <- function(hits, total_size = NULL, offset = 0L) {
  if (is.null(total_size)) {
    total_size <- length(hits)
  }
  list(
    hits = hits,
    totalHits = as.integer(total_size),
    size = length(hits),
    offset = as.integer(offset)
  )
}
