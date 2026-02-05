# Tests for API error handling

test_that("nva_get handles 404 errors", {
  mock_fn <- function(req) {
    mock_error_response(404L, "Resource not found")
  }

  local_mock_nva(mock_fn)

  expect_error(
    nva_get("cristin/person/nonexistent"),
    class = "httr2_http_404"
  )
})

test_that("nva_get handles 400 errors", {
  mock_fn <- function(req) {
    mock_error_response(400L, "Invalid request parameters")
  }

  local_mock_nva(mock_fn)

  expect_error(
    nva_get("search/resources"),
    class = "httr2_http_400"
  )
})

test_that("nva_get handles 500 errors", {
  mock_fn <- function(req) {
    mock_error_response(500L, "Internal server error")
  }

  local_mock_nva(mock_fn)

  expect_error(
    nva_get("search/resources"),
    class = "httr2_http_500"
  )
})

test_that("nva_get_tibble handles errors gracefully", {
  mock_fn <- function(req) {
    mock_error_response(404L, "Not found")
  }

  local_mock_nva(mock_fn)

  expect_error(
    nva_get_tibble("cristin/organization/invalid"),
    class = "httr2_http_404"
  )
})

test_that("error body extraction works with message field", {
  resp <- httr2::response_json(
    status_code = 404L,
    body = list(message = "Resource not found"),
    url = "https://api.nva.unit.no/test"
  )

  error_msg <- nva_error_body(resp)
  expect_equal(error_msg, "Resource not found")
})

test_that("error body extraction works with error field", {
  resp <- httr2::response_json(
    status_code = 400L,
    body = list(error = "Bad request"),
    url = "https://api.nva.unit.no/test"
  )

  error_msg <- nva_error_body(resp)
  expect_equal(error_msg, "Bad request")
})

test_that("error body extraction works with detail field", {
  resp <- httr2::response_json(
    status_code = 422L,
    body = list(detail = "Validation failed"),
    url = "https://api.nva.unit.no/test"
  )

  error_msg <- nva_error_body(resp)
  expect_equal(error_msg, "Validation failed")
})

test_that("error body returns NULL for unknown format", {
  resp <- httr2::response_json(
    status_code = 500L,
    body = list(unknown = "Something"),
    url = "https://api.nva.unit.no/test"
  )

  error_msg <- nva_error_body(resp)
  expect_null(error_msg)
})

test_that("nva_cristin_person errors include custom message", {
  mock_fn <- function(req) {
    mock_error_response(404L, "Person with ID 99999 not found")
  }

  local_mock_nva(mock_fn)

  expect_error(
    nva_cristin_person(99999),
    "Person with ID 99999 not found"
  )
})

test_that("nva_publication errors include custom message", {
  mock_fn <- function(req) {
    mock_error_response(404L, "Publication not found")
  }

  local_mock_nva(mock_fn)

  expect_error(
    nva_publication("invalid-uuid"),
    "Publication not found"
  )
})
