# Tests to verify the testing infrastructure is working

test_that("fixture loading works", {
  fixture <- load_fixture("empty-search.json")
  expect_type(fixture, "list")
  expect_equal(fixture$totalHits, 0)
  expect_length(fixture$hits, 0)
})

test_that("mock_json_response creates valid response", {
  body <- list(message = "test")
  resp <- mock_json_response(body, status_code = 200L)

  expect_s3_class(resp, "httr2_response")
  expect_equal(httr2::resp_status(resp), 200L)

  parsed <- httr2::resp_body_json(resp)
  expect_equal(parsed$message, "test")
})

test_that("mock_error_response creates error response", {
  resp <- mock_error_response(404L, "Not found")

  expect_s3_class(resp, "httr2_response")
  expect_equal(httr2::resp_status(resp), 404L)
})

test_that("empty_search_result returns correct structure", {
  result <- empty_search_result()
  expect_type(result, "list")
  expect_equal(result$totalHits, 0L)
  expect_length(result$hits, 0)
})

test_that("search_result creates correct structure", {
  hits <- list(list(id = 1), list(id = 2))
  result <- search_result(hits, total_size = 10)

  expect_equal(result$totalHits, 10L)
  expect_equal(result$size, 2)
  expect_length(result$hits, 2)
})

test_that("with_mock_nva allows mocking API calls", {
  mock_fn <- function(req) {
    mock_json_response(list(test = TRUE))
  }

  result <- with_mock_nva(mock_fn, {
    # Make a request that would normally go to the API
    req <- httr2::request("https://api.nva.unit.no/test")
    resp <- httr2::req_perform(req)
    httr2::resp_body_json(resp)
  })

  expect_true(result$test)
})
