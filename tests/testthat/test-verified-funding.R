# Tests for verified funding endpoints

# -- nva_verified_funding_nfr() tests --

test_that("nva_verified_funding_nfr returns funding data", {
  local_mock_nva(mock_from_fixture("nfr-funding.json"))

  result <- nva_verified_funding_nfr(123456)

  expect_type(result, "list")
  expect_equal(result$id, "https://api.nva.unit.no/verified-funding/nfr/123456")
  expect_equal(result$status, "ACTIVE")
  expect_equal(result$fundingAmount, 5000000)
  expect_equal(result$currency, "NOK")
})

test_that("nva_verified_funding_nfr errors on missing id", {
  expect_error(nva_verified_funding_nfr(), class = "rlang_error")
  expect_error(nva_verified_funding_nfr(NULL), class = "rlang_error")
})

test_that("nva_verified_funding_nfr accepts numeric or character id", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("nfr-funding.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_verified_funding_nfr(123456)
  })
  expect_true(grepl("verified-funding/nfr/123456", captured_req$url))

  with_mock_nva(mock_fn, {
    nva_verified_funding_nfr("123456")
  })
  expect_true(grepl("verified-funding/nfr/123456", captured_req$url))
})

# -- nva_verified_funding_nfr_search() tests --

test_that("nva_verified_funding_nfr_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("nfr-funding-search.json"))

  result <- nva_verified_funding_nfr_search(query = "climate")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "title", "status", "amount"))
})

test_that("nva_verified_funding_nfr_search parses data correctly", {
  local_mock_nva(mock_from_fixture("nfr-funding-search.json"))

  result <- nva_verified_funding_nfr_search(query = "test")

  expect_equal(nrow(result), 2)
  expect_equal(result$title[1], "Climate Research Project")
  expect_equal(result$status[1], "ACTIVE")
  expect_equal(result$amount[1], 5000000)

  expect_equal(result$title[2], "Biodiversity Study")
  expect_equal(result$status[2], "CONCLUDED")
  expect_equal(result$amount[2], 2500000)
})

test_that("nva_verified_funding_nfr_search errors when no parameters", {
  expect_error(
    nva_verified_funding_nfr_search(),
    class = "rlang_error"
  )
})

test_that("nva_verified_funding_nfr_search accepts project_id only", {
  local_mock_nva(mock_from_fixture("nfr-funding-search.json"))

  expect_no_error(nva_verified_funding_nfr_search(nfr_project_id = "123456"))
})

test_that("nva_verified_funding_nfr_search returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_verified_funding_nfr_search(query = "nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "title", "status", "amount"))
})

test_that("nva_verified_funding_nfr_search passes query parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("nfr-funding-search.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_verified_funding_nfr_search(
      query = "climate",
      nfr_project_id = "123456",
      limit = 25,
      page = 2
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$query, "climate")
  expect_equal(parsed$query$projectId, "123456")
  expect_equal(parsed$query$results, "25")
  expect_equal(parsed$query$page, "2")
})
