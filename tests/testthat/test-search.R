# Tests for nva_search()

test_that("nva_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_search("climate")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))
})

test_that("nva_search parses publication data correctly", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_search("climate")

  expect_equal(nrow(result), 2)

  # First publication
  expect_equal(result$identifier[1], "01907b5e-1234-5678-abcd-123456789abc")
  expect_equal(result$title[1], "Climate Change Impacts on Arctic Ecosystems")
  expect_equal(result$type[1], "AcademicArticle")
  expect_equal(result$year[1], 2024L)

  expect_equal(result$status[1], "PUBLISHED")

  # Contributors - first pub has contributorsPreview
  expect_equal(result$contributors[[1]], c("Ola Nordmann", "Kari Hansen"))

  # Institutions
  expect_equal(result$institutions[[1]], "University of Oslo")
})

test_that("nva_search handles contributors vs contributorsPreview", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_search("climate")

  # Second publication uses contributors (not contributorsPreview)
  expect_equal(result$contributors[[2]], "Per Olsen")
})

test_that("nva_search handles multiple institutions", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_search("climate")

  # Second publication has two institutions
  expect_length(result$institutions[[2]], 2)
  expect_equal(result$institutions[[2]][1], "OsloMet")  # nb fallback
  expect_equal(result$institutions[[2]][2], "Norwegian Institute of Public Health")
})

test_that("nva_search returns empty tibble with correct schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_search("nonexistent query xyz")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))

  # Check column types
  expect_type(result$identifier, "character")
  expect_type(result$title, "character")
  expect_type(result$year, "integer")
  expect_type(result$contributors, "list")
})

test_that("nva_search validates sort parameter", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  expect_error(nva_search("test", sort = "invalid"),
               class = "rlang_error")
})

test_that("nva_search accepts valid sort options", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  expect_no_error(nva_search("test", sort = "relevance"))
  expect_no_error(nva_search("test", sort = "modifiedDate"))
  expect_no_error(nva_search("test", sort = "createdDate"))
  expect_no_error(nva_search("test", sort = "publishedDate"))
})

test_that("nva_search passes query parameters correctly", {
  # Capture the request to verify parameters
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search("climate change",
               limit = 25,
               offset = 10,
               organization = "185",
               year = "2024",
               type = "AcademicArticle")
  })

  # Parse URL to check query params
  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$query, "climate change")
  expect_equal(parsed$query$results, "25")
  expect_equal(parsed$query$from, "10")
  expect_equal(parsed$query$institution, "185")
  expect_equal(parsed$query$year, "2024")
  expect_equal(parsed$query$instanceType, "AcademicArticle")
})
