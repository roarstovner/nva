# Tests for Cristin keyword and funding source endpoints

# -- nva_cristin_keyword() tests --

test_that("nva_cristin_keyword returns keyword data", {
  local_mock_nva(mock_from_fixture("cristin-keyword.json"))

  result <- nva_cristin_keyword(45678)

  expect_type(result, "list")
  expect_equal(result$id, "https://api.nva.unit.no/cristin/keyword/45678")
  expect_equal(result$label, "machine learning")
  expect_equal(result$language, "en")
})

test_that("nva_cristin_keyword errors on missing id", {
  expect_error(nva_cristin_keyword(), class = "rlang_error")
  expect_error(nva_cristin_keyword(NULL), class = "rlang_error")
})

test_that("nva_cristin_keyword accepts numeric or character id", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-keyword.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_keyword(45678)
  })
  expect_true(grepl("cristin/keyword/45678", captured_req$url))

  with_mock_nva(mock_fn, {
    nva_cristin_keyword("45678")
  })
  expect_true(grepl("cristin/keyword/45678", captured_req$url))
})

# -- nva_cristin_keyword_search() tests --

test_that("nva_cristin_keyword_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-keyword-search.json"))

  result <- nva_cristin_keyword_search(query = "machine")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "label", "language"))
})

test_that("nva_cristin_keyword_search parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-keyword-search.json"))

  result <- nva_cristin_keyword_search(query = "machine")

  expect_equal(nrow(result), 2)
  expect_equal(result$id[1], "45678")
  expect_equal(result$label[1], "machine learning")
  expect_equal(result$language[1], "en")

  expect_equal(result$id[2], "45679")
  expect_equal(result$label[2], "maskinlaering")
  expect_equal(result$language[2], "nb")
})

test_that("nva_cristin_keyword_search errors when query is missing", {
  expect_error(
    nva_cristin_keyword_search(),
    class = "rlang_error"
  )
})

test_that("nva_cristin_keyword_search returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_keyword_search(query = "nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "label", "language"))
})

test_that("nva_cristin_keyword_search passes query parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-keyword-search.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_keyword_search(
      query = "learning",
      language = "en",
      limit = 50,
      page = 2
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$name, "learning")
  expect_equal(parsed$query$language, "en")
  expect_equal(parsed$query$results, "50")
  expect_equal(parsed$query$page, "2")
})

# -- nva_cristin_funding_source() tests --

test_that("nva_cristin_funding_source returns funding source data", {
  local_mock_nva(mock_from_fixture("cristin-funding-source.json"))

  result <- nva_cristin_funding_source("NFR")

  expect_type(result, "list")
  expect_equal(result$identifier, "NFR")
  expect_equal(result$acronym, "NFR")
  expect_type(result$name, "list")
})

test_that("nva_cristin_funding_source errors on missing id", {
  expect_error(nva_cristin_funding_source(), class = "rlang_error")
  expect_error(nva_cristin_funding_source(NULL), class = "rlang_error")
})

test_that("nva_cristin_funding_source accepts character id", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-funding-source.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_funding_source("NFR")
  })
  expect_true(grepl("cristin/funding-sources/NFR", captured_req$url))
})

# -- nva_cristin_funding_source_search() tests --

test_that("nva_cristin_funding_source_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-funding-sources-search.json"))

  result <- nva_cristin_funding_source_search()

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "name", "acronym"))
})

test_that("nva_cristin_funding_source_search parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-funding-sources-search.json"))

  result <- nva_cristin_funding_source_search()

  expect_equal(nrow(result), 3)
  expect_equal(result$id[1], "NFR")
  expect_equal(result$acronym[1], "NFR")

  expect_equal(result$id[2], "EU")
  expect_equal(result$acronym[2], "EU")

  expect_equal(result$id[3], "PRIVAT")
  expect_equal(result$acronym[3], "PRIV")
})

test_that("nva_cristin_funding_source_search returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_funding_source_search()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "acronym"))
})

test_that("nva_cristin_funding_source_search passes pagination correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-funding-sources-search.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_funding_source_search(limit = 50, page = 2)
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$results, "50")
  expect_equal(parsed$query$page, "2")
})
