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
  expect_no_error(nva_search("test", sort = "title"))
  expect_no_error(nva_search("test", sort = "category"))
  expect_no_error(nva_search("test", sort = "publicationDate"))
  expect_no_error(nva_search("test", sort = "unitId"))
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
  expect_equal(parsed$query$publication_year, "2024")
  expect_equal(parsed$query$instanceType, "AcademicArticle")
})

test_that("nva_search passes year range correctly", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    httr2::response_json(body = list(hits = list(), totalHits = 0))
  }

  with_mock_nva(mock_fn, {
    nva_search("test", year = "2020,2024")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$publication_year, "2020,2024")
})

# Tests for nva_search_aggregations()

test_that("nva_search_aggregations returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_search_aggregations("climate")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("aggregation", "key", "label", "count"))
})

test_that("nva_search_aggregations includes all aggregation fields", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_search_aggregations("climate")

  agg_fields <- unique(result$aggregation)
  expect_true("type" %in% agg_fields)
  expect_true("topLevelOrganization" %in% agg_fields)
  expect_true("files" %in% agg_fields)
})

test_that("nva_search_aggregations parses counts correctly", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_search_aggregations("climate")

  type_rows <- result[result$aggregation == "type", ]
  expect_equal(nrow(type_rows), 3)
  expect_equal(type_rows$key[1], "AcademicArticle")
  expect_equal(type_rows$count[1], 80L)
})

test_that("nva_search_aggregations uses labels when available", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_search_aggregations("climate")

  org_rows <- result[result$aggregation == "topLevelOrganization", ]
  # Has labels -> uses nva_get_label (prefers English)
  expect_equal(org_rows$label[1], "University of Oslo")
  expect_equal(org_rows$label[2], "Oslo Metropolitan University")
})

test_that("nva_search_aggregations falls back to key when no labels", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_search_aggregations("climate")

  type_rows <- result[result$aggregation == "type", ]
  # No labels -> label equals key
  expect_equal(type_rows$label[1], "AcademicArticle")

  files_rows <- result[result$aggregation == "files", ]
  expect_equal(files_rows$label[1], "hasPublicFiles")
})

test_that("nva_search_aggregations passes aggregation=all to API", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search_aggregations("test")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$aggregation, "all")
})

test_that("nva_search_aggregations passes filter parameters to API", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search_aggregations("climate", organization = "185", year = "2024",
                            type = "AcademicArticle")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$institution, "185")
  expect_equal(parsed$query$publication_year, "2024")
  expect_equal(parsed$query$instanceType, "AcademicArticle")
})

test_that("nva_search_aggregations returns empty tibble with correct schema for no aggregations", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_search_aggregations("nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("aggregation", "key", "label", "count"))
  expect_type(result$aggregation, "character")
  expect_type(result$count, "integer")
})

# Tests for new search parameters

test_that("nva_search passes advanced filter parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search("test",
               contributor = "12345",
               contributor_name = "Nordmann",
               doi = "10.1371/journal.pone",
               title = "climate",
               abstract = "warming",
               tags = "arctic",
               journal = "Nature",
               publisher = "Springer",
               issn = "1234-5678",
               isbn = "978-3-16-148410-0",
               license = "CC-BY-4.0",
               files = "hasPublicFiles",
               funding_source = "NFR",
               funding_identifier = "123456",
               scientific_value = "LevelTwo",
               scientific_index_status = "Reported",
               created_since = "2024-01-01",
               modified_since = "2024-06-01",
               published_since = "2024-01-01",
               status = "PUBLISHED",
               orcid = "0000-0001-2345-6789",
               project = "54321",
               unit = "185.90.0.0")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$contributor, "12345")
  expect_equal(parsed$query$contributorName, "Nordmann")
  expect_equal(parsed$query$doi, "10.1371/journal.pone")
  expect_equal(parsed$query$title, "climate")
  expect_equal(parsed$query$abstract, "warming")
  expect_equal(parsed$query$tags, "arctic")
  expect_equal(parsed$query$journal, "Nature")
  expect_equal(parsed$query$publisher, "Springer")
  expect_equal(parsed$query$issn, "1234-5678")
  expect_equal(parsed$query$isbn, "978-3-16-148410-0")
  expect_equal(parsed$query$license, "CC-BY-4.0")
  expect_equal(parsed$query$files, "hasPublicFiles")
  expect_equal(parsed$query$fundingSource, "NFR")
  expect_equal(parsed$query$fundingIdentifier, "123456")
  expect_equal(parsed$query$scientificValue, "LevelTwo")
  expect_equal(parsed$query$scientificIndexStatus, "Reported")
  expect_equal(parsed$query$createdSince, "2024-01-01")
  expect_equal(parsed$query$modifiedSince, "2024-06-01")
  expect_equal(parsed$query$publishedSince, "2024-01-01")
  expect_equal(parsed$query$status, "PUBLISHED")
  expect_equal(parsed$query$orcid, "0000-0001-2345-6789")
  expect_equal(parsed$query$project, "54321")
  expect_equal(parsed$query$unit, "185.90.0.0")
})

test_that("nva_search passes ... params directly to API", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search("test", series = "MySeries", customParam = "value")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$series, "MySeries")
  expect_equal(parsed$query$customParam, "value")
})

test_that("nva_search omits NULL parameters from query", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search("test", doi = "10.1234")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$doi, "10.1234")
  expect_null(parsed$query$contributor)
  expect_null(parsed$query$orcid)
  expect_null(parsed$query$institution)
})

test_that("nva_search_aggregations passes advanced filter parameters", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search_aggregations("test",
                            doi = "10.1371",
                            scientific_value = "LevelTwo",
                            files = "hasPublicFiles")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$doi, "10.1371")
  expect_equal(parsed$query$scientificValue, "LevelTwo")
  expect_equal(parsed$query$files, "hasPublicFiles")
  expect_equal(parsed$query$aggregation, "all")
})

test_that("nva_search_aggregations passes ... params directly to API", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_search_aggregations("test", series = "MySeries")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$series, "MySeries")
})
