# Tests for nva_publication_search()

test_that("nva_publication_searchreturns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_publication_search("climate")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions", "doi"))
})

test_that("nva_publication_searchparses publication data correctly", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_publication_search("climate")

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

  # DOI
  expect_equal(result$doi[1], "https://doi.org/10.1234/arctic.2024")
})

test_that("nva_publication_searchhandles contributors vs contributorsPreview", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_publication_search("climate")

  # Second publication uses contributors (not contributorsPreview)
  expect_equal(result$contributors[[2]], "Per Olsen")
})

test_that("nva_publication_searchhandles multiple institutions", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_publication_search("climate")

  # Second publication has two institutions
  expect_length(result$institutions[[2]], 2)
  expect_equal(result$institutions[[2]][1], "OsloMet")  # nb fallback
  expect_equal(result$institutions[[2]][2], "Norwegian Institute of Public Health")
})

test_that("nva_publication_searchreturns empty tibble with correct schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_publication_search("nonexistent query xyz")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions", "doi"))

  # Check column types
  expect_type(result$identifier, "character")
  expect_type(result$title, "character")
  expect_type(result$year, "integer")
  expect_type(result$contributors, "list")
  expect_type(result$doi, "character")
})

test_that("nva_publication_searchvalidates limit parameter", {
  expect_error(nva_publication_search("test", limit = 0), class = "rlang_error")
  expect_error(nva_publication_search("test", limit = 101), class = "rlang_error")
  expect_error(nva_publication_search("test", limit = -1), class = "rlang_error")
})

test_that("nva_publication_searchvalidates sort parameter", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  expect_error(nva_publication_search("test", sort = "invalid"),
               class = "rlang_error")
})

test_that("nva_publication_searchaccepts valid sort options", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  expect_no_error(nva_publication_search("test", sort = "relevance"))
  expect_no_error(nva_publication_search("test", sort = "modifiedDate"))
  expect_no_error(nva_publication_search("test", sort = "createdDate"))
  expect_no_error(nva_publication_search("test", sort = "publishedDate"))
  expect_no_error(nva_publication_search("test", sort = "title"))
  expect_no_error(nva_publication_search("test", sort = "category"))
  expect_no_error(nva_publication_search("test", sort = "publicationDate"))
  expect_no_error(nva_publication_search("test", sort = "unitId"))
})

test_that("nva_publication_searchvalidates sort_order parameter", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  expect_error(nva_publication_search("test", sort_order = "invalid"),
               class = "rlang_error")
})

test_that("nva_publication_searchaccepts valid sort_order options", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  expect_no_error(nva_publication_search("test", sort = "modifiedDate", sort_order = "asc"))
  expect_no_error(nva_publication_search("test", sort = "modifiedDate", sort_order = "desc"))
})

test_that("nva_publication_searchapplies sort_order to sort key", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  # Test ascending order
  with_mock_nva(mock_fn, {
    nva_publication_search("test", sort = "modifiedDate", sort_order = "asc")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$sort, "modifiedDate:asc")

  # Test descending order (default)
  with_mock_nva(mock_fn, {
    nva_publication_search("test", sort = "publicationDate", sort_order = "desc")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$sort, "publicationDate:desc")
})

test_that("nva_publication_searchdoesn't apply sort_order to relevance", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test", sort = "relevance", sort_order = "asc")
  })

  parsed <- httr2::url_parse(captured_req$url)
  # relevance should not have :asc suffix
  expect_equal(parsed$query$sort, "relevance")
})

test_that("nva_publication_searchdefaults to descending order", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  # When sort_order not specified, should default to "desc"
  with_mock_nva(mock_fn, {
    nva_publication_search("test", sort = "modifiedDate")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$sort, "modifiedDate:desc")
})

test_that("nva_publication_searchpasses query parameters correctly", {
  # Capture the request to verify parameters
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("climate change",
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

test_that("nva_publication_searchpasses year range correctly", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    httr2::response_json(body = list(hits = list(), totalHits = 0))
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test", year = "2020,2024")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$publication_year, "2020,2024")
})

# Tests for nva_publication_search_aggregations()

test_that("nva_publication_search_aggregationsvalidates limit parameter", {
  expect_error(nva_publication_search_aggregations("test", limit = 0), class = "rlang_error")
  expect_error(nva_publication_search_aggregations("test", limit = 101), class = "rlang_error")
  expect_error(nva_publication_search_aggregations("test", limit = -1), class = "rlang_error")
})

test_that("nva_publication_search_aggregationsreturns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_publication_search_aggregations("climate")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("aggregation", "key", "label", "count"))
})

test_that("nva_publication_search_aggregationsincludes all aggregation fields", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_publication_search_aggregations("climate")

  agg_fields <- unique(result$aggregation)
  expect_true("type" %in% agg_fields)
  expect_true("topLevelOrganization" %in% agg_fields)
  expect_true("files" %in% agg_fields)
})

test_that("nva_publication_search_aggregationsparses counts correctly", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_publication_search_aggregations("climate")

  type_rows <- result[result$aggregation == "type", ]
  expect_equal(nrow(type_rows), 3)
  expect_equal(type_rows$key[1], "AcademicArticle")
  expect_equal(type_rows$count[1], 80L)
})

test_that("nva_publication_search_aggregationsuses labels when available", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_publication_search_aggregations("climate")

  org_rows <- result[result$aggregation == "topLevelOrganization", ]
  # Has labels -> uses nva_get_label (prefers English)
  expect_equal(org_rows$label[1], "University of Oslo")
  expect_equal(org_rows$label[2], "Oslo Metropolitan University")
})

test_that("nva_publication_search_aggregationsfalls back to key when no labels", {
  local_mock_nva(mock_from_fixture("search-with-aggregations.json"))

  result <- nva_publication_search_aggregations("climate")

  type_rows <- result[result$aggregation == "type", ]
  # No labels -> label equals key
  expect_equal(type_rows$label[1], "AcademicArticle")

  files_rows <- result[result$aggregation == "files", ]
  expect_equal(files_rows$label[1], "hasPublicFiles")
})

test_that("nva_publication_search_aggregationspasses aggregation=all to API", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search_aggregations("test")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$aggregation, "all")
})

test_that("nva_publication_search_aggregationspasses filter parameters to API", {
  captured_req <- NULL

  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search_aggregations("climate", organization = "185", year = "2024",
                            type = "AcademicArticle")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$institution, "185")
  expect_equal(parsed$query$publication_year, "2024")
  expect_equal(parsed$query$instanceType, "AcademicArticle")
})

test_that("nva_publication_search_aggregationsreturns empty tibble with correct schema for no aggregations", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_publication_search_aggregations("nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("aggregation", "key", "label", "count"))
  expect_type(result$aggregation, "character")
  expect_type(result$count, "integer")
})

# Tests for new search parameters

test_that("nva_publication_searchpasses advanced filter parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test",
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

test_that("nva_publication_search passes new filter parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test",
               context_type = "Journal",
               series = "NGI-Rapport",
               publication_language = "http://lexvo.org/id/iso639-3/eng",
               year_since = 2020,
               year_before = 2025,
               created_before = "2024-12-31",
               modified_before = "2024-12-31",
               published_before = "2024-12-31",
               top_level_organization = "https://api.nva.unit.no/cristin/organization/185.0.0.0",
               exclude_subunits = TRUE,
               handle = "https://hdl.handle.net/11250/3093139",
               cristin_identifier = "12345",
               scopus_identifier = "SCOPUS-123",
               course = "INF101",
               funding = "NFR-123",
               scientific_report_period = "2023",
               vocabulary = "https://example.org/vocab/123")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$contextType, "Journal")
  expect_equal(parsed$query$series, "NGI-Rapport")
  expect_equal(parsed$query$publicationLanguage, "http://lexvo.org/id/iso639-3/eng")
  expect_equal(parsed$query$publicationYearSince, "2020")
  expect_equal(parsed$query$publicationYearBefore, "2025")
  expect_equal(parsed$query$createdBefore, "2024-12-31")
  expect_equal(parsed$query$modifiedBefore, "2024-12-31")
  expect_equal(parsed$query$publishedBefore, "2024-12-31")
  expect_equal(parsed$query$topLevelOrganization, "https://api.nva.unit.no/cristin/organization/185.0.0.0")
  expect_equal(parsed$query$excludeSubunits, "TRUE")
  expect_equal(parsed$query$handle, "https://hdl.handle.net/11250/3093139")
  expect_equal(parsed$query$cristinIdentifier, "12345")
  expect_equal(parsed$query$scopusIdentifier, "SCOPUS-123")
  expect_equal(parsed$query$course, "INF101")
  expect_equal(parsed$query$funding, "NFR-123")
  expect_equal(parsed$query$scientificReportPeriod, "2023")
  expect_equal(parsed$query$vocabulary, "https://example.org/vocab/123")
})

test_that("nva_publication_search omits new NULL parameters from query", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test", context_type = "Journal")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$contextType, "Journal")
  expect_null(parsed$query$series)
  expect_null(parsed$query$publicationLanguage)
  expect_null(parsed$query$createdBefore)
  expect_null(parsed$query$topLevelOrganization)
  expect_null(parsed$query$handle)
  expect_null(parsed$query$cristinIdentifier)
  expect_null(parsed$query$scopusIdentifier)
  expect_null(parsed$query$course)
  expect_null(parsed$query$vocabulary)
})

test_that("nva_publication_search_aggregations passes new filter parameters", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search_aggregations("test",
                            context_type = "Report",
                            series = "NGI-Rapport",
                            top_level_organization = "185.0.0.0",
                            handle = "https://hdl.handle.net/11250/3093139",
                            created_before = "2024-12-31",
                            year_since = 2020)
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$contextType, "Report")
  expect_equal(parsed$query$series, "NGI-Rapport")
  expect_equal(parsed$query$topLevelOrganization, "185.0.0.0")
  expect_equal(parsed$query$handle, "https://hdl.handle.net/11250/3093139")
  expect_equal(parsed$query$createdBefore, "2024-12-31")
  expect_equal(parsed$query$publicationYearSince, "2020")
  expect_equal(parsed$query$aggregation, "all")
})

test_that("nva_publication_searchpasses ... params directly to API", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test", series = "MySeries", customParam = "value")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$series, "MySeries")
  expect_equal(parsed$query$customParam, "value")
})

test_that("nva_publication_searchomits NULL parameters from query", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search("test", doi = "10.1234")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$doi, "10.1234")
  expect_null(parsed$query$contributor)
  expect_null(parsed$query$orcid)
  expect_null(parsed$query$institution)
})

test_that("nva_publication_search_aggregationspasses advanced filter parameters", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search_aggregations("test",
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

test_that("nva_publication_search_aggregationspasses ... params directly to API", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search_aggregations("test", series = "MySeries")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$series, "MySeries")
})

test_that("nva_publication_search_aggregationsapplies sort_order to sort key", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-with-aggregations.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication_search_aggregations("test", sort = "modifiedDate", sort_order = "asc")
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$sort, "modifiedDate:asc")
  expect_equal(parsed$query$aggregation, "all")
})
