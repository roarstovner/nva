# Tests for Cristin person endpoints

# -- nva_cristin_person() tests --

test_that("nva_cristin_person returns person data", {
  local_mock_nva(mock_from_fixture("cristin-person.json"))

  result <- nva_cristin_person(12345)

  expect_type(result, "list")
  expect_equal(result$id, "https://api.nva.unit.no/cristin/person/12345")
  expect_length(result$names, 3)
  expect_length(result$identifiers, 2)
})

test_that("nva_cristin_person errors on missing id", {
  expect_error(nva_cristin_person(), class = "rlang_error")
  expect_error(nva_cristin_person(NULL), class = "rlang_error")
})

test_that("nva_cristin_person accepts numeric or character id", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-person.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_person(12345)
  })
  expect_true(grepl("cristin/person/12345", captured_req$url))

  with_mock_nva(mock_fn, {
    nva_cristin_person("12345")
  })
  expect_true(grepl("cristin/person/12345", captured_req$url))
})

# -- nva_cristin_person_search() tests --

test_that("nva_cristin_person_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-person-search.json"))

  result <- nva_cristin_person_search(query = "Nordmann")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "first_name", "last_name", "affiliations"))
})

test_that("nva_cristin_person_search parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-person-search.json"))

  result <- nva_cristin_person_search(query = "test")

  expect_equal(nrow(result), 2)
  expect_equal(result$id[1], "12345")
  expect_equal(result$first_name[1], "Ola")
  expect_equal(result$last_name[1], "Nordmann")

  expect_equal(result$id[2], "67890")
  expect_equal(result$first_name[2], "Kari")
  expect_equal(result$last_name[2], "Hansen")
})

test_that("nva_cristin_person_search parses affiliations correctly", {
  local_mock_nva(mock_from_fixture("cristin-person-search.json"))

  result <- nva_cristin_person_search(query = "test")

  # First person has one affiliation
  expect_length(result$affiliations[[1]], 1)
  expect_equal(result$affiliations[[1]][[1]]$organization, "185.0.0.0")
  expect_true(result$affiliations[[1]][[1]]$active)

  # Second person has two affiliations
  expect_length(result$affiliations[[2]], 2)
  expect_equal(result$affiliations[[2]][[1]]$organization, "194.0.0.0")
  expect_true(result$affiliations[[2]][[1]]$active)
  expect_equal(result$affiliations[[2]][[2]]$organization, "185.0.0.0")
  expect_false(result$affiliations[[2]][[2]]$active)
})

test_that("nva_cristin_person_search errors when no query or organization", {
  expect_error(
    nva_cristin_person_search(),
    class = "rlang_error"
  )
})

test_that("nva_cristin_person_search accepts organization filter only", {
  local_mock_nva(mock_from_fixture("cristin-person-search.json"))

  expect_no_error(nva_cristin_person_search(organization = "185"))
})

test_that("nva_cristin_person_search returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_person_search(query = "nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "first_name", "last_name", "affiliations"))
})

test_that("nva_cristin_person_search passes query parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-person-search.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_person_search(
      query = "Hansen",
      organization = "185",
      limit = 25,
      page = 2
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$name, "Hansen")
  expect_equal(parsed$query$organization, "185")
  expect_equal(parsed$query$results, "25")
  expect_equal(parsed$query$page, "2")
})

# -- nva_cristin_person_publications() tests --

test_that("nva_cristin_person_publications returns publication tibble", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_cristin_person_publications(12345)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))
})

test_that("nva_cristin_person_publications errors on missing id", {
  expect_error(nva_cristin_person_publications(), class = "rlang_error")
  expect_error(nva_cristin_person_publications(NULL), class = "rlang_error")
})

test_that("nva_cristin_person_publications returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_person_publications(99999)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))
})

test_that("nva_cristin_person_publications passes parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_person_publications(
      id = 12345,
      limit = 50,
      offset = 20,
      year = "2024"
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$contributor, "12345")
  expect_equal(parsed$query$results, "50")
  expect_equal(parsed$query$from, "20")
  expect_equal(parsed$query$year, "2024")
})
