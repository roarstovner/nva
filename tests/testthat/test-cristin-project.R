# Tests for Cristin project endpoints

# -- nva_cristin_project() tests --

test_that("nva_cristin_project returns project data", {
  local_mock_nva(mock_from_fixture("cristin-project.json"))

  result <- nva_cristin_project(123456)

  expect_type(result, "list")
  expect_equal(result$id, "https://api.nva.unit.no/cristin/project/123456")
  expect_equal(result$status, "ACTIVE")
  expect_equal(result$startDate, "2020-01-01")
  expect_equal(result$endDate, "2025-12-31")
  expect_length(result$fundingSources, 1)
  expect_length(result$participants, 1)
})

test_that("nva_cristin_project errors on missing id", {
  expect_error(nva_cristin_project(), class = "rlang_error")
  expect_error(nva_cristin_project(NULL), class = "rlang_error")
})

test_that("nva_cristin_project accepts numeric or character id", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-project.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_project(123456)
  })
  expect_true(grepl("cristin/project/123456", captured_req$url))

  with_mock_nva(mock_fn, {
    nva_cristin_project("123456")
  })
  expect_true(grepl("cristin/project/123456", captured_req$url))
})

# -- nva_cristin_projects() tests --

test_that("nva_cristin_projects returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-project.json"))

  result <- nva_cristin_projects("123456")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "title", "status", "start_date", "end_date", "coordinating_institution"))
})

test_that("nva_cristin_projects parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-project.json"))

  result <- nva_cristin_projects("123456")

  expect_equal(nrow(result), 1)
  expect_equal(result$id[1], "123456")
  expect_equal(result$title[1], "Climate Change and Sea Level")
  expect_equal(result$status[1], "ACTIVE")
  expect_equal(result$start_date[1], "2020-01-01")
  expect_equal(result$end_date[1], "2025-12-31")
  expect_equal(result$coordinating_institution[1], "185.0.0.0")
})

test_that("nva_cristin_projects accepts numeric ids", {
  local_mock_nva(mock_from_fixture("cristin-project.json"))

  result <- nva_cristin_projects(123456)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("nva_cristin_projects errors on invalid input", {
  expect_error(nva_cristin_projects(character()), class = "rlang_error")
  expect_error(nva_cristin_projects(list()), class = "rlang_error")
})

test_that("nva_cristin_projects handles multiple IDs", {
  local_mock_nva(mock_from_fixture("cristin-project.json"))

  result <- nva_cristin_projects(c("123456", "789012"))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("nva_cristin_projects handles errors gracefully", {
  call_count <- 0
  mock_fn <- function(req) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      # First call fails
      httr2::response(status_code = 404)
    } else {
      # Second call succeeds
      body <- load_fixture("cristin-project.json")
      mock_json_response(body, url = req$url)
    }
  }

  expect_warning(
    with_mock_nva(mock_fn, {
      result <- nva_cristin_projects(c("999", "123456"))
      expect_equal(nrow(result), 1)
      expect_equal(result$id[1], "123456")
    }),
    "Failed to fetch project"
  )
})

test_that("nva_cristin_projects returns empty schema when all fail", {
  mock_fn <- function(req) {
    httr2::response(status_code = 404)
  }

  expect_warning(
    with_mock_nva(mock_fn, {
      result <- nva_cristin_projects("999")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
      expect_named(result, c("id", "title", "status", "start_date", "end_date", "coordinating_institution"))
    }),
    "Failed to fetch project"
  )
})

# -- nva_cristin_project_search() tests --

test_that("nva_cristin_project_search validates limit parameter", {
  expect_error(nva_cristin_project_search(query = "test", limit = 0), class = "rlang_error")
  expect_error(nva_cristin_project_search(query = "test", limit = 101), class = "rlang_error")
  expect_error(nva_cristin_project_search(query = "test", limit = -1), class = "rlang_error")
})

test_that("nva_cristin_project_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-project-search.json"))

  result <- nva_cristin_project_search(query = "climate")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "title", "status", "start_date", "end_date"))
})

test_that("nva_cristin_project_search parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-project-search.json"))

  result <- nva_cristin_project_search(query = "test")

  expect_equal(nrow(result), 2)
  expect_equal(result$id[1], "123456")
  expect_equal(result$status[1], "ACTIVE")
  expect_equal(result$start_date[1], "2020-01-01")
  expect_equal(result$end_date[1], "2025-12-31")

  expect_equal(result$id[2], "789012")
  expect_equal(result$status[2], "CONCLUDED")
})

test_that("nva_cristin_project_search errors when no search parameters", {
  expect_error(
    nva_cristin_project_search(),
    class = "rlang_error"
  )
})

test_that("nva_cristin_project_search accepts organization filter only", {
  local_mock_nva(mock_from_fixture("cristin-project-search.json"))

  expect_no_error(nva_cristin_project_search(organization = "185"))
})

test_that("nva_cristin_project_search accepts keyword filter only", {
  local_mock_nva(mock_from_fixture("cristin-project-search.json"))

  expect_no_error(nva_cristin_project_search(keyword = "climate"))
})

test_that("nva_cristin_project_search accepts status filter only", {
  local_mock_nva(mock_from_fixture("cristin-project-search.json"))

  expect_no_error(nva_cristin_project_search(status = "ACTIVE"))
})

test_that("nva_cristin_project_search returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_project_search(query = "nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "title", "status", "start_date", "end_date"))
})

test_that("nva_cristin_project_search passes query parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-project-search.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_project_search(
      query = "climate",
      organization = "185",
      keyword = "environment",
      status = "ACTIVE",
      limit = 25,
      page = 2
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$title, "climate")
  expect_equal(parsed$query$institution, "185")
  expect_equal(parsed$query$keyword, "environment")
  expect_equal(parsed$query$status, "ACTIVE")
  expect_equal(parsed$query$results, "25")
  expect_equal(parsed$query$page, "2")
})

# -- nva_cristin_project_categories() tests --

test_that("nva_cristin_project_categories returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-project-categories.json"))

  result <- nva_cristin_project_categories()

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "name"))
})

test_that("nva_cristin_project_categories parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-project-categories.json"))

  result <- nva_cristin_project_categories()

  expect_equal(nrow(result), 3)
  expect_equal(result$code[1], "BASICRESEARCH")
  expect_equal(result$name[1], "Basic research")
  expect_equal(result$code[2], "APPLIEDRESEARCH")
  expect_equal(result$name[2], "Applied research")
})

test_that("nva_cristin_project_categories returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_project_categories()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("code", "name"))
})
