# Tests for utility functions

# -- nva_fetch_all tests --

test_that("nva_fetch_all fetches multiple pages", {
  request_count <- 0

  mock_fn <- function(req) {
    request_count <<- request_count + 1
    parsed <- httr2::url_parse(req$url)
    offset <- as.integer(parsed$query$`from` %||% parsed$query$offset %||% "0")

    if (offset == 0) {
      body <- search_result(
        list(
          list(identifier = "id1", entityDescription = list(mainTitle = "Title 1")),
          list(identifier = "id2", entityDescription = list(mainTitle = "Title 2"))
        ),
        total_size = 5,
        offset = 0
      )
    } else if (offset == 2) {
      body <- search_result(
        list(
          list(identifier = "id3", entityDescription = list(mainTitle = "Title 3")),
          list(identifier = "id4", entityDescription = list(mainTitle = "Title 4"))
        ),
        total_size = 5,
        offset = 2
      )
    } else {
      body <- search_result(
        list(
          list(identifier = "id5", entityDescription = list(mainTitle = "Title 5"))
        ),
        total_size = 5,
        offset = 4
      )
    }

    mock_json_response(body, url = req$url)
  }

  local_mock_nva(mock_fn)

  result <- nva_fetch_all(
    "search/resources",
    query = "test",
    results_per_page = 2L,
    progress = FALSE
  )

  expect_equal(nrow(result), 5)
  expect_equal(result$identifier, c("id1", "id2", "id3", "id4", "id5"))
  expect_gte(request_count, 3)
})

test_that("nva_fetch_all respects max_results", {
  mock_fn <- function(req) {
    body <- search_result(
      list(
        list(identifier = "id1"),
        list(identifier = "id2"),
        list(identifier = "id3")
      ),
      total_size = 100,
      offset = 0
    )
    mock_json_response(body, url = req$url)
  }

  local_mock_nva(mock_fn)

  result <- nva_fetch_all(
    "search/resources",
    query = "test",
    results_per_page = 10L,
    max_results = 3L,
    progress = FALSE
  )

  expect_equal(nrow(result), 3)
})

test_that("nva_fetch_all returns empty tibble for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_fetch_all(
    "search/resources",
    query = "nonexistent",
    progress = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

# -- nva_extract_id tests --

test_that("nva_extract_id extracts ID from URLs", {
  url <- "https://api.nva.unit.no/cristin/person/12345"
  expect_equal(nva_extract_id(url, "cristin/person"), "12345")

  url <- "https://api.nva.unit.no/cristin/organization/185.90.0.0"
  expect_equal(nva_extract_id(url, "cristin/organization"), "185.90.0.0")
})

test_that("nva_extract_id handles vector input", {
  urls <- c(
    "https://api.nva.unit.no/cristin/person/111",
    "https://api.nva.unit.no/cristin/person/222"
  )
  result <- nva_extract_id(urls, "cristin/person")
  expect_equal(result, c("111", "222"))
})

# -- nva_get_label tests --

test_that("nva_get_label prefers English", {
  labels <- list(en = "English", nb = "Norwegian", nn = "Nynorsk")
  expect_equal(nva_get_label(labels), "English")
})

test_that("nva_get_label falls back to Norwegian", {
  labels <- list(nb = "Norwegian", nn = "Nynorsk")
  expect_equal(nva_get_label(labels), "Norwegian")
})

test_that("nva_get_label falls back to Nynorsk", {
  labels <- list(nn = "Nynorsk")
  expect_equal(nva_get_label(labels), "Nynorsk")
})

test_that("nva_get_label returns first available when none match", {
  labels <- list(de = "German", fr = "French")
  expect_equal(nva_get_label(labels), "German")
})

test_that("nva_get_label returns NA for NULL", {
  expect_equal(nva_get_label(NULL), NA_character_)
})

test_that("nva_get_label returns NA for empty list", {
  expect_equal(nva_get_label(list()), NA_character_)
})

# -- nva_empty_tibble tests --

test_that("nva_empty_tibble creates typed empty tibble", {
  result <- nva_empty_tibble(
    name = "chr",
    count = "int",
    value = "dbl",
    active = "lgl",
    items = "list"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 5)
  expect_type(result$name, "character")
  expect_type(result$count, "integer")
  expect_type(result$value, "double")
  expect_type(result$active, "logical")
  expect_type(result$items, "list")
})

# -- nva_pagination_info tests --

test_that("nva_pagination_info extracts pagination details", {
  body <- list(
    hits = list(list(id = 1), list(id = 2)),
    totalHits = 100,
    size = 2,
    offset = 10
  )

  info <- nva_pagination_info(body)

  expect_equal(info$total, 100)
  expect_equal(info$size, 2)
  expect_equal(info$offset, 10)
  expect_true(info$has_more)
})

test_that("nva_pagination_info handles missing fields", {
  body <- list(hits = list())

  info <- nva_pagination_info(body)

  expect_equal(info$total, 0)
  expect_equal(info$size, 0)
  expect_equal(info$offset, 0)
  expect_false(info$has_more)
})
