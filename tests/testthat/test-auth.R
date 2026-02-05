# Tests for authentication functions

test_that("nva_api_key returns NULL when not set", {
  withr::local_envvar(NVA_API_KEY = NA)

  expect_null(nva_api_key())
})

test_that("nva_api_key returns key when set", {
  withr::local_envvar(NVA_API_KEY = "test-key-123")

  expect_equal(nva_api_key(), "test-key-123")
})

test_that("nva_api_key returns NULL for empty string", {
  withr::local_envvar(NVA_API_KEY = "")

  expect_null(nva_api_key())
})

test_that("nva_has_api_key returns FALSE when not set", {
  withr::local_envvar(NVA_API_KEY = NA)

  expect_false(nva_has_api_key())
})

test_that("nva_has_api_key returns TRUE when set", {
  withr::local_envvar(NVA_API_KEY = "test-key")

  expect_true(nva_has_api_key())
})

test_that("nva_set_api_key sets the environment variable", {
  withr::local_envvar(NVA_API_KEY = NA)

  expect_false(nva_has_api_key())

  expect_message(
    nva_set_api_key("new-key-456"),
    "NVA API key set"
  )

  expect_true(nva_has_api_key())
  expect_equal(nva_api_key(), "new-key-456")
})

test_that("nva_set_api_key errors without key argument", {
  expect_error(nva_set_api_key(), class = "rlang_error")
})

test_that("nva_request includes auth header when API key is set", {
  withr::local_envvar(NVA_API_KEY = "test-bearer-token")

  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    mock_json_response(list(test = TRUE), url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_get("test")
  })

  # httr2 stores auth in a redacted manner, check that Authorization header exists
  expect_true("Authorization" %in% names(captured_req$headers))
})

test_that("nva_request omits auth header when API key is not set", {
  withr::local_envvar(NVA_API_KEY = NA)

  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    mock_json_response(list(test = TRUE), url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_get("test")
  })

  expect_false("Authorization" %in% names(captured_req$headers))
})
