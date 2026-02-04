# Tests for publication endpoints

# -- nva_publication() tests --

test_that("nva_publication returns publication data", {
  local_mock_nva(mock_from_fixture("publication-single.json"))

  result <- nva_publication("01907b5e-1234-5678-abcd-123456789abc")

  expect_type(result, "list")
  expect_equal(result$identifier, "01907b5e-1234-5678-abcd-123456789abc")
  expect_equal(result$status, "PUBLISHED")
  expect_equal(result$entityDescription$mainTitle, "A Comprehensive Study of Testing in R")
})

test_that("nva_publication errors on missing id", {
  expect_error(nva_publication(), class = "rlang_error")
  expect_error(nva_publication(NULL), class = "rlang_error")
  expect_error(nva_publication(""), class = "rlang_error")
})

test_that("nva_publication calls correct endpoint", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("publication-single.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_publication("test-id-123")
  })

  expect_true(grepl("publication/test-id-123", captured_req$url))
})

# -- nva_publications() tests --

test_that("nva_publications returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("publication-single.json"))

  result <- nva_publications("01907b5e-1234-5678-abcd-123456789abc")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "doi"))
})

test_that("nva_publications parses data correctly", {
  local_mock_nva(mock_from_fixture("publication-single.json"))

  result <- nva_publications("01907b5e-1234-5678-abcd-123456789abc")

  expect_equal(nrow(result), 1)
  expect_equal(result$identifier[1], "01907b5e-1234-5678-abcd-123456789abc")
  expect_equal(result$title[1], "A Comprehensive Study of Testing in R")
  expect_equal(result$type[1], "AcademicArticle")
  expect_equal(result$year[1], 2024L)
  expect_equal(result$status[1], "PUBLISHED")
  expect_equal(result$contributors[[1]], c("Test Author", "Second Author"))
  expect_equal(result$doi[1], "https://doi.org/10.18637/jss.v000.i00")
})

test_that("nva_publications errors on invalid input", {
  expect_error(nva_publications(character()), class = "rlang_error")
  expect_error(nva_publications(123), class = "rlang_error")
})

test_that("nva_publications returns empty schema on all failures", {
  # Mock that returns 404 for all requests
  mock_404 <- function(req) {
    mock_error_response(404L, "Not found", url = req$url)
  }

  result <- suppressWarnings(
    with_mock_nva(mock_404, {
      nva_publications(c("invalid-1", "invalid-2"))
    })
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "doi"))
})

# -- nva_publication_files() tests --

test_that("nva_publication_files returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("publication-single.json"))

  result <- nva_publication_files("01907b5e-1234-5678-abcd-123456789abc")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("identifier", "name", "mimetype", "size", "license",
                         "administrative_agreement"))
})

test_that("nva_publication_files parses file data correctly", {
  local_mock_nva(mock_from_fixture("publication-single.json"))

  result <- nva_publication_files("01907b5e-1234-5678-abcd-123456789abc")

  # Should only have the PublishedFile, not the Link
  expect_equal(nrow(result), 1)
  expect_equal(result$identifier[1], "file-uuid-1234")
  expect_equal(result$name[1], "manuscript.pdf")
  expect_equal(result$mimetype[1], "application/pdf")
  expect_equal(result$size[1], 1234567L)
  expect_equal(result$license[1], "https://creativecommons.org/licenses/by/4.0/")
  expect_false(result$administrative_agreement[1])
})

test_that("nva_publication_files excludes non-file artifacts", {
  # The fixture has both PublishedFile and Link types
  local_mock_nva(mock_from_fixture("publication-single.json"))

  result <- nva_publication_files("01907b5e-1234-5678-abcd-123456789abc")

  # Only files should be returned, not links
  expect_equal(nrow(result), 1)
  expect_false("link-uuid-5678" %in% result$identifier)
})

test_that("nva_publication_files returns empty schema when no files", {
  # Create a publication with no artifacts
  mock_fn <- function(req) {
    body <- list(
      identifier = "pub-no-files",
      status = "PUBLISHED",
      entityDescription = list(mainTitle = "No Files"),
      associatedArtifacts = list()
    )
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    result <- nva_publication_files("pub-no-files")

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_named(result, c("identifier", "name", "mimetype", "size", "license",
                           "administrative_agreement"))
  })
})

test_that("nva_publication_files returns empty schema when only links", {
  mock_fn <- function(req) {
    body <- list(
      identifier = "pub-links-only",
      status = "PUBLISHED",
      entityDescription = list(mainTitle = "Links Only"),
      associatedArtifacts = list(
        list(type = "Link", identifier = "link-1"),
        list(type = "AssociatedLink", identifier = "link-2")
      )
    )
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    result <- nva_publication_files("pub-links-only")

    expect_equal(nrow(result), 0)
  })
})

# -- nva_download_file() tests --

test_that("nva_download_file errors on missing file_id", {
  expect_error(nva_download_file(), class = "rlang_error")
  expect_error(nva_download_file(NULL, destfile = "test.pdf"), class = "rlang_error")
  expect_error(nva_download_file("", destfile = "test.pdf"), class = "rlang_error")
})

test_that("nva_download_file errors on missing destfile", {
  expect_error(
    nva_download_file("file-123"),
    class = "rlang_error"
  )
})

test_that("nva_download_file errors when file exists and overwrite=FALSE", {
  tmp <- tempfile(fileext = ".pdf")
  file.create(tmp)
  on.exit(unlink(tmp))

  expect_error(
    nva_download_file("file-123", destfile = tmp, overwrite = FALSE),
    "already exists"
  )
})

test_that("nva_download_file constructs correct URL", {
  captured_req <- NULL
  mock_download <- function(req) {
    captured_req <<- req
    httr2::response(
      status_code = 200L,
      url = req$url,
      headers = list("Content-Type" = "application/pdf"),
      body = charToRaw("fake pdf content")
    )
  }

  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp))

  httr2::with_mocked_responses(mock_download, {
    nva_download_file("test-file-uuid", destfile = tmp, progress = FALSE)
  })

  expect_true(grepl("download/public/test-file-uuid", captured_req$url))
})
