# Tests for Cristin organization endpoints

# -- nva_cristin_organization() tests --

test_that("nva_cristin_organization returns organization data", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organization(185)

  expect_type(result, "list")
  expect_equal(result$id, "https://api.nva.unit.no/cristin/organization/185.0.0.0")
  expect_equal(result$labels$en, "University of Oslo")
  expect_equal(result$acronym, "UiO")
  expect_equal(result$country, "NO")
})

test_that("nva_cristin_organization errors on missing id", {
  expect_error(nva_cristin_organization(), class = "rlang_error")
  expect_error(nva_cristin_organization(NULL), class = "rlang_error")
})

test_that("nva_cristin_organization normalizes short IDs", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-organization.json")
    mock_json_response(body, url = req$url)
  }

  # Short ID should become full format
  with_mock_nva(mock_fn, {
    nva_cristin_organization("185")
  })
  expect_true(grepl("cristin/organization/185.0.0.0", captured_req$url))

  # Partial ID should be padded
  with_mock_nva(mock_fn, {
    nva_cristin_organization("185.15")
  })
  expect_true(grepl("cristin/organization/185.15.0.0", captured_req$url))

  # Full ID should remain unchanged
  with_mock_nva(mock_fn, {
    nva_cristin_organization("185.15.2.10")
  })
  expect_true(grepl("cristin/organization/185.15.2.10", captured_req$url))
})

# -- nva_cristin_organizations() tests --

test_that("nva_cristin_organizations returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organizations("185")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "name", "acronym", "country", "parent_id"))
})

test_that("nva_cristin_organizations parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organizations("185")

  expect_equal(nrow(result), 1)
  expect_equal(result$id[1], "185.0.0.0")
  expect_equal(result$name[1], "University of Oslo")
  expect_equal(result$acronym[1], "UiO")
  expect_equal(result$country[1], "NO")
  expect_true(is.na(result$parent_id[1]))  # Top-level org has no parent
})

test_that("nva_cristin_organizations accepts numeric ids", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organizations(185)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that("nva_cristin_organizations errors on invalid input", {
  expect_error(nva_cristin_organizations(character()), class = "rlang_error")
  expect_error(nva_cristin_organizations(list()), class = "rlang_error")
})

test_that("nva_cristin_organizations handles multiple IDs", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organizations(c("185", "194"))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("nva_cristin_organizations handles errors gracefully", {
  call_count <- 0
  mock_fn <- function(req) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      # First call fails
      httr2::response(status_code = 404)
    } else {
      # Second call succeeds
      body <- load_fixture("cristin-organization.json")
      mock_json_response(body, url = req$url)
    }
  }

  expect_warning(
    with_mock_nva(mock_fn, {
      result <- nva_cristin_organizations(c("999", "185"))
      expect_equal(nrow(result), 1)
      expect_equal(result$id[1], "185.0.0.0")
    }),
    "Failed to fetch organization"
  )
})

test_that("nva_cristin_organizations returns empty schema when all fail", {
  mock_fn <- function(req) {
    httr2::response(status_code = 404)
  }

  expect_warning(
    with_mock_nva(mock_fn, {
      result <- nva_cristin_organizations("999")
      expect_s3_class(result, "tbl_df")
      expect_equal(nrow(result), 0)
      expect_named(result, c("id", "name", "acronym", "country", "parent_id"))
    }),
    "Failed to fetch organization"
  )
})

# -- nva_cristin_organization_search() tests --

test_that("nva_cristin_organization_search returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-organization-search.json"))

  result <- nva_cristin_organization_search(query = "oslo")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "name", "acronym", "country"))
})

test_that("nva_cristin_organization_search parses data correctly", {
  local_mock_nva(mock_from_fixture("cristin-organization-search.json"))

  result <- nva_cristin_organization_search(query = "oslo")

  expect_equal(nrow(result), 3)

  # First org has English label
  expect_equal(result$id[1], "185.0.0.0")
  expect_equal(result$name[1], "University of Oslo")
  expect_equal(result$acronym[1], "UiO")
  expect_equal(result$country[1], "NO")

  # Second org has only Norwegian label
  expect_equal(result$id[2], "186.0.0.0")
  expect_equal(result$name[2], "OsloMet - storbyuniversitetet")

  # Third org has no acronym
  expect_equal(result$id[3], "7465.0.0.0")
  expect_true(is.na(result$acronym[3]))
})

test_that("nva_cristin_organization_search returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_organization_search(query = "nonexistent")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "acronym", "country"))
})

test_that("nva_cristin_organization_search validates depth parameter", {
  local_mock_nva(mock_from_fixture("cristin-organization-search.json"))

  expect_no_error(nva_cristin_organization_search(query = "test", depth = "top"))
  expect_no_error(nva_cristin_organization_search(query = "test", depth = "full"))
  expect_error(
    nva_cristin_organization_search(query = "test", depth = "invalid"),
    class = "rlang_error"
  )
})

test_that("nva_cristin_organization_search passes query parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("cristin-organization-search.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_organization_search(
      query = "university",
      depth = "full",
      limit = 25,
      page = 2
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$query, "university")
  expect_equal(parsed$query$depth, "full")
  expect_equal(parsed$query$results, "25")
  expect_equal(parsed$query$page, "2")
})

# -- nva_cristin_organization_subunits() tests --

test_that("nva_cristin_organization_subunits returns tibble with expected columns", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organization_subunits(185)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("id", "name", "acronym", "level"))
})

test_that("nva_cristin_organization_subunits returns immediate children by default", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organization_subunits(185, depth = 1)

  # Only level 1 (faculties)
  expect_equal(nrow(result), 2)
  expect_true(all(result$level == 1L))

  expect_equal(result$id[1], "185.15.0.0")
  expect_equal(result$name[1], "Faculty of Mathematics and Natural Sciences")
  expect_equal(result$acronym[1], "MN")

  expect_equal(result$id[2], "185.10.0.0")
  expect_equal(result$name[2], "Faculty of Social Sciences")
  expect_true(is.na(result$acronym[2]))
})

test_that("nva_cristin_organization_subunits respects depth parameter", {
  local_mock_nva(mock_from_fixture("cristin-organization.json"))

  result <- nva_cristin_organization_subunits(185, depth = 2)

  # Should include both level 1 (faculties) and level 2 (departments)
  expect_equal(nrow(result), 3)
  expect_equal(sum(result$level == 1L), 2)
  expect_equal(sum(result$level == 2L), 1)

  # Check the department is included
  dept <- result[result$level == 2L, ]
  expect_equal(dept$id, "185.15.2.0")
  expect_equal(dept$name, "Department of Informatics")
  expect_equal(dept$acronym, "IFI")
})

test_that("nva_cristin_organization_subunits errors on missing id", {
  expect_error(nva_cristin_organization_subunits(), class = "rlang_error")
  expect_error(nva_cristin_organization_subunits(NULL), class = "rlang_error")
})

test_that("nva_cristin_organization_subunits returns empty schema for no subunits", {
  mock_fn <- function(req) {
    body <- list(
      id = "https://api.nva.unit.no/cristin/organization/999.0.0.0",
      labels = list(en = "Leaf Organization"),
      hasPart = list()
    )
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    result <- nva_cristin_organization_subunits("999")

    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0)
    expect_named(result, c("id", "name", "acronym", "level"))
  })
})

# -- nva_cristin_organization_publications() tests --

test_that("nva_cristin_organization_publications returns publication tibble", {
  local_mock_nva(mock_from_fixture("search-publications.json"))

  result <- nva_cristin_organization_publications(185)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))
})

test_that("nva_cristin_organization_publications errors on missing id", {
  expect_error(nva_cristin_organization_publications(), class = "rlang_error")
  expect_error(nva_cristin_organization_publications(NULL), class = "rlang_error")
})

test_that("nva_cristin_organization_publications returns empty schema for no results", {
  local_mock_nva(mock_from_fixture("empty-search.json"))

  result <- nva_cristin_organization_publications("999")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))
})

test_that("nva_cristin_organization_publications passes parameters correctly", {
  captured_req <- NULL
  mock_fn <- function(req) {
    captured_req <<- req
    body <- load_fixture("search-publications.json")
    mock_json_response(body, url = req$url)
  }

  with_mock_nva(mock_fn, {
    nva_cristin_organization_publications(
      id = "185",
      limit = 50,
      offset = 20,
      year = "2024",
      type = "AcademicArticle"
    )
  })

  parsed <- httr2::url_parse(captured_req$url)
  expect_equal(parsed$query$institution, "185")
  expect_equal(parsed$query$results, "50")
  expect_equal(parsed$query$from, "20")
  expect_equal(parsed$query$publication_year, "2024")
  expect_equal(parsed$query$instanceType, "AcademicArticle")
})
