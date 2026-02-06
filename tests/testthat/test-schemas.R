# Tests for schema functions (empty result templates)

# -- schema_publication_search --

test_that("schema_publication_search returns correct structure", {
  result <- schema_publication_search()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "institutions"))
  expect_type(result$identifier, "character")
  expect_type(result$title, "character")
  expect_type(result$type, "character")
  expect_type(result$year, "integer")
  expect_type(result$status, "character")
  expect_type(result$contributors, "list")
  expect_type(result$institutions, "list")
})

# -- schema_publication_detail --

test_that("schema_publication_detail returns correct structure", {
  result <- schema_publication_detail()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "title", "type", "year", "status",
                         "contributors", "doi"))
  expect_type(result$identifier, "character")
  expect_type(result$year, "integer")
  expect_type(result$contributors, "list")
  expect_type(result$doi, "character")
})

# -- schema_publication_file --

test_that("schema_publication_file returns correct structure", {
  result <- schema_publication_file()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("identifier", "name", "mimetype", "size", "license",
                         "administrative_agreement"))
  expect_type(result$identifier, "character")
  expect_type(result$name, "character")
  expect_type(result$mimetype, "character")
  expect_type(result$size, "integer")
  expect_type(result$license, "character")
  expect_type(result$administrative_agreement, "logical")
})

# -- schema_cristin_organization --

test_that("schema_cristin_organization returns correct structure", {
  result <- schema_cristin_organization()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "acronym", "country"))
  expect_type(result$id, "character")
  expect_type(result$name, "character")
  expect_type(result$acronym, "character")
  expect_type(result$country, "character")
})

# -- schema_cristin_organization_detail --

test_that("schema_cristin_organization_detail returns correct structure", {
  result <- schema_cristin_organization_detail()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "acronym", "country", "parent_id"))
  expect_type(result$parent_id, "character")
})

# -- schema_cristin_organization_subunit --

test_that("schema_cristin_organization_subunit returns correct structure", {
  result <- schema_cristin_organization_subunit()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "acronym", "level"))
  expect_type(result$level, "integer")
})

# -- schema_cristin_person --

test_that("schema_cristin_person returns correct structure", {
  result <- schema_cristin_person()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "first_name", "last_name", "affiliations"))
  expect_type(result$id, "character")
  expect_type(result$first_name, "character")
  expect_type(result$last_name, "character")
  expect_type(result$affiliations, "list")
})

# -- schema_cristin_person_detail --

test_that("schema_cristin_person_detail returns correct structure", {
  result <- schema_cristin_person_detail()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "first_name", "last_name",
                         "preferred_first_name", "orcid", "affiliations"))
  expect_type(result$preferred_first_name, "character")
  expect_type(result$orcid, "character")
  expect_type(result$affiliations, "list")
})

# -- schema_cristin_project --

test_that("schema_cristin_project returns correct structure", {
  result <- schema_cristin_project()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "title", "status", "start_date", "end_date"))
  expect_type(result$id, "character")
  expect_type(result$start_date, "character")
  expect_type(result$end_date, "character")
})

# -- schema_cristin_project_detail --

test_that("schema_cristin_project_detail returns correct structure", {
  result <- schema_cristin_project_detail()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "title", "status", "start_date", "end_date",
                         "coordinating_institution"))
  expect_type(result$coordinating_institution, "character")
})

# -- schema_cristin_keyword --

test_that("schema_cristin_keyword returns correct structure", {
  result <- schema_cristin_keyword()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "label", "language"))
  expect_type(result$id, "character")
  expect_type(result$label, "character")
  expect_type(result$language, "character")
})

# -- schema_cristin_funding_source --

test_that("schema_cristin_funding_source returns correct structure", {
  result <- schema_cristin_funding_source()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "acronym"))
  expect_type(result$id, "character")
  expect_type(result$name, "character")
  expect_type(result$acronym, "character")
})

# -- schema_nfr_funding --

test_that("schema_nfr_funding returns correct structure", {
  result <- schema_nfr_funding()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "title", "status", "amount"))
  expect_type(result$id, "character")
  expect_type(result$title, "character")
  expect_type(result$status, "character")
  expect_type(result$amount, "double")
})

# -- consistency checks --

test_that("all schemas are bindable with themselves", {
  schemas <- list(
    schema_publication_search(),
    schema_publication_detail(),
    schema_publication_file(),
    schema_cristin_organization(),
    schema_cristin_organization_detail(),
    schema_cristin_organization_subunit(),
    schema_cristin_person(),
    schema_cristin_person_detail(),
    schema_cristin_project(),
    schema_cristin_project_detail(),
    schema_cristin_keyword(),
    schema_cristin_funding_source(),
    schema_nfr_funding()
  )

  for (schema in schemas) {
    bound <- dplyr::bind_rows(schema, schema)
    expect_equal(nrow(bound), 0)
    expect_equal(names(bound), names(schema))
  }
})
