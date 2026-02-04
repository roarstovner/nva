#' Empty result schemas for consistent return types
#'
#' Internal functions that define the column structure for empty result tibbles.
#' Used to ensure consistent return types when API queries return no results.
#'
#' @name schemas
#' @noRd
NULL

#' @noRd
schema_publication_search <- function() {

  nva_empty_tibble(
    identifier = "chr",
    title = "chr",
    type = "chr",
    year = "int",
    status = "chr",
    contributors = "list",
    institutions = "list"
  )
}

#' @noRd
schema_publication_detail <- function() {
  nva_empty_tibble(
    identifier = "chr",
    title = "chr",
    type = "chr",
    year = "int",
    status = "chr",
    contributors = "list",
    doi = "chr"
  )
}

#' @noRd
schema_publication_file <- function() {
  nva_empty_tibble(
    identifier = "chr",
    name = "chr",
    mimetype = "chr",
    size = "int",
    license = "chr",
    administrative_agreement = "lgl"
  )
}

#' @noRd
schema_cristin_organization <- function() {
  nva_empty_tibble(
    id = "chr",
    name = "chr",
    acronym = "chr",
    country = "chr"
  )
}

#' @noRd
schema_cristin_organization_subunit <- function() {
  nva_empty_tibble(
    id = "chr",
    name = "chr",
    acronym = "chr",
    level = "int"
  )
}

#' @noRd
schema_cristin_person <- function() {
  nva_empty_tibble(
    id = "chr",
    first_name = "chr",
    last_name = "chr",
    affiliations = "list"
  )
}

#' @noRd
schema_cristin_project <- function() {
  nva_empty_tibble(
    id = "chr",
    title = "chr",
    status = "chr",
    start_date = "chr",
    end_date = "chr"
  )
}

#' @noRd
schema_cristin_keyword <- function() {
  nva_empty_tibble(
    id = "chr",
    label = "chr",
    language = "chr"
  )
}

#' @noRd
schema_cristin_funding_source <- function() {
  nva_empty_tibble(
    id = "chr",
    name = "chr",
    acronym = "chr"
  )
}

#' @noRd
schema_nfr_funding <- function() {
  nva_empty_tibble(
    id = "chr",
    title = "chr",
    status = "chr",
    amount = "dbl"
  )
}
