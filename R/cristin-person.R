#' Get a Cristin person by ID
#'
#' Retrieves information about a researcher from the Cristin registry.
#'
#' @param id Cristin person identifier (numeric or character)
#'
#' @return A list containing the person record:
#' \describe{
#'   \item{id}{Cristin person ID}
#'   \item{names}{List with firstName and lastName}
#'   \item{affiliations}{List of organizational affiliations}
#'   \item{identifiers}{External identifiers (ORCID, etc.)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a researcher by Cristin ID
#' person <- nva_cristin_person(12345)
#'
#' # Access their name
#' paste(person$names$firstName, person$names$lastName)
#' }
nva_cristin_person <- function(id) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- as.character(id)

  nva_get(paste0("cristin/person/", id))
}

#' Search for Cristin persons
#'
#' Search the Cristin person registry by name or other criteria.
#'
#' @param query Name to search for (partial match supported)
#' @param organization Filter by organization Cristin ID
#' @param limit Number of results to return per page (default: 10, max: 100)
#' @param page Page number for pagination (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin person ID}
#'   \item{first_name}{First name}
#'   \item{last_name}{Last name}
#'   \item{affiliations}{List of current affiliations}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for researchers by name
#' nva_cristin_person_search(query = "Hansen")
#'
#' # Search within an organization
#' nva_cristin_person_search(query = "Olsen", organization = "185")
#' }
nva_cristin_person_search <- function(query = NULL,
                                       organization = NULL,
                                       limit = 10L,
                                       page = 1L) {
  if (is.null(query) && is.null(organization)) {
    cli::cli_abort("At least one of {.arg query} or {.arg organization} must be provided.")
  }

  tbl <- nva_get_tibble(
    "cristin/person",
    name = query,
    organization = organization,
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_person())
  }

  nva_parse_cristin_persons(tbl)
}

#' Parse Cristin person search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_persons <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) nva_extract_id(x, "cristin/person")),
    first_name = purrr::map_chr(tbl$names, \(names_list) {
      first <- purrr::keep(names_list, \(n) n$type == "FirstName")
      if (length(first) > 0) first[[1]]$value else NA_character_
    }),
    last_name = purrr::map_chr(tbl$names, \(names_list) {
      last <- purrr::keep(names_list, \(n) n$type == "LastName")
      if (length(last) > 0) last[[1]]$value else NA_character_
    }),
    affiliations = purrr::map(tbl$affiliations, \(affs) {
      if (is.null(affs) || length(affs) == 0) return(list())
      purrr::map(affs, \(a) {
        list(
          organization = nva_extract_id(a$organization %||% "", "cristin/organization"),
          active = a$active %||% NA
        )
      })
    })
  )
}

#' Get publications for a Cristin person
#'
#' Retrieves publications associated with a specific Cristin person ID.
#'
#' @param id Cristin person identifier
#' @param limit Number of results to return per page (default: 10, max: 100)
#' @param offset Offset for pagination (default: 0)
#' @param year Filter by publication year
#' @param fetch_all If TRUE, fetch all publications. Default: FALSE
#' @param max_results Maximum results when `fetch_all = TRUE` (default: Inf)
#'
#' @return A tibble with publication information (same format as nva_search)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get publications for a researcher
#' pubs <- nva_cristin_person_publications(12345)
#'
#' # Get all publications
#' all_pubs <- nva_cristin_person_publications(12345, fetch_all = TRUE)
#' }
nva_cristin_person_publications <- function(id,
                                             limit = 10L,
                                             offset = 0L,
                                             year = NULL,
                                             fetch_all = FALSE,
                                             max_results = Inf) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- as.character(id)

  if (fetch_all) {
    tbl <- nva_fetch_all(
      "search/resources",
      contributor = id,
      year = year,
      results_per_page = 100L,
      max_results = max_results
    )
  } else {
    tbl <- nva_get_tibble(
      "search/resources",
      contributor = id,
      year = year,
      results = limit,
      from = offset
    )
  }

  if (nrow(tbl) == 0) {
    return(schema_publication_search())
  }

  nva_parse_search_results(tbl)
}
