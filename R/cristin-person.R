#' Get a Cristin person by ID
#'
#' Retrieves information about a researcher from the Cristin registry.
#'
#' @param cristin_id Cristin person identifier (numeric or character)
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
nva_cristin_person <- function(cristin_id) {
  if (missing(cristin_id) || is.null(cristin_id)) {
    cli::cli_abort("{.arg cristin_id} is required.")
  }

  cristin_id <- as.character(cristin_id)

  nva_get(paste0("cristin/person/", cristin_id))
}

#' Search for Cristin persons
#'
#' Search the Cristin person registry by name or other criteria.
#'
#' @param name Name to search for (partial match supported)
#' @param organization Filter by organization Cristin ID
#' @param results Number of results to return per page (default: 10, max: 100)
#' @param page Page number for pagination (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin person ID}
#'   \item{firstName}{First name}
#'   \item{lastName}{Last name}
#'   \item{affiliations}{List of current affiliations}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for researchers by name
#' nva_cristin_person_search(name = "Hansen")
#'
#' # Search within an organization
#' nva_cristin_person_search(name = "Olsen", organization = "185")
#' }
nva_cristin_person_search <- function(name = NULL,
                                       organization = NULL,
                                       results = 10L,
                                       page = 1L) {
  if (is.null(name) && is.null(organization)) {
    cli::cli_abort("At least one of {.arg name} or {.arg organization} must be provided.")
  }

  resp <- nva_request(
    "cristin/person",
    name = name,
    organization = organization,
    results = results,
    page = page
  ) |>
    httr2::req_perform()

  tbl <- nva_resp_body_tibble(resp)

  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      id = character(),
      firstName = character(),
      lastName = character(),
      affiliations = list()
    ))
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
    id = purrr::map_chr(tbl$id, \(x) {
      # Extract ID from URL
      sub(".*/cristin/person/", "", x)
    }),
    firstName = purrr::map_chr(tbl$names, \(names_list) {
      # names is a list of {type, value} objects
      first <- purrr::keep(names_list, \(n) n$type == "FirstName")
      if (length(first) > 0) first[[1]]$value else NA_character_
    }),
    lastName = purrr::map_chr(tbl$names, \(names_list) {
      last <- purrr::keep(names_list, \(n) n$type == "LastName")
      if (length(last) > 0) last[[1]]$value else NA_character_
    }),
    affiliations = purrr::map(tbl$affiliations, \(affs) {
      if (is.null(affs) || length(affs) == 0) return(list())
      purrr::map(affs, \(a) {
        list(
          organization = sub(".*/cristin/organization/", "", a$organization %||% ""),
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
#' @param cristin_id Cristin person identifier
#' @param results Number of results to return per page (default: 10, max: 100)
#' @param from Offset for pagination (default: 0)
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
nva_cristin_person_publications <- function(cristin_id,
                                             results = 10L,
                                             from = 0L,
                                             year = NULL,
                                             fetch_all = FALSE,
                                             max_results = Inf) {
  if (missing(cristin_id) || is.null(cristin_id)) {
    cli::cli_abort("{.arg cristin_id} is required.")
  }

  cristin_id <- as.character(cristin_id)

  if (fetch_all) {
    tbl <- nva_fetch_all(
      "search/resources",
      contributor = cristin_id,
      year = year,
      results_per_page = 100L,
      max_results = max_results
    )
  } else {
    resp <- nva_request(
      "search/resources",
      contributor = cristin_id,
      year = year,
      results = results,
      from = from
    ) |>
      httr2::req_perform()

    tbl <- nva_resp_body_tibble(resp)
  }

  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      identifier = character(),
      title = character(),
      type = character(),
      year = integer(),
      status = character(),
      contributors = list(),
      institutions = list()
    ))
  }

  nva_parse_search_results(tbl)
}
