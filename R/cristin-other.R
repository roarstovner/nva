#' Search for Cristin keywords
#'
#' Search the Cristin keyword registry.
#'
#' @param query Keyword text to search for
#' @param language Language filter (e.g., "nb", "en")
#' @param results Number of results per page (default: 10)
#' @param page Page number (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Keyword ID}
#'   \item{label}{Keyword text}
#'   \item{language}{Language code}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' nva_cristin_keywords(query = "machine learning")
#' }
nva_cristin_keywords <- function(query = NULL,
                                  language = NULL,
                                  results = 10L,
                                  page = 1L) {
  if (is.null(query)) {
    cli::cli_abort("{.arg query} is required.")
  }

  resp <- nva_request(
    "cristin/keyword",
    name = query,
    language = language,
    results = results,
    page = page
  ) |>
    httr2::req_perform()

  tbl <- nva_resp_body_tibble(resp)

  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      id = character(),
      label = character(),
      language = character()
    ))
  }

  nva_parse_cristin_keywords(tbl)
}

#' Get a Cristin keyword by ID
#'
#' Retrieves a specific keyword from the Cristin registry.
#'
#' @param keyword_id Keyword identifier
#'
#' @return A list with the keyword record
#'
#' @export
#'
#' @examples
#' \dontrun{
#' keyword <- nva_cristin_keyword("12345")
#' }
nva_cristin_keyword <- function(keyword_id) {
  if (missing(keyword_id) || is.null(keyword_id)) {
    cli::cli_abort("{.arg keyword_id} is required.")
  }

  keyword_id <- as.character(keyword_id)

  nva_get(paste0("cristin/keyword/", keyword_id))
}

#' Parse Cristin keyword search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_keywords <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) {
      sub(".*/cristin/keyword/", "", x)
    }),
    label = tbl$label %||% NA_character_,
    language = tbl$language %||% NA_character_
  )
}

#' List funding sources from Cristin
#'
#' Retrieves the list of available funding sources.
#'
#' @param results Number of results per page (default: 100)
#' @param page Page number (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Funding source identifier}
#'   \item{name}{Funding source name}
#'   \item{acronym}{Funding source acronym}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sources <- nva_cristin_funding_sources()
#' }
nva_cristin_funding_sources <- function(results = 100L, page = 1L) {
  resp <- nva_request(
    "cristin/funding-sources",
    results = results,
    page = page
  ) |>
    httr2::req_perform()

  tbl <- nva_resp_body_tibble(resp)

  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      id = character(),
      name = character(),
      acronym = character()
    ))
  }

  nva_parse_funding_sources(tbl)
}

#' Get a specific funding source
#'
#' Retrieves information about a specific funding source by ID.
#'
#' @param source_id Funding source identifier
#'
#' @return A list with funding source details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' source <- nva_cristin_funding_source("NFR")
#' }
nva_cristin_funding_source <- function(source_id) {
  if (missing(source_id) || is.null(source_id)) {
    cli::cli_abort("{.arg source_id} is required.")
  }

  source_id <- as.character(source_id)

  nva_get(paste0("cristin/funding-sources/", source_id))
}

#' Parse funding sources response
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_funding_sources <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$identifier, \(x) x %||% NA_character_),
    name = purrr::map_chr(tbl$name, \(n) {
      if (is.null(n) || length(n) == 0) return(NA_character_)
      n[[1]] %||% NA_character_
    }),
    acronym = purrr::map_chr(tbl$acronym, \(a) a %||% NA_character_)
  )
}
