#' Search for Cristin keywords
#'
#' Search the Cristin keyword registry.
#'
#' @param query Keyword text to search for
#' @param language Language filter (e.g., "nb", "en")
#' @param limit Number of results per page (default: 10)
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
#' nva_cristin_keyword_search(query = "machine learning")
#' }
nva_cristin_keyword_search <- function(query = NULL,
                                        language = NULL,
                                        limit = 10L,
                                        page = 1L) {
  if (is.null(query)) {
    cli::cli_abort("{.arg query} is required.")
  }

  tbl <- nva_get_tibble(
    "cristin/keyword",
    name = query,
    language = language,
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_keyword())
  }

  nva_parse_cristin_keywords(tbl)
}

#' Get a Cristin keyword by ID
#'
#' Retrieves a specific keyword from the Cristin registry.
#'
#' @param id Keyword identifier
#'
#' @return A list with the keyword record
#'
#' @export
#'
#' @examples
#' \dontrun{
#' keyword <- nva_cristin_keyword("12345")
#' }
nva_cristin_keyword <- function(id) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- as.character(id)

  nva_get(paste0("cristin/keyword/", id))
}

#' Parse Cristin keyword search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_keywords <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) nva_extract_id(x, "cristin/keyword")),
    label = tbl$label %||% NA_character_,
    language = tbl$language %||% NA_character_
  )
}

#' Search funding sources from Cristin
#'
#' Retrieves the list of available funding sources.
#'
#' @param limit Number of results per page (default: 100)
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
#' sources <- nva_cristin_funding_source_search()
#' }
nva_cristin_funding_source_search <- function(limit = 100L, page = 1L) {
  tbl <- nva_get_tibble(
    "cristin/funding-sources",
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_funding_source())
  }

  nva_parse_funding_sources(tbl)
}

#' Get a specific funding source
#'
#' Retrieves information about a specific funding source by ID.
#'
#' @param id Funding source identifier
#'
#' @return A list with funding source details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' source <- nva_cristin_funding_source("NFR")
#' }
nva_cristin_funding_source <- function(id) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- as.character(id)

  nva_get(paste0("cristin/funding-sources/", id))
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
