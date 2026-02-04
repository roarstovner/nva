#' Search NVA publications
#'
#' Search the Norwegian National Research Archive for publications matching
#' a query string.
#'
#' @param query Search query string
#' @param limit Number of results to return per page (default: 10, max: 100)
#' @param offset Offset for pagination (default: 0)
#' @param sort Sort order. One of "relevance" (default), "modifiedDate",
#'   "createdDate", or "publishedDate"
#' @param organization Filter by organization Cristin ID (e.g., "185" for UiO)
#' @param year Filter by publication year (e.g., 2024 or "2020-2024")
#' @param type Filter by publication type (e.g., "AcademicArticle")
#' @param fetch_all If TRUE, fetch all matching results (may be slow for large
#'   result sets). Default: FALSE
#' @param max_results Maximum results to fetch when `fetch_all = TRUE` (default: Inf)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{identifier}{Publication identifier}
#'   \item{title}{Main title}
#'   \item{type}{Publication type}
#'   \item{year}{Publication year}
#'   \item{status}{Publication status}
#'   \item{contributors}{List of contributor names}
#'   \item{institutions}{List of top-level institution names}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple search
#' nva_search("climate change")
#'
#' # Search with filters
#' nva_search("machine learning", year = 2024, limit = 20)
#'
#' # Fetch all results for a specific organization
#' nva_search("biodiversity", organization = "185", fetch_all = TRUE)
#' }
nva_search <- function(query,
                       limit = 10L,
                       offset = 0L,
                       sort = c("relevance", "modifiedDate", "createdDate", "publishedDate"),
                       organization = NULL,
                       year = NULL,
                       type = NULL,
                       fetch_all = FALSE,
                       max_results = Inf) {
  sort <- rlang::arg_match(sort)

  if (fetch_all) {
    tbl <- nva_fetch_all(
      "search/resources",
      query = query,
      sort = sort,
      institution = organization,
      year = year,
      instanceType = type,
      results_per_page = 100L,
      max_results = max_results
    )
  } else {
    tbl <- nva_get_tibble(
      "search/resources",
      query = query,
      results = limit,
      from = offset,
      sort = sort,
      institution = organization,
      year = year,
      instanceType = type
    )
  }

  if (nrow(tbl) == 0) {
    return(schema_publication_search())
  }

  nva_parse_search_results(tbl)
}

#' Parse raw search results into clean tibble
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble with selected columns
#' @noRd
nva_parse_search_results <- function(tbl) {
  tibble::tibble(
    identifier = tbl$identifier,
    title = purrr::map_chr(tbl$entityDescription, \(x) x$mainTitle %||% NA_character_),
    type = purrr::map_chr(tbl$entityDescription, \(x) {
      ref <- x$reference
      if (is.null(ref)) return(NA_character_)
      ref$publicationInstance$type %||% NA_character_
    }),
    year = purrr::map_int(tbl$entityDescription, \(x) {
      pd <- x$publicationDate
      if (is.null(pd) || is.null(pd$year)) return(NA_integer_)
      as.integer(pd$year)
    }),
    status = tbl$status,
    contributors = purrr::map(tbl$entityDescription, \(x) {
      contribs <- x$contributorsPreview %||% x$contributors %||% list()
      purrr::map_chr(contribs, \(c) c$identity$name %||% NA_character_)
    }),
    institutions = purrr::map(tbl$topLevelOrganizations, \(orgs) {
      if (is.null(orgs)) return(character())
      purrr::map_chr(orgs, \(o) nva_get_label(o$labels))
    })
  )
}
