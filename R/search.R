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
#' @param year Filter by publication year. Supports single year (e.g., 2024) or
#'   range as comma-separated values (e.g., "2020,2024" for years 2020-2024)
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
#' @seealso [nva_search_aggregations()] for facet counts of search results
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
      publication_year = year,
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
      publication_year = year,
      instanceType = type
    )
  }

  if (nrow(tbl) == 0) {
    return(schema_publication_search())
  }

  nva_parse_search_results(tbl)
}

#' Get aggregations (facet counts) for an NVA search
#'
#' Returns facet counts for a search query, showing how results are distributed
#' across categories like publication type, institution, year, etc. Accepts the
#' same filter parameters as [nva_search()].
#'
#' @inheritParams nva_search
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{aggregation}{Aggregation field name (e.g., "type", "topLevelOrganization")}
#'   \item{key}{Bucket key (identifier or value)}
#'   \item{label}{Human-readable label (English preferred, falls back to Norwegian)}
#'   \item{count}{Number of matching publications}
#' }
#'
#' @details
#' The API returns up to 100 buckets per aggregation field. Fields with fewer
#' natural categories (e.g., "type", "files", "license") return all values,
#' while high-cardinality fields (e.g., "contributor", "journal",
#' "topLevelOrganization") are capped at the top 100 by count. To get more
#' granular breakdowns, narrow your search with filters.
#'
#' Available aggregation fields: `type`, `topLevelOrganization`, `contributor`,
#' `journal`, `series`, `publisher`, `fundingSource`, `scientificIndex`,
#' `license`, `files`.
#'
#' @seealso [nva_search()] for retrieving the publications themselves
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # See how "climate" results break down by type and institution
#' nva_search_aggregations("climate")
#'
#' # Filter to just publication type counts
#' nva_search_aggregations("climate") |>
#'   dplyr::filter(aggregation == "type")
#'
#' # Aggregations for a filtered search
#' nva_search_aggregations("machine learning", year = 2024, organization = "185")
#' }
nva_search_aggregations <- function(query,
                                    limit = 10L,
                                    offset = 0L,
                                    sort = c("relevance", "modifiedDate", "createdDate", "publishedDate"),
                                    organization = NULL,
                                    year = NULL,
                                    type = NULL) {
  sort <- rlang::arg_match(sort)

  body <- nva_get(
    "search/resources",
    query = query,
    results = limit,
    from = offset,
    sort = sort,
    institution = organization,
    publication_year = year,
    instanceType = type,
    aggregation = "all"
  )

  aggs <- body$aggregations %||% list()

  if (length(aggs) == 0) {
    return(nva_empty_tibble(
      aggregation = "chr", key = "chr", label = "chr", count = "int"
    ))
  }

  purrr::map2(aggs, names(aggs), \(buckets, agg_name) {
    tibble::tibble(
      aggregation = agg_name,
      key = purrr::map_chr(buckets, "key"),
      label = purrr::map_chr(buckets, \(b) {
        if (!is.null(b$labels)) nva_get_label(b$labels) else b$key
      }),
      count = purrr::map_int(buckets, "count")
    )
  }) |>
    purrr::list_rbind()
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
