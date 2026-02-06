#' Search NVA publications
#'
#' Search the Norwegian National Research Archive for publications matching
#' a query string.
#'
#' @param query Search query string
#' @param limit Number of results to return per page (default: 10, max: 100)
#' @param offset Offset for pagination (default: 0)
#' @param sort Sort order. One of "relevance" (default), "modifiedDate",
#'   "createdDate", "publishedDate", "title", "category", "publicationDate",
#'   or "unitId"
#' @param organization Filter by organization Cristin ID (e.g., "185" for UiO)
#' @param year Filter by publication year. Supports single year (e.g., 2024) or
#'   range as comma-separated values (e.g., "2020,2024" for years 2020-2024)
#' @param type Filter by publication type (e.g., "AcademicArticle")
#' @param contributor Filter by Cristin person ID
#' @param contributor_name Filter by contributor name (fuzzy match)
#' @param doi Filter by DOI
#' @param title Filter by title text
#' @param abstract Filter by abstract text
#' @param tags Filter by tags/keywords
#' @param journal Filter by journal name, ID, or ISSN
#' @param publisher Filter by publisher name or ID
#' @param issn Filter by ISSN
#' @param isbn Filter by ISBN
#' @param license Filter by license
#' @param files Filter by file status (e.g., "hasPublicFiles")
#' @param funding_source Filter by funding source text
#' @param funding_identifier Filter by funding project ID
#' @param scientific_value Filter by NPI level ("LevelOne", "LevelTwo")
#' @param scientific_index_status Filter by reporting status
#' @param created_since Filter by creation date (ISO 8601)
#' @param modified_since Filter by modification date (ISO 8601)
#' @param published_since Filter by publication date (ISO 8601)
#' @param status Filter by publication status
#' @param orcid Filter by ORCID
#' @param project Filter by Cristin project ID
#' @param unit Filter by organizational unit ID
#' @param ... Additional API query parameters passed directly (see
#'   [NVA Search API docs](https://github.com/BIBSYSDEV/nva-search-api))
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
#'
#' # Filter by DOI
#' nva_search("climate", doi = "10.1371")
#'
#' # Filter by NPI level
#' nva_search("machine learning", scientific_value = "LevelTwo")
#'
#' # Only publications with public files, published since 2024
#' nva_search("AI", files = "hasPublicFiles", published_since = "2024")
#'
#' # Pass additional API parameters directly
#' nva_search("test", series = "MySeriesName")
#' }
nva_search <- function(query,
                       limit = 10L,
                       offset = 0L,
                       sort = c("relevance", "modifiedDate", "createdDate",
                                "publishedDate", "title", "category",
                                "publicationDate", "unitId"),
                       organization = NULL,
                       year = NULL,
                       type = NULL,
                       contributor = NULL,
                       contributor_name = NULL,
                       doi = NULL,
                       title = NULL,
                       abstract = NULL,
                       tags = NULL,
                       journal = NULL,
                       publisher = NULL,
                       issn = NULL,
                       isbn = NULL,
                       license = NULL,
                       files = NULL,
                       funding_source = NULL,
                       funding_identifier = NULL,
                       scientific_value = NULL,
                       scientific_index_status = NULL,
                       created_since = NULL,
                       modified_since = NULL,
                       published_since = NULL,
                       status = NULL,
                       orcid = NULL,
                       project = NULL,
                       unit = NULL,
                       ...,
                       fetch_all = FALSE,
                       max_results = Inf) {
  sort <- rlang::arg_match(sort)

  params <- nva_search_params(
    query = query, sort = sort, organization = organization, year = year,
    type = type, contributor = contributor, contributor_name = contributor_name,
    doi = doi, title = title, abstract = abstract, tags = tags,
    journal = journal, publisher = publisher, issn = issn, isbn = isbn,
    license = license, files = files, funding_source = funding_source,
    funding_identifier = funding_identifier,
    scientific_value = scientific_value,
    scientific_index_status = scientific_index_status,
    created_since = created_since, modified_since = modified_since,
    published_since = published_since, status = status, orcid = orcid,
    project = project, unit = unit, ...
  )

  if (fetch_all) {
    tbl <- rlang::inject(nva_fetch_all(
      "search/resources",
      !!!params,
      results_per_page = 100L,
      max_results = max_results
    ))
  } else {
    tbl <- rlang::inject(nva_get_tibble(
      "search/resources",
      !!!params,
      results = limit,
      from = offset
    ))
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
                                    sort = c("relevance", "modifiedDate",
                                             "createdDate", "publishedDate",
                                             "title", "category",
                                             "publicationDate", "unitId"),
                                    organization = NULL,
                                    year = NULL,
                                    type = NULL,
                                    contributor = NULL,
                                    contributor_name = NULL,
                                    doi = NULL,
                                    title = NULL,
                                    abstract = NULL,
                                    tags = NULL,
                                    journal = NULL,
                                    publisher = NULL,
                                    issn = NULL,
                                    isbn = NULL,
                                    license = NULL,
                                    files = NULL,
                                    funding_source = NULL,
                                    funding_identifier = NULL,
                                    scientific_value = NULL,
                                    scientific_index_status = NULL,
                                    created_since = NULL,
                                    modified_since = NULL,
                                    published_since = NULL,
                                    status = NULL,
                                    orcid = NULL,
                                    project = NULL,
                                    unit = NULL,
                                    ...) {
  sort <- rlang::arg_match(sort)

  params <- nva_search_params(
    query = query, sort = sort, organization = organization, year = year,
    type = type, contributor = contributor, contributor_name = contributor_name,
    doi = doi, title = title, abstract = abstract, tags = tags,
    journal = journal, publisher = publisher, issn = issn, isbn = isbn,
    license = license, files = files, funding_source = funding_source,
    funding_identifier = funding_identifier,
    scientific_value = scientific_value,
    scientific_index_status = scientific_index_status,
    created_since = created_since, modified_since = modified_since,
    published_since = published_since, status = status, orcid = orcid,
    project = project, unit = unit, ...
  )

  body <- rlang::inject(nva_get(
    "search/resources",
    !!!params,
    results = limit,
    from = offset,
    aggregation = "all"
  ))

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

#' Build search query parameters
#'
#' Maps R parameter names to NVA API query parameter names, dropping NULLs.
#'
#' @param query Search query string
#' @param sort Sort order
#' @param organization Organization Cristin ID
#' @param year Publication year
#' @param type Publication type
#' @param contributor Cristin person ID
#' @param contributor_name Contributor name
#' @param doi DOI
#' @param title Title text
#' @param abstract Abstract text
#' @param tags Tags/keywords
#' @param journal Journal name/ID/ISSN
#' @param publisher Publisher name/ID
#' @param issn ISSN
#' @param isbn ISBN
#' @param license License
#' @param files File status
#' @param funding_source Funding source
#' @param funding_identifier Funding project ID
#' @param scientific_value NPI level
#' @param scientific_index_status Reporting status
#' @param created_since Creation date filter
#' @param modified_since Modification date filter
#' @param published_since Publication date filter
#' @param status Publication status
#' @param orcid ORCID
#' @param project Cristin project ID
#' @param unit Organizational unit ID
#' @param ... Additional API parameters
#'
#' @return A named list of non-NULL query parameters
#' @noRd
nva_search_params <- function(query = NULL, sort = NULL, organization = NULL,
                              year = NULL, type = NULL, contributor = NULL,
                              contributor_name = NULL, doi = NULL,
                              title = NULL, abstract = NULL, tags = NULL,
                              journal = NULL, publisher = NULL, issn = NULL,
                              isbn = NULL, license = NULL, files = NULL,
                              funding_source = NULL,
                              funding_identifier = NULL,
                              scientific_value = NULL,
                              scientific_index_status = NULL,
                              created_since = NULL, modified_since = NULL,
                              published_since = NULL, status = NULL,
                              orcid = NULL, project = NULL, unit = NULL,
                              ...) {
  params <- list(
    query = query,
    sort = sort,
    institution = organization,
    publication_year = year,
    instanceType = type,
    contributor = contributor,
    contributorName = contributor_name,
    doi = doi,
    title = title,
    abstract = abstract,
    tags = tags,
    journal = journal,
    publisher = publisher,
    issn = issn,
    isbn = isbn,
    license = license,
    files = files,
    fundingSource = funding_source,
    fundingIdentifier = funding_identifier,
    scientificValue = scientific_value,
    scientificIndexStatus = scientific_index_status,
    createdSince = created_since,
    modifiedSince = modified_since,
    publishedSince = published_since,
    status = status,
    orcid = orcid,
    project = project,
    unit = unit,
    ...
  )

  # Drop NULL values
  params[!vapply(params, is.null, logical(1))]
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
