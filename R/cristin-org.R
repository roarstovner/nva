#' Get a Cristin organization by ID
#'
#' Retrieves information about an organization from the Cristin registry.
#'
#' @param cristin_id Cristin organization identifier (numeric or character).
#'   Top-level institution IDs are typically 3 digits (e.g., "185" for UiO).
#'
#' @return A list containing the organization record:
#' \describe{
#'   \item{id}{Cristin organization ID}
#'   \item{labels}{Organization name in different languages (nb, en, nn)}
#'   \item{acronym}{Organization acronym if available}
#'   \item{partOf}{Parent organization hierarchy}
#'   \item{hasPart}{Child organizations}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get University of Oslo
#' org <- nva_cristin_organization(185)
#' org$labels$en
#'
#' # Get a department
#' dept <- nva_cristin_organization("185.15.2.10")
#' }
nva_cristin_organization <- function(cristin_id) {
  if (missing(cristin_id) || is.null(cristin_id)) {
    cli::cli_abort("{.arg cristin_id} is required.")
  }

  cristin_id <- nva_normalize_org_id(cristin_id)

  nva_get(paste0("cristin/organization/", cristin_id))
}

#' Normalize Cristin organization ID to full format
#'
#' Converts short IDs (e.g., "185") to full format (e.g., "185.0.0.0")
#'
#' @param id Organization ID (short or full format)
#'
#' @return Full format organization ID
#' @noRd
nva_normalize_org_id <- function(id) {
  id <- as.character(id)
  parts <- strsplit(id, "\\.")[[1]]
  if (length(parts) < 4) {
    parts <- c(parts, rep("0", 4 - length(parts)))
  }
  paste(parts, collapse = ".")
}

#' Search for Cristin organizations
#'
#' Search the Cristin organization registry by name.
#'
#' @param query Search query string (name or acronym)
#' @param depth Depth of subunits to include. One of "top" (default) or "full"
#' @param results Number of results to return per page (default: 10, max: 100)
#' @param page Page number for pagination (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin organization ID}
#'   \item{name}{Organization name (prefers English, falls back to Norwegian)}
#'   \item{acronym}{Organization acronym}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for organizations by name
#' nva_cristin_organization_search("university")
#'
#' # Search with more results
#' nva_cristin_organization_search("oslo", results = 20)
#' }
nva_cristin_organization_search <- function(query = NULL,
                                             depth = c("top", "full"),
                                             results = 10L,
                                             page = 1L) {
  depth <- rlang::arg_match(depth)

  resp <- nva_request(
    "cristin/organization",
    query = query,
    depth = depth,
    results = results,
    page = page
  ) |>
    httr2::req_perform()

  tbl <- nva_resp_body_tibble(resp)

  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      id = character(),
      name = character(),
      acronym = character(),
      country = character()
    ))
  }

  nva_parse_cristin_organizations(tbl)
}

#' Parse Cristin organization search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_organizations <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) {
      # Extract ID from URL like "https://api.nva.unit.no/cristin/organization/185.0.0.0"
      sub(".*/cristin/organization/", "", x)
    }),
    name = purrr::map_chr(tbl$labels, \(x) {
      x$en %||% x$nb %||% x$nn %||% NA_character_
    }),
    acronym = if ("acronym" %in% names(tbl)) {
      as.character(tbl$acronym)
    } else {
      rep(NA_character_, nrow(tbl))
    }
  )
}

#' Get subunits of a Cristin organization
#'
#' Retrieves the organizational hierarchy below a given organization.
#'
#' @param cristin_id Cristin organization identifier
#' @param depth Maximum depth of subunits to retrieve (default: 1 for immediate children)
#'
#' @return A tibble with subunit information:
#' \describe{
#'   \item{id}{Cristin organization ID}
#'   \item{name}{Organization name}
#'   \item{acronym}{Organization acronym}
#'   \item{level}{Depth level from parent (1 = immediate child)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get faculties of UiO
#' faculties <- nva_cristin_organization_subunits(185)
#'
#' # Get deeper hierarchy (faculties and departments)
#' all_units <- nva_cristin_organization_subunits(185, depth = 2)
#' }
nva_cristin_organization_subunits <- function(cristin_id, depth = 1L) {
  if (missing(cristin_id) || is.null(cristin_id)) {
    cli::cli_abort("{.arg cristin_id} is required.")
  }

  # nva_cristin_organization already normalizes the ID
  org <- nva_cristin_organization(cristin_id)
  subunits <- org$hasPart %||% list()

  if (length(subunits) == 0) {
    return(tibble::tibble(
      id = character(),
      name = character(),
      acronym = character(),
      level = integer()
    ))
  }

  results <- list()
  results <- collect_subunits(subunits, level = 1L, depth = depth, results = results)

  if (length(results) == 0) {
    return(tibble::tibble(
      id = character(),
      name = character(),
      acronym = character(),
      level = integer()
    ))
  }

  tibble::tibble(
    id = purrr::map_chr(results, \(x) as.character(x$id)),
    name = purrr::map_chr(results, \(x) {
      labels <- x$labels %||% list()
      labels$en %||% labels$nb %||% labels$nn %||% NA_character_
    }),
    acronym = purrr::map_chr(results, \(x) x$acronym %||% NA_character_),
    level = purrr::map_int(results, \(x) x$level)
  )
}

#' Recursively collect subunits
#'
#' @param subunits List of subunit objects
#' @param level Current depth level
#' @param depth Maximum depth to collect
#' @param results Accumulator list
#'
#' @return List of subunit objects with level added
#' @noRd
collect_subunits <- function(subunits, level, depth, results) {
  for (unit in subunits) {
    unit$level <- level
    results <- c(results, list(unit))

    if (level < depth && !is.null(unit$hasPart) && length(unit$hasPart) > 0) {
      results <- collect_subunits(unit$hasPart, level + 1L, depth, results)
    }
  }
  results
}

#' Get publications for a Cristin organization
#'
#' Retrieves publications associated with a specific Cristin organization.
#'
#' @param cristin_id Cristin organization identifier
#' @param results Number of results to return per page (default: 10, max: 100)
#' @param from Offset for pagination (default: 0)
#' @param year Filter by publication year
#' @param type Filter by publication type
#' @param fetch_all If TRUE, fetch all publications. Default: FALSE
#' @param max_results Maximum results when `fetch_all = TRUE` (default: Inf)
#'
#' @return A tibble with publication information (same format as nva_search)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get recent publications from UiO
#' pubs <- nva_cristin_organization_publications(185, year = 2024)
#'
#' # Get all publications from a department
#' all_pubs <- nva_cristin_organization_publications("185.15.2.10", fetch_all = TRUE)
#' }
nva_cristin_organization_publications <- function(cristin_id,
                                                   results = 10L,
                                                   from = 0L,
                                                   year = NULL,
                                                   type = NULL,
                                                   fetch_all = FALSE,
                                                   max_results = Inf) {
  if (missing(cristin_id) || is.null(cristin_id)) {
    cli::cli_abort("{.arg cristin_id} is required.")
  }

  cristin_id <- as.character(cristin_id)

  if (fetch_all) {
    tbl <- nva_fetch_all(
      "search/resources",
      institution = cristin_id,
      year = year,
      instanceType = type,
      results_per_page = 100L,
      max_results = max_results
    )
  } else {
    resp <- nva_request(
      "search/resources",
      institution = cristin_id,
      year = year,
      instanceType = type,
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
