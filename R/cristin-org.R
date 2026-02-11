#' Get a Cristin organization by ID
#'
#' Retrieves information about an organization from the Cristin registry.
#'
#' @param id Cristin organization identifier (numeric or character).
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
nva_cristin_organization <- function(id) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- nva_normalize_org_id(id)

  nva_get(paste0("cristin/organization/", id))
}

#' Get multiple Cristin organizations by identifiers
#'
#' Retrieves detailed information about multiple organizations from the single-item
#' endpoint. Returns a tibble with the full organization record.
#'
#' @param ids Character or numeric vector of Cristin organization identifiers
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin organization ID}
#'   \item{name}{Organization name (prefers English, falls back to Norwegian)}
#'   \item{acronym}{Organization acronym}
#'   \item{country}{Country code}
#'   \item{parent_id}{Parent organization ID, if applicable}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a single organization as a tibble
#' nva_cristin_organizations(185)
#'
#' # Get multiple organizations
#' nva_cristin_organizations(c(185, 194))
#' }
nva_cristin_organizations <- function(ids) {
  if (!is.character(ids) && !is.numeric(ids)) {
    cli::cli_abort("{.arg ids} must be a character or numeric vector.")
  }
  if (length(ids) == 0) {
    cli::cli_abort("{.arg ids} must be a non-empty vector.")
  }

  ids <- as.character(ids)

  nva_fetch_multiple(
    ids = ids,
    fetch_fn = nva_cristin_organization,
    parse_fn = nva_parse_cristin_organization_details,
    empty_schema = schema_cristin_organization_detail,
    resource_name = "organization"
  )
}

#' Parse Cristin organization detail records into tibble
#'
#' @param orgs List of organization records from the single-item API
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_organization_details <- function(orgs) {
  tibble::tibble(
    id = purrr::map_chr(orgs, \(o) nva_extract_id(o$id, "cristin/organization")),
    name = purrr::map_chr(orgs, \(o) nva_get_label(o$labels)),
    acronym = purrr::map_chr(orgs, \(o) o$acronym %||% NA_character_),
    country = purrr::map_chr(orgs, \(o) o$country %||% NA_character_),
    parent_id = purrr::map_chr(orgs, \(o) {
      if (!is.null(o$partOf) && length(o$partOf) > 0) {
        nva_extract_id(o$partOf[[1]]$id, "cristin/organization")
      } else {
        NA_character_
      }
    })
  )
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
#' @param limit Number of results to return per page (default: 10, max: 100)
#' @param page Page number for pagination (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin organization ID}
#'   \item{name}{Organization name (prefers English, falls back to Norwegian)}
#'   \item{acronym}{Organization acronym}
#'   \item{country}{Country code}
#'   \item{type}{Organization type (e.g., university, research institute)}
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
#' nva_cristin_organization_search("oslo", limit = 20)
#' }
nva_cristin_organization_search <- function(query = NULL,
                                             depth = c("top", "full"),
                                             limit = 10L,
                                             page = 1L) {
  if (is.null(query) || identical(query, "")) {
    cli::cli_abort("{.arg query} must be provided.")
  }

  if (limit < 1 || limit > 100) {
    cli::cli_abort("{.arg limit} must be between 1 and 100.")
  }

  depth <- rlang::arg_match(depth)

  tbl <- nva_get_tibble(
    "cristin/organization",
    query = query,
    depth = depth,
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_organization())
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
    id = purrr::map_chr(tbl$id, \(x) nva_extract_id(x, "cristin/organization")),
    name = purrr::map_chr(tbl$labels, nva_get_label),
    acronym = if ("acronym" %in% names(tbl)) {
      as.character(tbl$acronym)
    } else {
      rep(NA_character_, nrow(tbl))
    },
    country = if ("country" %in% names(tbl)) {
      as.character(tbl$country)
    } else {
      rep(NA_character_, nrow(tbl))
    },
    type = if ("institutionType" %in% names(tbl)) {
      purrr::map_chr(tbl$institutionType, \(x) {
        if (is.null(x) || length(x) == 0) {
          NA_character_
        } else if (is.list(x) && "labels" %in% names(x)) {
          nva_get_label(x$labels)
        } else {
          as.character(x)
        }
      })
    } else {
      rep(NA_character_, nrow(tbl))
    }
  )
}

#' Get subunits of a Cristin organization
#'
#' Retrieves the organizational hierarchy below a given organization.
#'
#' @param id Cristin organization identifier
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
nva_cristin_organization_subunits <- function(id, depth = 1L) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  org <- nva_cristin_organization(id)
  subunits <- org$hasPart %||% list()

  if (length(subunits) == 0) {
    return(schema_cristin_organization_subunit())
  }

  results <- list()
  results <- collect_subunits(subunits, level = 1L, depth = depth, results = results)

  if (length(results) == 0) {
    return(schema_cristin_organization_subunit())
  }

  tibble::tibble(
    id = purrr::map_chr(results, \(x) as.character(x$id)),
    name = purrr::map_chr(results, \(x) nva_get_label(x$labels)),
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
#' @param id Cristin organization identifier
#' @param limit Number of results to return per page (default: 10, max: 100)
#' @param offset Offset for pagination (default: 0)
#' @param year Filter by publication year. Supports single year (e.g., 2024) or
#'   range as comma-separated values (e.g., "2020,2024" for years 2020-2024)
#' @param type Filter by publication type
#' @param fetch_all If TRUE, fetch all publications. Default: FALSE
#' @param max_results Maximum results when `fetch_all = TRUE` (default: Inf)
#'
#' @return A tibble with publication information (same format as nva_publication_search)
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
nva_cristin_organization_publications <- function(id,
                                                   limit = 10L,
                                                   offset = 0L,
                                                   year = NULL,
                                                   type = NULL,
                                                   fetch_all = FALSE,
                                                   max_results = Inf) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  if (limit < 1 || limit > 100) {
    cli::cli_abort("{.arg limit} must be between 1 and 100.")
  }

  id <- as.character(id)

  if (fetch_all) {
    tbl <- nva_fetch_all(
      "search/resources",
      institution = id,
      publication_year = year,
      instanceType = type,
      results_per_page = 100L,
      max_results = max_results,
      empty_schema = schema_publication_search
    )
  } else {
    tbl <- nva_get_tibble(
      "search/resources",
      institution = id,
      publication_year = year,
      instanceType = type,
      results = limit,
      from = offset
    )
  }

  if (nrow(tbl) == 0) {
    return(schema_publication_search())
  }

  nva_parse_search_results(tbl)
}

#' Get persons affiliated with a Cristin organization
#'
#' Retrieves a list of persons affiliated with a specific organization.
#'
#' @param id Cristin organization identifier
#' @param limit Number of results per page (default: 10, max: 100)
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
#' # Get persons at University of Oslo
#' persons <- nva_cristin_organization_persons(185)
#'
#' # Get more results
#' persons <- nva_cristin_organization_persons(185, limit = 50, page = 2)
#' }
nva_cristin_organization_persons <- function(id, limit = 10L, page = 1L) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  if (limit < 1 || limit > 100) {
    cli::cli_abort("{.arg limit} must be between 1 and 100.")
  }

  id <- nva_normalize_org_id(id)

  tbl <- nva_get_tibble(
    paste0("cristin/organization/", id, "/persons"),
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_person())
  }

  nva_parse_cristin_persons(tbl)
}

#' Get projects associated with a Cristin organization
#'
#' Retrieves a list of projects associated with a specific organization.
#'
#' @param id Cristin organization identifier
#' @param limit Number of results per page (default: 5, max: 100)
#' @param page Page number for pagination (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin project ID}
#'   \item{title}{Project title}
#'   \item{status}{Project status}
#'   \item{start_date}{Project start date}
#'   \item{end_date}{Project end date}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get projects at University of Oslo
#' projects <- nva_cristin_organization_projects(185)
#'
#' # Get more results
#' projects <- nva_cristin_organization_projects(185, limit = 20, page = 2)
#' }
nva_cristin_organization_projects <- function(id, limit = 5L, page = 1L) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  if (limit < 1 || limit > 100) {
    cli::cli_abort("{.arg limit} must be between 1 and 100.")
  }

  id <- nva_normalize_org_id(id)

  tbl <- nva_get_tibble(
    paste0("cristin/organization/", id, "/projects"),
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_project())
  }

  nva_parse_cristin_projects(tbl)
}
