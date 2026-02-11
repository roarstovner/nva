#' Search for Cristin projects
#'
#' Search the Cristin project registry.
#'
#' @param query Project title to search for (searches in project title field)
#' @param organization Cristin organization ID to filter by
#' @param keyword Keyword to search for (searches in project keyword tags, separate from title)
#' @param status Project status ("ACTIVE", "CONCLUDED", or "NOTSTARTED")
#' @param limit Number of results per page (default: 10)
#' @param page Page number (default: 1)
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
#' # Search for projects by title
#' nva_cristin_project_search(query = "climate")
#'
#' # Search for projects by keyword tag
#' nva_cristin_project_search(keyword = "sustainability")
#'
#' # Combine multiple search criteria
#' nva_cristin_project_search(query = "climate", status = "ACTIVE")
#' }
nva_cristin_project_search <- function(query = NULL,
                                        organization = NULL,
                                        keyword = NULL,
                                        status = NULL,
                                        limit = 10L,
                                        page = 1L) {
  if (is.null(query) && is.null(organization) && is.null(keyword) && is.null(status)) {
    cli::cli_abort("At least one search parameter must be provided.")
  }

  if (limit < 1 || limit > 100) {
    cli::cli_abort("{.arg limit} must be between 1 and 100.")
  }

  tbl <- nva_get_tibble(
    "cristin/project",
    title = query,
    institution = organization,
    keyword = keyword,
    status = status,
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_cristin_project())
  }

  nva_parse_cristin_projects(tbl)
}

#' Get a Cristin project by ID
#'
#' Retrieves information about a specific project from the Cristin registry.
#'
#' @param id Cristin project identifier
#'
#' @return A list containing the project record with fields like title, status,
#'   participants, funding, etc.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' project <- nva_cristin_project(123456)
#' project$title
#' }
nva_cristin_project <- function(id) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- as.character(id)

  nva_get(paste0("cristin/project/", id))
}

#' Get multiple Cristin projects by identifiers
#'
#' Retrieves detailed information about multiple projects from the single-item
#' endpoint. Returns a tibble with the full project record.
#'
#' @param ids Character or numeric vector of Cristin project identifiers
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin project ID}
#'   \item{title}{Project title}
#'   \item{status}{Project status}
#'   \item{start_date}{Project start date}
#'   \item{end_date}{Project end date}
#'   \item{coordinating_institution}{Cristin ID of coordinating institution}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a single project as a tibble
#' nva_cristin_projects(123456)
#'
#' # Get multiple projects
#' nva_cristin_projects(c(123456, 789012))
#' }
nva_cristin_projects <- function(ids) {
  if (!is.character(ids) && !is.numeric(ids)) {
    cli::cli_abort("{.arg ids} must be a character or numeric vector.")
  }
  if (length(ids) == 0) {
    cli::cli_abort("{.arg ids} must be a non-empty vector.")
  }

  ids <- as.character(ids)

  nva_fetch_multiple(
    ids = ids,
    fetch_fn = nva_cristin_project,
    parse_fn = nva_parse_cristin_project_details,
    empty_schema = schema_cristin_project_detail,
    resource_name = "project"
  )
}

#' Parse Cristin project detail records into tibble
#'
#' @param projects List of project records from the single-item API
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_project_details <- function(projects) {
  tibble::tibble(
    id = purrr::map_chr(projects, \(p) nva_extract_id(p$id, "cristin/project")),
    title = purrr::map_chr(projects, \(p) nva_get_label(p$title)),
    status = purrr::map_chr(projects, \(p) p$status %||% NA_character_),
    start_date = purrr::map_chr(projects, \(p) p$startDate %||% NA_character_),
    end_date = purrr::map_chr(projects, \(p) p$endDate %||% NA_character_),
    coordinating_institution = purrr::map_chr(projects, \(p) {
      if (!is.null(p$coordinatingInstitution)) {
        nva_extract_id(p$coordinatingInstitution, "cristin/organization")
      } else {
        NA_character_
      }
    })
  )
}

#' Parse Cristin project search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_projects <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) nva_extract_id(x, "cristin/project")),
    title = purrr::map_chr(tbl$title, nva_get_label),
    status = tbl$status %||% NA_character_,
    start_date = purrr::map_chr(tbl$startDate, \(d) d %||% NA_character_),
    end_date = purrr::map_chr(tbl$endDate, \(d) d %||% NA_character_)
  )
}

#' Get Cristin project categories
#'
#' Retrieves the list of available project categories from the Cristin registry.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{Category code}
#'   \item{name}{Category name (prefers English, falls back to Norwegian)}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' categories <- nva_cristin_project_categories()
#' }
nva_cristin_project_categories <- function() {
  tbl <- nva_get_tibble("cristin/category/project")

  if (nrow(tbl) == 0) {
    return(schema_cristin_project_category())
  }

  nva_parse_cristin_project_categories(tbl)
}

#' Parse Cristin project category results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_project_categories <- function(tbl) {
  tibble::tibble(
    code = purrr::map_chr(tbl$code, \(x) x %||% NA_character_),
    name = purrr::map_chr(tbl$name, nva_get_label)
  )
}
