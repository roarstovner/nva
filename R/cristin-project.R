#' Search for Cristin projects
#'
#' Search the Cristin project registry.
#'
#' @param query Project title to search for
#' @param organization Cristin organization ID to filter by
#' @param keyword Keyword to search for
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
#' # Search for active projects
#' nva_cristin_project_search(status = "ACTIVE")
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

#' Parse Cristin project search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_projects <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) nva_extract_id(x, "cristin/project")),
    title = purrr::map_chr(tbl$title, \(titles) {
      if (is.null(titles) || length(titles) == 0) return(NA_character_)
      titles[[1]] %||% NA_character_
    }),
    status = tbl$status %||% NA_character_,
    start_date = purrr::map_chr(tbl$startDate, \(d) d %||% NA_character_),
    end_date = purrr::map_chr(tbl$endDate, \(d) d %||% NA_character_)
  )
}
