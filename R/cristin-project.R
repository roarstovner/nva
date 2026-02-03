#' Search for Cristin projects
#'
#' Search the Cristin project registry.
#'
#' @param title Project title to search for
#' @param organization Cristin organization ID to filter by
#' @param keyword Keyword to search for
#' @param status Project status ("ACTIVE", "CONCLUDED", or "NOTSTARTED")
#' @param results Number of results per page (default: 10)
#' @param page Page number (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{Cristin project ID}
#'   \item{title}{Project title}
#'   \item{status}{Project status}
#'   \item{startDate}{Project start date}
#'   \item{endDate}{Project end date}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for projects by title
#' nva_cristin_projects(title = "climate")
#'
#' # Search for active projects
#' nva_cristin_projects(status = "ACTIVE")
#' }
nva_cristin_projects <- function(title = NULL,
                                  organization = NULL,
                                  keyword = NULL,
                                  status = NULL,
                                  results = 10L,
                                  page = 1L) {
  if (is.null(title) && is.null(organization) && is.null(keyword) && is.null(status)) {
    cli::cli_abort("At least one search parameter must be provided.")
  }

  resp <- nva_request(
    "cristin/project",
    title = title,
    institution = organization,
    keyword = keyword,
    status = status,
    results = results,
    page = page
  ) |>
    httr2::req_perform()

  tbl <- nva_resp_body_tibble(resp)

  if (nrow(tbl) == 0) {
    return(tibble::tibble(
      id = character(),
      title = character(),
      status = character(),
      startDate = character(),
      endDate = character()
    ))
  }

  nva_parse_cristin_projects(tbl)
}

#' Get a Cristin project by ID
#'
#' Retrieves information about a specific project from the Cristin registry.
#'
#' @param project_id Cristin project identifier
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
nva_cristin_project <- function(project_id) {
  if (missing(project_id) || is.null(project_id)) {
    cli::cli_abort("{.arg project_id} is required.")
  }

  project_id <- as.character(project_id)

  nva_get(paste0("cristin/project/", project_id))
}

#' Parse Cristin project search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_cristin_projects <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) {
      sub(".*/cristin/project/", "", x)
    }),
    title = purrr::map_chr(tbl$title, \(titles) {
      if (is.null(titles) || length(titles) == 0) return(NA_character_)
      # titles is a named list with language codes
      titles[[1]] %||% NA_character_
    }),
    status = tbl$status %||% NA_character_,
    startDate = purrr::map_chr(tbl$startDate, \(d) d %||% NA_character_),
    endDate = purrr::map_chr(tbl$endDate, \(d) d %||% NA_character_)
  )
}
