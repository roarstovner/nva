#' Search verified NFR funding
#'
#' Search the Norwegian Research Council (NFR) verified funding registry.
#'
#' @param query Search query
#' @param project_id NFR project ID
#' @param limit Number of results per page (default: 10)
#' @param page Page number (default: 1)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{id}{NFR project identifier}
#'   \item{title}{Project title}
#'   \item{status}{Funding status}
#'   \item{amount}{Funding amount}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Search for NFR funding by query
#' nva_verified_funding_nfr_search(query = "climate")
#'
#' # Search by project ID
#' nva_verified_funding_nfr_search(project_id = "123456")
#' }
nva_verified_funding_nfr_search <- function(query = NULL,
                                             project_id = NULL,
                                             limit = 10L,
                                             page = 1L) {
  if (is.null(query) && is.null(project_id)) {
    cli::cli_abort("At least one of {.arg query} or {.arg project_id} must be provided.")
  }

  tbl <- nva_get_tibble(
    "verified-funding/nfr",
    query = query,
    projectId = project_id,
    results = limit,
    page = page
  )

  if (nrow(tbl) == 0) {
    return(schema_nfr_funding())
  }

  nva_parse_nfr_funding(tbl)
}

#' Get a specific NFR funding record
#'
#' Retrieves details about a specific NFR funding record.
#'
#' @param id NFR project identifier
#'
#' @return A list with NFR funding details including project info, participants, and amounts
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funding <- nva_verified_funding_nfr("123456")
#' funding$title
#' }
nva_verified_funding_nfr <- function(id) {
  if (missing(id) || is.null(id)) {
    cli::cli_abort("{.arg id} is required.")
  }

  id <- as.character(id)

  nva_get(paste0("verified-funding/nfr/", id))
}

#' Parse NFR funding search results
#'
#' @param tbl Raw tibble from API response
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_nfr_funding <- function(tbl) {
  tibble::tibble(
    id = purrr::map_chr(tbl$id, \(x) {
      if (is.null(x)) return(NA_character_)
      nva_extract_id(x, "nfr")
    }),
    title = purrr::map_chr(tbl$title, \(t) t %||% NA_character_),
    status = purrr::map_chr(tbl$status, \(s) s %||% NA_character_),
    amount = purrr::map_dbl(tbl$fundingAmount, \(a) as.numeric(a %||% NA_real_))
  )
}
