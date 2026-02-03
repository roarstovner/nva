#' Search verified NFR funding
#'
#' Search the Norwegian Research Council (NFR) verified funding registry.
#'
#' @param query Search query
#' @param project_id NFR project ID
#' @param results Number of results per page (default: 10)
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
#' nva_verified_funding_nfr(query = "climate")
#'
#' # Search by project ID
#' nva_verified_funding_nfr(project_id = "123456")
#' }
nva_verified_funding_nfr <- function(query = NULL,
                                      project_id = NULL,
                                      results = 10L,
                                      page = 1L) {
  if (is.null(query) && is.null(project_id)) {
    cli::cli_abort("At least one of {.arg query} or {.arg project_id} must be provided.")
  }

  resp <- nva_request(
    "verified-funding/nfr",
    query = query,
    projectId = project_id,
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
      amount = numeric()
    ))
  }

  nva_parse_nfr_funding(tbl)
}

#' Get a specific NFR funding record
#'
#' Retrieves details about a specific NFR funding record.
#'
#' @param nfr_id NFR project identifier
#'
#' @return A list with NFR funding details including project info, participants, and amounts
#'
#' @export
#'
#' @examples
#' \dontrun{
#' funding <- nva_verified_funding_nfr_item("123456")
#' funding$title
#' }
nva_verified_funding_nfr_item <- function(nfr_id) {
  if (missing(nfr_id) || is.null(nfr_id)) {
    cli::cli_abort("{.arg nfr_id} is required.")
  }

  nfr_id <- as.character(nfr_id)

  nva_get(paste0("verified-funding/nfr/", nfr_id))
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
      sub(".*/nfr/", "", x)
    }),
    title = purrr::map_chr(tbl$title, \(t) t %||% NA_character_),
    status = purrr::map_chr(tbl$status, \(s) s %||% NA_character_),
    amount = purrr::map_dbl(tbl$fundingAmount, \(a) as.numeric(a %||% NA_real_))
  )
}
