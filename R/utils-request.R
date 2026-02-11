#' Create a base NVA API request
#'
#' @param endpoint API endpoint path (e.g., "search/resources")
#' @param ... Additional query parameters passed to the request
#'
#' @return An httr2 request object
#' @noRd
nva_request <- function(endpoint, ...) {
  req <- httr2::request("https://api.nva.unit.no") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_query(...) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent("nva R package (https://github.com/roarstovner/nva)") |>
    httr2::req_throttle(rate = 10 / 60) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L)
    ) |>
    httr2::req_error(body = nva_error_body)

  api_key <- nva_api_key(error_call = rlang::caller_env())
  if (!is.null(api_key)) {
    req <- httr2::req_auth_bearer_token(req, api_key)
  }

  req
}

#' Extract error message from NVA API response
#'
#' @param resp An httr2 response object
#'
#' @return A character string with the error message
#' @noRd
nva_error_body <- function(resp) {
  body <- httr2::resp_body_json(resp, check_type = FALSE)

  if (is.list(body) && !is.null(body$message)) {
    return(body$message)
  }
  if (is.list(body) && !is.null(body$error)) {
    return(body$error)
  }
  if (is.list(body) && !is.null(body$detail)) {
    return(body$detail)
  }


  NULL
}

#' Perform an NVA API request and return parsed response
#'
#' @param endpoint API endpoint path
#' @param ... Additional query parameters
#'
#' @return Parsed JSON response as a list
#' @noRd
nva_get <- function(endpoint, ...) {
  req <- nva_request(endpoint, ...)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Perform an NVA API request and return tibble
#'
#' Combines request, perform, and tibble conversion in one call.
#'
#' @param endpoint API endpoint path
#' @param ... Additional query parameters
#'
#' @return A tibble from the API response
#' @noRd
nva_get_tibble <- function(endpoint, ...) {
  nva_request(endpoint, ...) |>
    httr2::req_perform() |>
    nva_resp_body_tibble()
}
