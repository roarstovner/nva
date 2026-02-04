#' Extract ID from NVA API URL
#'
#' Extracts the resource ID from URLs like:
#' `https://api.nva.unit.no/cristin/organization/185.0.0.0`
#'
#' @param url URL string or vector of URLs
#' @param resource Resource type path (e.g., "cristin/organization", "cristin/person")
#'
#' @return Character vector of extracted IDs
#' @noRd
nva_extract_id <- function(url, resource) {
  pattern <- paste0(".*/", resource, "/")
  sub(pattern, "", as.character(url))
}

#' Extract label with language preference
#'
#' Extracts a label from a named list with language codes, preferring
#' English, then Norwegian Bokmal, then Norwegian Nynorsk.
#'
#' @param labels Named list with language codes (en, nb, nn)
#' @param languages Character vector of language codes in preference order
#'
#' @return Character string or NA_character_
#' @noRd
nva_get_label <- function(labels, languages = c("en", "nb", "nn")) {
  if (is.null(labels)) return(NA_character_)

  for (lang in languages) {
    if (!is.null(labels[[lang]])) {
      return(labels[[lang]])
    }
  }

  # Fall back to first available
  if (length(labels) > 0) {
    return(labels[[1]])
  }

  NA_character_
}

#' Create empty tibble with specified column types
#'
#' @param ... Named type specifications. Names become column names,
#'   values are type indicators: "chr", "int", "dbl", "lgl", "list"
#'
#' @return An empty tibble with proper column types
#' @noRd
nva_empty_tibble <- function(...) {
  specs <- list(...)

  type_map <- list(
    chr = character(),
    int = integer(),
    dbl = double(),
    lgl = logical(),
    list = list()
  )

  cols <- purrr::map(specs, \(type) type_map[[type]])
  tibble::as_tibble(cols)
}

#' Convert NVA API response to tibble
#'
#' Converts a JSON response from the NVA API to a tibble. Handles both
#' paginated responses (with 'hits' field) and direct responses.
#'
#' @param resp An httr2 response object
#' @param flatten Whether to flatten nested list columns (default: TRUE)
#'
#' @return A tibble
#' @noRd
nva_resp_body_tibble <- function(resp, flatten = TRUE) {
  body <- httr2::resp_body_json(resp)
  nva_as_tibble(body, flatten = flatten)
}

#' Convert NVA response body to tibble
#'
#' @param body Parsed JSON body (list)
#' @param flatten Whether to flatten nested structures
#'
#' @return A tibble
#' @noRd
nva_as_tibble <- function(body, flatten = TRUE) {
  if (is.list(body) && "hits" %in% names(body)) {
    hits <- body$hits
    if (length(hits) == 0) {
      return(tibble::tibble())
    }
    tbl <- tibble::tibble(data = hits) |>
      tidyr::unnest_wider(data)
  } else if (is.list(body) && !is.data.frame(body)) {
    tbl <- tibble::as_tibble(body)
  } else {
    tbl <- tibble::as_tibble(body)
  }

  tbl
}

#' Extract pagination info from response
#'
#' @param body Parsed JSON body (list)
#'
#' @return A list with pagination details
#' @noRd
nva_pagination_info <- function(body) {
  list(
    total = body$totalHits %||% 0L,
    size = body$size %||% length(body$hits %||% list()),
    offset = body$offset %||% 0L,
    has_more = isTRUE((body$offset %||% 0L) + length(body$hits %||% list()) < (body$totalHits %||% 0L))
  )
}

#' Fetch all pages from a paginated endpoint
#'
#' @param endpoint API endpoint path
#' @param ... Additional query parameters
#' @param results_per_page Number of results per page (default: 100)
#' @param max_results Maximum total results to fetch (default: Inf)
#' @param progress Show progress bar (default: TRUE)
#'
#' @return A tibble with all results
#' @noRd
nva_fetch_all <- function(endpoint, ..., results_per_page = 100L, max_results = Inf, progress = TRUE) {
  offset <- 0L
  all_results <- list()
  total <- NULL

  if (progress) {
    cli::cli_progress_bar("Fetching", type = "tasks")
  }

  repeat {
    resp <- nva_request(endpoint, ..., results = results_per_page, offset = offset) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(resp)
    hits <- body$hits %||% list()

    if (is.null(total)) {
      total <- body$totalHits %||% length(hits)
      if (progress) {
        cli::cli_progress_update(total = min(total, max_results))
      }
    }

    if (length(hits) == 0) {
      break
    }

    all_results <- c(all_results, hits)

    if (progress) {
      cli::cli_progress_update(set = length(all_results))
    }

    if (length(all_results) >= max_results || length(all_results) >= total) {
      break
    }

    offset <- offset + results_per_page
  }

  if (progress) {
    cli::cli_progress_done()
  }

  if (length(all_results) == 0) {
    return(tibble::tibble())
  }

  if (length(all_results) > max_results) {
    all_results <- all_results[seq_len(max_results)]
  }

  tibble::tibble(data = all_results) |>
    tidyr::unnest_wider(data)
}
