#' Get NVA API key
#'
#' Retrieves the NVA API key from the `NVA_API_KEY` environment variable.
#'
#' @param error_call The calling environment for error reporting
#'
#' @return The API key as a character string, or NULL if not set
#' @noRd
nva_api_key <- function(error_call = rlang::caller_env()) {
  key <- Sys.getenv("NVA_API_KEY", unset = "")
  if (key == "") {
    return(NULL)
  }
  key
}

#' Check if NVA API key is configured
#'
#' @return TRUE if the NVA_API_KEY environment variable is set, FALSE otherwise
#' @export
#'
#' @examples
#' nva_has_api_key()
nva_has_api_key <- function() {
  !is.null(nva_api_key())
}

#' Set NVA API key
#'
#' Sets the NVA API key for the current R session. For persistent configuration,
#' add `NVA_API_KEY=your_key` to your `.Renviron` file.
#'
#' @param key The API key to set
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' \dontrun{
#' nva_set_api_key("your-api-key-here")
#' }
nva_set_api_key <- function(key) {
  rlang::check_required(key)
  Sys.setenv(NVA_API_KEY = key)
  cli::cli_alert_success("NVA API key set for this session")
  invisible(NULL)
}
