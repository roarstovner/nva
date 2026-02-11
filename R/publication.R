#' Get a publication by identifier
#'
#' Retrieves detailed information about a single publication from the NVA API.
#'
#' @param id Publication identifier (the UUID-based NVA ID)
#'
#' @return A list containing the full publication record, including:
#' \describe{
#'   \item{identifier}{Publication identifier}
#'   \item{entityDescription}{Title, contributors, publication date, etc.}
#'   \item{status}{Publication status}
#'   \item{associatedArtifacts}{Files and links associated with the publication}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get a specific publication
#' pub <- nva_publication("01907b56-a6b0-7b8c-8f79-12345abcdef")
#'
#' # Access the title
#' pub$entityDescription$mainTitle
#' }
nva_publication <- function(id) {
  if (missing(id) || is.null(id) || !nzchar(id)) {
    cli::cli_abort("{.arg id} must be a non-empty string.")
  }

  nva_get(paste0("publication/", id))
}

#' Get multiple publications by identifiers
#'
#' Retrieves detailed information about multiple publications in a single call.
#'
#' @param ids Character vector of publication identifiers
#'
#' @return A tibble with one row per publication containing:
#' \describe{
#'   \item{identifier}{Publication identifier}
#'   \item{title}{Main title}
#'   \item{type}{Publication type}
#'   \item{year}{Publication year}
#'   \item{status}{Publication status}
#'   \item{contributors}{List of contributor names}
#'   \item{institutions}{List of top-level institution names}
#'   \item{doi}{DOI if available}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get multiple publications
#' ids <- c("01907b56-a6b0-7b8c-8f79-12345abcdef",
#'          "01907b56-a6b0-7b8c-8f79-67890fedcba")
#' pubs <- nva_publications(ids)
#' }
nva_publications <- function(ids) {
  # Maintain original validation: publications requires character input
  if (!is.character(ids) || length(ids) == 0) {
    cli::cli_abort("{.arg ids} must be a non-empty character vector.")
  }

  nva_fetch_multiple(
    ids = ids,
    fetch_fn = nva_publication,
    parse_fn = nva_parse_publications,
    empty_schema = schema_publication_detail,
    resource_name = "publication"
  )
}

#' Parse publication list into tibble
#'
#' @param pubs List of publication records
#'
#' @return Cleaned tibble
#' @noRd
nva_parse_publications <- function(pubs) {
  tibble::tibble(
    identifier = purrr::map_chr(pubs, \(x) x$identifier %||% NA_character_),
    title = purrr::map_chr(pubs, \(x) {
      x$entityDescription$mainTitle %||% NA_character_
    }),
    type = purrr::map_chr(pubs, \(x) {
      ref <- x$entityDescription$reference
      if (is.null(ref)) return(NA_character_)
      ref$publicationInstance$type %||% NA_character_
    }),
    year = purrr::map_int(pubs, \(x) {
      pd <- x$entityDescription$publicationDate
      if (is.null(pd) || is.null(pd$year)) return(NA_integer_)
      as.integer(pd$year)
    }),
    status = purrr::map_chr(pubs, \(x) x$status %||% NA_character_),
    contributors = purrr::map(pubs, \(x) {
      contribs <- x$entityDescription$contributors %||% list()
      purrr::map_chr(contribs, \(c) c$identity$name %||% NA_character_)
    }),
    institutions = purrr::map(pubs, \(x) {
      orgs <- x$topLevelOrganizations %||% list()
      if (length(orgs) == 0) return(character())
      purrr::map_chr(orgs, \(o) nva_get_label(o$labels))
    }),
    doi = purrr::map_chr(pubs, \(x) {
      ref <- x$entityDescription$reference
      if (is.null(ref)) return(NA_character_)
      ref$doi %||% NA_character_
    })
  )
}

#' Get files associated with a publication
#'
#' Extracts file information from a publication record.
#'
#' @param id Publication identifier
#'
#' @return A tibble with file information:
#' \describe{
#'   \item{identifier}{File identifier}
#'   \item{name}{File name}
#'   \item{mimetype}{MIME type}
#'   \item{size}{File size in bytes}
#'   \item{license}{License URI if available}
#'   \item{administrative_agreement}{Whether file is under admin agreement}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get files for a publication
#' files <- nva_publication_files("01907b56-a6b0-7b8c-8f79-12345abcdef")
#' }
nva_publication_files <- function(id) {
  pub <- nva_publication(id)

  artifacts <- pub$associatedArtifacts %||% list()

  if (length(artifacts) == 0) {
    return(schema_publication_file())
  }

  # Filter to only file types (exclude links)
  files <- purrr::keep(artifacts, \(x) {
    type <- x$type %||% ""
    type %in% c("PublishedFile", "UnpublishedFile", "UnpublishableFile")
  })

  if (length(files) == 0) {
    return(schema_publication_file())
  }

  tibble::tibble(
    identifier = purrr::map_chr(files, \(x) x$identifier %||% NA_character_),
    name = purrr::map_chr(files, \(x) x$name %||% NA_character_),
    mimetype = purrr::map_chr(files, \(x) x$mimeType %||% NA_character_),
    size = purrr::map_int(files, \(x) as.integer(x$size %||% NA_integer_)),
    license = purrr::map_chr(files, \(x) x$license %||% NA_character_),
    administrative_agreement = purrr::map_lgl(files, \(x) {
      isTRUE(x$administrativeAgreement)
    })
  )
}

#' Download a file from a publication
#'
#' Downloads a file associated with a publication to a local path.
#'
#' @param id File identifier (from `nva_publication_files()`)
#' @param destfile Destination file path. If NULL, uses the original filename
#'   in the current working directory.
#' @param overwrite Logical. Should existing files be overwritten? Default FALSE.
#' @param progress Logical. Show download progress bar? Default TRUE for
#'
#' @return Invisibly returns the path to the downloaded file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get files for a publication
#' files <- nva_publication_files("01907b56-a6b0-7b8c-8f79-12345abcdef")
#'
#' # Download the first file
#' nva_download_file(files$identifier[1], destfile = "paper.pdf")
#'
#' # Download with original filename
#' nva_download_file(files$identifier[1])
#' }
nva_download_file <- function(id, destfile = NULL, overwrite = FALSE,
                              progress = TRUE) {
  if (missing(id) || is.null(id) || !nzchar(id)) {
    cli::cli_abort("{.arg id} must be a non-empty string.")
  }

  if (is.null(destfile)) {
    cli::cli_abort(
      "{.arg destfile} must be specified (automatic filename detection not yet supported)."
    )
  }

  if (file.exists(destfile) && !overwrite) {
    cli::cli_abort(
      c("File {.path {destfile}} already exists.",
        "i" = "Use {.code overwrite = TRUE} to overwrite.")
    )
  }

  req <- nva_request(paste0("download/public/", id))

  if (progress) {
    req <- httr2::req_progress(req)
  }

  httr2::req_perform(req, path = destfile)

  cli::cli_alert_success("Downloaded to {.path {destfile}}")
  invisible(destfile)
}
