#' @details
#' ## Return Types
#'
#' Functions in this package follow a consistent return type convention:
#'
#' - **Singular functions** (`nva_publication()`, `nva_cristin_person()`) return
#'   raw list objects from the API, preserving the full JSON structure for
#'   maximum flexibility.
#'
#' - **Plural functions** (`nva_publications()`, `nva_cristin_persons()`) return
#'   tibbles with parsed and cleaned data. These tibbles may have different
#'   columns than search results since detail endpoints provide more information.
#'
#' - **Search functions** (ending in `_search`, like `nva_publication_search()`)
#'   return tibbles with standardized columns optimized for browsing results.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
## usethis namespace: end
NULL

# Suppress R CMD check notes for tidyr column names
utils::globalVariables("data")
