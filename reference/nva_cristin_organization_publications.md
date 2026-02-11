# Get publications for a Cristin organization

Retrieves publications associated with a specific Cristin organization.

## Usage

``` r
nva_cristin_organization_publications(
  id,
  limit = 10L,
  offset = 0L,
  year = NULL,
  type = NULL,
  fetch_all = FALSE,
  max_results = Inf
)
```

## Arguments

- id:

  Cristin organization identifier

- limit:

  Number of results to return per page (default: 10, max: 100)

- offset:

  Offset for pagination (default: 0)

- year:

  Filter by publication year. Supports single year (e.g., 2024) or range
  as comma-separated values (e.g., "2020,2024" for years 2020-2024)

- type:

  Filter by publication type

- fetch_all:

  If TRUE, fetch all publications. Default: FALSE

- max_results:

  Maximum results when `fetch_all = TRUE` (default: Inf)

## Value

A tibble with publication information (same format as
nva_publication_search)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get recent publications from UiO
pubs <- nva_cristin_organization_publications(185, year = 2024)

# Get all publications from a department
all_pubs <- nva_cristin_organization_publications("185.15.2.10", fetch_all = TRUE)
} # }
```
