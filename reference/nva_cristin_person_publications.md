# Get publications for a Cristin person

Retrieves publications associated with a specific Cristin person ID.

## Usage

``` r
nva_cristin_person_publications(
  id,
  limit = 10L,
  offset = 0L,
  year = NULL,
  fetch_all = FALSE,
  max_results = Inf
)
```

## Arguments

- id:

  Cristin person identifier

- limit:

  Number of results to return per page (default: 10, max: 100)

- offset:

  Offset for pagination (default: 0)

- year:

  Filter by publication year. Supports single year (e.g., 2024) or range
  as comma-separated values (e.g., "2020,2024" for years 2020-2024)

- fetch_all:

  If TRUE, fetch all publications. Default: FALSE

- max_results:

  Maximum results when `fetch_all = TRUE` (default: Inf)

## Value

A tibble with publication information (same format as nva_search)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get publications for a researcher
pubs <- nva_cristin_person_publications(12345)

# Get all publications
all_pubs <- nva_cristin_person_publications(12345, fetch_all = TRUE)
} # }
```
