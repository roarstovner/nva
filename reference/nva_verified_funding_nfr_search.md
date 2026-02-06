# Search verified NFR funding

Search the Norwegian Research Council (NFR) verified funding registry.

## Usage

``` r
nva_verified_funding_nfr_search(
  query = NULL,
  nfr_project_id = NULL,
  limit = 10L,
  page = 1L
)
```

## Arguments

- query:

  Search query

- nfr_project_id:

  NFR project ID

- limit:

  Number of results per page (default: 10)

- page:

  Page number (default: 1)

## Value

A tibble with columns:

- id:

  NFR project identifier

- title:

  Project title

- status:

  Funding status

- amount:

  Funding amount

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for NFR funding by query
nva_verified_funding_nfr_search(query = "climate")

# Search by project ID
nva_verified_funding_nfr_search(nfr_project_id = "123456")
} # }
```
