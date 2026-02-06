# Search for Cristin organizations

Search the Cristin organization registry by name.

## Usage

``` r
nva_cristin_organization_search(
  query = NULL,
  depth = c("top", "full"),
  limit = 10L,
  page = 1L
)
```

## Arguments

- query:

  Search query string (name or acronym)

- depth:

  Depth of subunits to include. One of "top" (default) or "full"

- limit:

  Number of results to return per page (default: 10, max: 100)

- page:

  Page number for pagination (default: 1)

## Value

A tibble with columns:

- id:

  Cristin organization ID

- name:

  Organization name (prefers English, falls back to Norwegian)

- acronym:

  Organization acronym

- country:

  Country code

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for organizations by name
nva_cristin_organization_search("university")

# Search with more results
nva_cristin_organization_search("oslo", limit = 20)
} # }
```
