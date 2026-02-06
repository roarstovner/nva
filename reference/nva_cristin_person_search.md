# Search for Cristin persons

Search the Cristin person registry by name or other criteria.

## Usage

``` r
nva_cristin_person_search(
  query = NULL,
  organization = NULL,
  limit = 10L,
  page = 1L
)
```

## Arguments

- query:

  Name to search for (partial match supported)

- organization:

  Filter by organization Cristin ID

- limit:

  Number of results to return per page (default: 10, max: 100)

- page:

  Page number for pagination (default: 1)

## Value

A tibble with columns:

- id:

  Cristin person ID

- first_name:

  First name

- last_name:

  Last name

- affiliations:

  List of current affiliations

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for researchers by name
nva_cristin_person_search(query = "Hansen")

# Search within an organization
nva_cristin_person_search(query = "Olsen", organization = "185")
} # }
```
