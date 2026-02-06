# Search funding sources from Cristin

Retrieves the list of available funding sources.

## Usage

``` r
nva_cristin_funding_source_search(limit = 100L, page = 1L)
```

## Arguments

- limit:

  Number of results per page (default: 100)

- page:

  Page number (default: 1)

## Value

A tibble with columns:

- id:

  Funding source identifier

- name:

  Funding source name

- acronym:

  Funding source acronym

## Examples

``` r
if (FALSE) { # \dontrun{
sources <- nva_cristin_funding_source_search()
} # }
```
