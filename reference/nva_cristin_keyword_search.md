# Search for Cristin keywords

Search the Cristin keyword registry.

## Usage

``` r
nva_cristin_keyword_search(
  query = NULL,
  language = NULL,
  limit = 10L,
  page = 1L
)
```

## Arguments

- query:

  Keyword text to search for

- language:

  Language filter (e.g., "nb", "en")

- limit:

  Number of results per page (default: 10)

- page:

  Page number (default: 1)

## Value

A tibble with columns:

- id:

  Keyword ID

- label:

  Keyword text

- language:

  Language code

## Examples

``` r
if (FALSE) { # \dontrun{
nva_cristin_keyword_search(query = "machine learning")
} # }
```
