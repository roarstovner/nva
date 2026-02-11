# Get multiple publications by identifiers

Retrieves detailed information about multiple publications in a single
call.

## Usage

``` r
nva_publications(ids)
```

## Arguments

- ids:

  Character vector of publication identifiers

## Value

A tibble with one row per publication containing:

- identifier:

  Publication identifier

- title:

  Main title

- type:

  Publication type

- year:

  Publication year

- status:

  Publication status

- contributors:

  List of contributor names

- institutions:

  List of top-level institution names

- doi:

  DOI if available

## Examples

``` r
if (FALSE) { # \dontrun{
# Get multiple publications
ids <- c("01907b56-a6b0-7b8c-8f79-12345abcdef",
         "01907b56-a6b0-7b8c-8f79-67890fedcba")
pubs <- nva_publications(ids)
} # }
```
