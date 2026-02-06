# Get a specific NFR funding record

Retrieves details about a specific NFR funding record.

## Usage

``` r
nva_verified_funding_nfr(id)
```

## Arguments

- id:

  NFR project identifier

## Value

A list with NFR funding details including project info, participants,
and amounts

## Examples

``` r
if (FALSE) { # \dontrun{
funding <- nva_verified_funding_nfr("123456")
funding$title
} # }
```
