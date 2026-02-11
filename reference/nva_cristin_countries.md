# Get Cristin countries

Retrieves the list of available countries from the Cristin registry.

## Usage

``` r
nva_cristin_countries()
```

## Value

A tibble with columns:

- code:

  Country code

- name:

  Country name (prefers English, falls back to Norwegian)

## Examples

``` r
if (FALSE) { # \dontrun{
countries <- nva_cristin_countries()
} # }
```
