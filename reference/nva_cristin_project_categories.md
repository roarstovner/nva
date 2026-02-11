# Get Cristin project categories

Retrieves the list of available project categories from the Cristin
registry.

## Usage

``` r
nva_cristin_project_categories()
```

## Value

A tibble with columns:

- code:

  Category code

- name:

  Category name (prefers English, falls back to Norwegian)

## Examples

``` r
if (FALSE) { # \dontrun{
categories <- nva_cristin_project_categories()
} # }
```
