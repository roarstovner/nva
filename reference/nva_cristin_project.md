# Get a Cristin project by ID

Retrieves information about a specific project from the Cristin
registry.

## Usage

``` r
nva_cristin_project(id)
```

## Arguments

- id:

  Cristin project identifier

## Value

A list containing the project record with fields like title, status,
participants, funding, etc.

## Examples

``` r
if (FALSE) { # \dontrun{
project <- nva_cristin_project(123456)
project$title
} # }
```
