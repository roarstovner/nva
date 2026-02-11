# Get persons affiliated with a Cristin organization

Retrieves a list of persons affiliated with a specific organization.

## Usage

``` r
nva_cristin_organization_persons(id, limit = 10L, page = 1L)
```

## Arguments

- id:

  Cristin organization identifier

- limit:

  Number of results per page (default: 10, max: 100)

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
# Get persons at University of Oslo
persons <- nva_cristin_organization_persons(185)

# Get more results
persons <- nva_cristin_organization_persons(185, limit = 50, page = 2)
} # }
```
