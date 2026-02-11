# Get projects associated with a Cristin organization

Retrieves a list of projects associated with a specific organization.

## Usage

``` r
nva_cristin_organization_projects(id, limit = 5L, page = 1L)
```

## Arguments

- id:

  Cristin organization identifier

- limit:

  Number of results per page (default: 5, max: 100)

- page:

  Page number for pagination (default: 1)

## Value

A tibble with columns:

- id:

  Cristin project ID

- title:

  Project title

- status:

  Project status

- start_date:

  Project start date

- end_date:

  Project end date

## Examples

``` r
if (FALSE) { # \dontrun{
# Get projects at University of Oslo
projects <- nva_cristin_organization_projects(185)

# Get more results
projects <- nva_cristin_organization_projects(185, limit = 20, page = 2)
} # }
```
