# Get multiple Cristin projects by identifiers

Retrieves detailed information about multiple projects from the
single-item endpoint. Returns a tibble with the full project record.

## Usage

``` r
nva_cristin_projects(ids)
```

## Arguments

- ids:

  Character or numeric vector of Cristin project identifiers

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

- coordinating_institution:

  Cristin ID of coordinating institution

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a single project as a tibble
nva_cristin_projects(123456)

# Get multiple projects
nva_cristin_projects(c(123456, 789012))
} # }
```
