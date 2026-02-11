# Search for Cristin projects

Search the Cristin project registry.

## Usage

``` r
nva_cristin_project_search(
  query = NULL,
  organization = NULL,
  keyword = NULL,
  status = NULL,
  limit = 10L,
  page = 1L
)
```

## Arguments

- query:

  Project title to search for (searches in project title field)

- organization:

  Cristin organization ID to filter by

- keyword:

  Keyword to search for (searches in project keyword tags, separate from
  title)

- status:

  Project status ("ACTIVE", "CONCLUDED", or "NOTSTARTED")

- limit:

  Number of results per page (default: 10)

- page:

  Page number (default: 1)

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
# Search for projects by title
nva_cristin_project_search(query = "climate")

# Search for projects by keyword tag
nva_cristin_project_search(keyword = "sustainability")

# Combine multiple search criteria
nva_cristin_project_search(query = "climate", status = "ACTIVE")
} # }
```
