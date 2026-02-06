# Search NVA publications

Search the Norwegian National Research Archive for publications matching
a query string.

## Usage

``` r
nva_search(
  query,
  limit = 10L,
  offset = 0L,
  sort = c("relevance", "modifiedDate", "createdDate", "publishedDate", "title",
    "category", "publicationDate", "unitId"),
  organization = NULL,
  year = NULL,
  type = NULL,
  contributor = NULL,
  contributor_name = NULL,
  doi = NULL,
  title = NULL,
  abstract = NULL,
  tags = NULL,
  journal = NULL,
  publisher = NULL,
  issn = NULL,
  isbn = NULL,
  license = NULL,
  files = NULL,
  funding_source = NULL,
  funding_identifier = NULL,
  scientific_value = NULL,
  scientific_index_status = NULL,
  created_since = NULL,
  modified_since = NULL,
  published_since = NULL,
  status = NULL,
  orcid = NULL,
  project = NULL,
  unit = NULL,
  ...,
  fetch_all = FALSE,
  max_results = Inf
)
```

## Arguments

- query:

  Search query string

- limit:

  Number of results to return per page (default: 10, max: 100)

- offset:

  Offset for pagination (default: 0)

- sort:

  Sort order. One of "relevance" (default), "modifiedDate",
  "createdDate", "publishedDate", "title", "category",
  "publicationDate", or "unitId"

- organization:

  Filter by organization Cristin ID (e.g., "185" for UiO)

- year:

  Filter by publication year. Supports single year (e.g., 2024) or range
  as comma-separated values (e.g., "2020,2024" for years 2020-2024)

- type:

  Filter by publication type (e.g., "AcademicArticle")

- contributor:

  Filter by Cristin person ID

- contributor_name:

  Filter by contributor name (fuzzy match)

- doi:

  Filter by DOI

- title:

  Filter by title text

- abstract:

  Filter by abstract text

- tags:

  Filter by tags/keywords

- journal:

  Filter by journal name, ID, or ISSN

- publisher:

  Filter by publisher name or ID

- issn:

  Filter by ISSN

- isbn:

  Filter by ISBN

- license:

  Filter by license

- files:

  Filter by file status (e.g., "hasPublicFiles")

- funding_source:

  Filter by funding source text

- funding_identifier:

  Filter by funding project ID

- scientific_value:

  Filter by NPI level ("LevelOne", "LevelTwo")

- scientific_index_status:

  Filter by reporting status

- created_since:

  Filter by creation date (ISO 8601)

- modified_since:

  Filter by modification date (ISO 8601)

- published_since:

  Filter by publication date (ISO 8601)

- status:

  Filter by publication status

- orcid:

  Filter by ORCID

- project:

  Filter by Cristin project ID

- unit:

  Filter by organizational unit ID

- ...:

  Additional API query parameters passed directly (see [NVA Search API
  docs](https://github.com/BIBSYSDEV/nva-search-api))

- fetch_all:

  If TRUE, fetch all matching results (may be slow for large result
  sets). Default: FALSE

- max_results:

  Maximum results to fetch when `fetch_all = TRUE` (default: Inf)

## Value

A tibble with columns:

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

## See also

[`nva_search_aggregations()`](https://roarstovner.github.io/nva/reference/nva_search_aggregations.md)
for facet counts of search results

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple search
nva_search("climate change")

# Search with filters
nva_search("machine learning", year = 2024, limit = 20)

# Fetch all results for a specific organization
nva_search("biodiversity", organization = "185", fetch_all = TRUE)

# Filter by DOI
nva_search("climate", doi = "10.1371")

# Filter by NPI level
nva_search("machine learning", scientific_value = "LevelTwo")

# Only publications with public files, published since 2024
nva_search("AI", files = "hasPublicFiles", published_since = "2024")

# Pass additional API parameters directly
nva_search("test", series = "MySeriesName")
} # }
```
