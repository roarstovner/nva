# Search NVA publications

Search the Norwegian National Research Archive for publications matching
a query string.

## Usage

``` r
nva_publication_search(
  query,
  limit = 10L,
  offset = 0L,
  sort = c("relevance", "modifiedDate", "createdDate", "publishedDate", "title",
    "category", "publicationDate", "unitId"),
  sort_order = c("desc", "asc"),
  organization = NULL,
  year = NULL,
  year_since = NULL,
  year_before = NULL,
  type = NULL,
  context_type = NULL,
  contributor = NULL,
  contributor_name = NULL,
  doi = NULL,
  title = NULL,
  abstract = NULL,
  tags = NULL,
  journal = NULL,
  publisher = NULL,
  series = NULL,
  issn = NULL,
  isbn = NULL,
  license = NULL,
  files = NULL,
  funding = NULL,
  funding_source = NULL,
  funding_identifier = NULL,
  scientific_value = NULL,
  scientific_index_status = NULL,
  scientific_report_period = NULL,
  created_since = NULL,
  created_before = NULL,
  modified_since = NULL,
  modified_before = NULL,
  published_since = NULL,
  published_before = NULL,
  publication_language = NULL,
  status = NULL,
  orcid = NULL,
  project = NULL,
  unit = NULL,
  top_level_organization = NULL,
  exclude_subunits = NULL,
  handle = NULL,
  cristin_identifier = NULL,
  scopus_identifier = NULL,
  course = NULL,
  vocabulary = NULL,
  ...,
  fetch_all = FALSE,
  max_results = Inf
)

nva_search(...)
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

- sort_order:

  Sort direction: "desc" (default) for descending or "asc" for
  ascending. Only applies when sort is specified (not for "relevance")

- organization:

  Filter by organization Cristin ID (e.g., "185" for UiO)

- year:

  Filter by publication year. Supports single year (e.g., 2024) or range
  as comma-separated values (e.g., "2020,2024" for years 2020-2024)

- year_since:

  Filter by minimum publication year (inclusive)

- year_before:

  Filter by maximum publication year (exclusive)

- type:

  Filter by publication type (e.g., "AcademicArticle")

- context_type:

  Filter by publication context type (e.g., "Journal", "Report",
  "Anthology", "Book")

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

- series:

  Filter by series name, ID, title, or ISSN

- issn:

  Filter by ISSN

- isbn:

  Filter by ISBN

- license:

  Filter by license

- files:

  Filter by file status (e.g., "hasPublicFiles")

- funding:

  Combined funding search (matches funding ID or source)

- funding_source:

  Filter by funding source text

- funding_identifier:

  Filter by funding project ID

- scientific_value:

  Filter by NPI level ("LevelOne", "LevelTwo")

- scientific_index_status:

  Filter by reporting status

- scientific_report_period:

  Filter by NVI reporting period year

- created_since:

  Filter by creation date lower bound (ISO 8601)

- created_before:

  Filter by creation date upper bound (ISO 8601)

- modified_since:

  Filter by modification date lower bound (ISO 8601)

- modified_before:

  Filter by modification date upper bound (ISO 8601)

- published_since:

  Filter by published date lower bound (ISO 8601)

- published_before:

  Filter by published date upper bound (ISO 8601)

- publication_language:

  Filter by language URI (e.g., "http://lexvo.org/id/iso639-3/eng")

- status:

  Filter by publication status

- orcid:

  Filter by ORCID

- project:

  Filter by Cristin project ID

- unit:

  Filter by organizational unit ID

- top_level_organization:

  Filter by top-level organization ID

- exclude_subunits:

  If TRUE, exclude publications from subunits when filtering by
  organization

- handle:

  Filter by handle URL

- cristin_identifier:

  Filter by Cristin identifier

- scopus_identifier:

  Filter by Scopus identifier

- course:

  Filter by course code

- vocabulary:

  Filter by subject vocabulary URI

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

- doi:

  DOI if available

## See also

[`nva_publication_search_aggregations()`](https://roarstovner.github.io/nva/reference/nva_publication_search_aggregations.md)
for facet counts of search results

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple search
nva_publication_search("climate change")

# Search with filters
nva_publication_search("machine learning", year = 2024, limit = 20)

# Fetch all results for a specific organization
nva_publication_search("biodiversity", organization = "185", fetch_all = TRUE)

# Filter by DOI
nva_publication_search("climate", doi = "10.1371")

# Filter by NPI level
nva_publication_search("machine learning", scientific_value = "LevelTwo")

# Only publications with public files, published since 2024
nva_publication_search("AI", files = "hasPublicFiles", published_since = "2024")

# Sort by publication date, ascending (oldest first)
nva_publication_search("climate", sort = "publicationDate", sort_order = "asc")

# Sort by modified date, descending (most recent first - default)
nva_publication_search("AI", sort = "modifiedDate", sort_order = "desc")

# Pass additional API parameters directly
nva_publication_search("test", series = "MySeriesName")
} # }
```
