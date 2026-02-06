# Get aggregations (facet counts) for an NVA search

Returns facet counts for a search query, showing how results are
distributed across categories like publication type, institution, year,
etc. Accepts the same filter parameters as
[`nva_search()`](https://roarst.github.io/nva/reference/nva_search.md).

## Usage

``` r
nva_search_aggregations(
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
  ...
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

## Value

A tibble with columns:

- aggregation:

  Aggregation field name (e.g., "type", "topLevelOrganization")

- key:

  Bucket key (identifier or value)

- label:

  Human-readable label (English preferred, falls back to Norwegian)

- count:

  Number of matching publications

## Details

The API returns up to 100 buckets per aggregation field. Fields with
fewer natural categories (e.g., "type", "files", "license") return all
values, while high-cardinality fields (e.g., "contributor", "journal",
"topLevelOrganization") are capped at the top 100 by count. To get more
granular breakdowns, narrow your search with filters.

Available aggregation fields: `type`, `topLevelOrganization`,
`contributor`, `journal`, `series`, `publisher`, `fundingSource`,
`scientificIndex`, `license`, `files`.

## See also

[`nva_search()`](https://roarst.github.io/nva/reference/nva_search.md)
for retrieving the publications themselves

## Examples

``` r
if (FALSE) { # \dontrun{
# See how "climate" results break down by type and institution
nva_search_aggregations("climate")

# Filter to just publication type counts
nva_search_aggregations("climate") |>
  dplyr::filter(aggregation == "type")

# Aggregations for a filtered search
nva_search_aggregations("machine learning", year = 2024, organization = "185")
} # }
```
