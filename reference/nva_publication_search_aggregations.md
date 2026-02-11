# Get aggregations (facet counts) for an NVA search

Returns facet counts for a search query, showing how results are
distributed across categories like publication type, institution, year,
etc. Accepts the same filter parameters as
[`nva_publication_search()`](https://roarstovner.github.io/nva/reference/nva_publication_search.md).

## Usage

``` r
nva_publication_search_aggregations(
  query,
  limit = 10L,
  offset = 0L,
  sort = c("relevance", "modifiedDate", "createdDate", "publishedDate", "title",
    "category", "publicationDate", "unitId"),
  sort_order = c("desc", "asc"),
  ...
)

nva_search_aggregations(...)
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

[`nva_publication_search()`](https://roarstovner.github.io/nva/reference/nva_publication_search.md)
for retrieving the publications themselves

## Examples

``` r
if (FALSE) { # \dontrun{
# See how "climate" results break down by type and institution
nva_publication_search_aggregations("climate")

# Filter to just publication type counts
nva_publication_search_aggregations("climate") |>
  dplyr::filter(aggregation == "type")

# Aggregations for a filtered search
nva_publication_search_aggregations("machine learning", year = 2024, organization = "185")
} # }
```
