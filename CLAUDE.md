This is an R package that thinly wraps the API of https://nva.sikt.no.

## API Documentation

- **Swagger UI**: https://swagger-ui.nva.unit.no
- **Search API parameters**: https://raw.githubusercontent.com/BIBSYSDEV/nva-search-api/main/search-commons/src/main/java/no/unit/nva/search/resource/README.md
  - Documents all valid search parameters, sort keys, and query types

## Development

Use the R package httr2 - check out the docs, including the vignette "Wrapping APIs".

## Coding Conventions

### Function Naming
- All exported functions use `nva_` prefix
- Single-item getters: `nva_cristin_person(id)`, `nva_publication(id)`
- Search/list functions: use `_search` suffix - `nva_publication_search()`, `nva_cristin_person_search()`
- Related data: `nva_cristin_person_publications()`, `nva_publication_files()`
- Multi-item fetchers: plural form - `nva_publications(ids)`, `nva_cristin_persons(ids)`

### Parameter Naming
Use these standard parameter names consistently:
- `id` - Resource identifier (not `cristin_id`, `project_id`, etc.)
- `ids` - Multiple identifiers (not `identifiers`)
- `query` - Search text (not `name`, `title`, `keyword`)
- `limit` - Results per page (not `results`)
- `offset` - Pagination offset for NVA endpoints
- `page` - Page number for Cristin endpoints (they use page-based pagination)
- `organization` - Organization filter (not `institution`)

### Output Columns
- Use `snake_case` for all column names: `first_name`, `start_date`, `administrative_agreement`
- Keep `identifier` for NVA publication IDs (UUID format, distinct from Cristin numeric IDs)
- Use `id` for Cristin resource IDs

### HTTP Requests
- All HTTP requests MUST go through `nva_request()` in `utils-request.R`
- Never build requests with raw `httr2::request()` — this bypasses auth, throttling, retry, and error handling

### Label/Title Extraction
- Always use `nva_get_label(labels)` when extracting human-readable names from API responses
- This ensures consistent language preference (en > nb > nn) across the package
- Never pick a hardcoded index like `titles[[1]]` — use `nva_get_label()` instead

### Utility Functions (internal)
- `nva_request(endpoint, ...)` - Build authenticated, throttled request (ALL requests go through this)
- `nva_get(endpoint, ...)` - Request + perform + parse JSON
- `nva_get_tibble(endpoint, ...)` - Request + perform + convert to tibble
- `nva_extract_id(url, resource)` - Extract ID from API URLs
- `nva_get_label(labels)` - Get label with language preference (en > nb > nn)
- `nva_empty_tibble(...)` - Create typed empty tibble

### Empty Result Schemas
Use schema functions from `R/schemas.R` for consistent empty results:
```r
if (nrow(tbl) == 0) {
 return(schema_publication_search())
}
```

### Return Types
- Single-item getters (`nva_publication`, `nva_cristin_person`): Return raw list from API
- Search functions: Return tibble with parsed/cleaned data
- Search and detail tibbles for the same resource may have different columns (detail endpoints return more data)

### Input Validation
- Validate `limit` bounds (1-100) in search functions
- All single-item getters must validate that `id` is non-empty

