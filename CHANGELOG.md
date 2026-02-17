# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/).

## \[0.1.0\] - 2026-02-06

### Added

- Reduce code duplication between nva_search and nva_search_aggregations
  parameter lists (#66)
- Add missing search filter parameters that the API supports (#74)
- Add input validation for ‘limit’ parameter across all search functions
  (#70)
- Fix schema functions not used consistently for empty returns (#76)
- Fix nva_cristin_organization_search not requiring any search parameter
  (#71)
- Add type column to nva_cristin_organization_search results (#78)
- Add wrapper for cristin/organization/{id}/projects endpoint (#53)
- Add wrapper for cristin/country endpoint (#56)
- Add documentation explaining singular vs plural function return types
  (#80)
- Add ‘sort_order’ parameter to nva_search for ascending/descending
  control (#72)
- Fix nva_cristin_project_search ‘keyword’ parameter to use ‘query’
  convention (#58)
- Add wrapper for cristin/category/project endpoint (#55)
- Add wrapper for cristin/organization/{id}/persons endpoint (#52)
- Add README with dynamic code examples (#48)
- Add GitHub Actions workflow for pkgdown site deployment (#47)
- Add getting-started vignette (#39)
- Add GitHub Actions CI (#42)
- Add pkgdown site configuration (#40)
- Add advanced search parameters to nva_search() (#38)
- Add plural tibble getters for organizations and projects to match
  nva_cristin_persons pattern (#35)
- Add comprehensive test coverage for all endpoints (#24)
- Create nva R package wrapping the NVA API (#1)
- Add documentation and tests (#6)
- Add file download support (nva_download_file) (#5)
- Add tests for Cristin organization endpoints (#11)
- Add tests for Cristin person endpoints (#10)
- Add tests for publication endpoints (#9)
- Add tests for nva_search (#8)
- Utility functions: `nva_extract_id()`, `nva_get_label()`,
  `nva_empty_tibble()`, `nva_get_tibble()` (#12)
- Schema functions for consistent empty result structures (#12)
- Set up testthat infrastructure and mock helpers (#7)
- Implement extended Cristin endpoints (projects, keywords, funding)
  (#4)

### Fixed

- Create nva R package wrapping the NVA API (#1)
- Fix inconsistent output columns between nva_search and
  nva_publications (#62)
- Fix nva_fetch_all using ‘results’ param instead of correct API param
  name (#69)
- Fix nva_verified_funding_nfr_search parameter naming: ‘nfr_project_id’
  should follow conventions (#60)
- Fix nva_download_file parameter naming: ‘file_id’ should be ‘id’ (#59)
- Fix nva_cristin_project_search title extraction fragility (#77)
- Fix nva_download_file not using nva_request() infrastructure (#68)
- Fix nva_download_file using hardcoded incorrect GitHub URL in user
  agent (#67)
- Fix pkgdown site URL to use correct GitHub username (#49)
- Fix missing dplyr and withr in test Suggests (#44)
- Fix year parameter from ‘year’ to ‘publicationYear’ in nva_search
  (#33)

### Changed

- Fix nva_search function name to follow naming convention (#73)
- Add .Rbuildignore for dev files (#45)
- Add tests for schema functions in schemas.R (#43)
- Fix user-agent and add DESCRIPTION metadata (#41)
- Add nva_search_aggregations() as separate function for search facet
  counts (#36)
- Add aggregation support to nva_search (#34)
- Add tests for NFR verified funding endpoints (#27)
- Add tests for authentication functions (#30)
- Add tests for API error handling (#29)
- Add tests for fetch_all pagination functionality (#28)
- Add tests for Cristin keyword and funding source endpoints (#26)
- Add tests for Cristin project endpoints (#25)
- **BREAKING**: Rename `results` parameter to `limit` across all
  functions (#12)
- **BREAKING**: Rename `from` parameter to `offset` for NVA endpoints
  (#12)
- **BREAKING**: Rename resource ID parameters to `id` (was `cristin_id`,
  `project_id`, etc.) (#12)
- **BREAKING**: Rename `identifier`/`identifiers` params to `id`/`ids`
  in publication functions (#12)
- **BREAKING**: Rename `institution` parameter to `organization` (#12)
- **BREAKING**: Rename search functions to use `_search` suffix:
  - [`nva_cristin_projects()`](https://roarstovner.github.io/nva/reference/nva_cristin_projects.md)
    →
    [`nva_cristin_project_search()`](https://roarstovner.github.io/nva/reference/nva_cristin_project_search.md)
  - `nva_cristin_keywords()` →
    [`nva_cristin_keyword_search()`](https://roarstovner.github.io/nva/reference/nva_cristin_keyword_search.md)
  - `nva_cristin_funding_sources()` →
    [`nva_cristin_funding_source_search()`](https://roarstovner.github.io/nva/reference/nva_cristin_funding_source_search.md)
  - [`nva_verified_funding_nfr()`](https://roarstovner.github.io/nva/reference/nva_verified_funding_nfr.md)
    →
    [`nva_verified_funding_nfr_search()`](https://roarstovner.github.io/nva/reference/nva_verified_funding_nfr_search.md)
  - `nva_verified_funding_nfr_item()` →
    [`nva_verified_funding_nfr()`](https://roarstovner.github.io/nva/reference/nva_verified_funding_nfr.md)
    (#12)
- **BREAKING**: Output columns now use snake_case: `first_name`,
  `last_name`, `start_date`, `end_date`, `administrative_agreement`
  (#12)
- Implement core endpoints (search, publication, basic Cristin) (#3)
- Set up core infrastructure (utils-request.R, auth.R, utils-response.R)
  (#2)
