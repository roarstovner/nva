# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [0.1.0] - 2026-02-06

### Added
- Add README with dynamic code examples (#48)
- Add GitHub Actions workflow for pkgdown site deployment (#47)
- Add getting-started vignette (#39)
- Add GitHub Actions CI (#42)
- Add pkgdown site configuration (#40)
- Add advanced search parameters to nva_search() (#38)
- Add plural tibble getters for organizations and projects to match nva_cristin_persons pattern (#35)
- Add comprehensive test coverage for all endpoints (#24)
- Create nva R package wrapping the NVA API (#1)
- Add documentation and tests (#6)
- Add file download support (nva_download_file) (#5)
- Add tests for Cristin organization endpoints (#11)
- Add tests for Cristin person endpoints (#10)
- Add tests for publication endpoints (#9)
- Add tests for nva_search (#8)
- Utility functions: `nva_extract_id()`, `nva_get_label()`, `nva_empty_tibble()`, `nva_get_tibble()` (#12)
- Schema functions for consistent empty result structures (#12)
- Set up testthat infrastructure and mock helpers (#7)
- Implement extended Cristin endpoints (projects, keywords, funding) (#4)

### Fixed
- Fix missing dplyr and withr in test Suggests (#44)
- Fix year parameter from 'year' to 'publicationYear' in nva_search (#33)

### Changed
- Add .Rbuildignore for dev files (#45)
- Add tests for schema functions in schemas.R (#43)
- Fix user-agent and add DESCRIPTION metadata (#41)
- Add nva_search_aggregations() as separate function for search facet counts (#36)
- Add aggregation support to nva_search (#34)
- Add tests for NFR verified funding endpoints (#27)
- Add tests for authentication functions (#30)
- Add tests for API error handling (#29)
- Add tests for fetch_all pagination functionality (#28)
- Add tests for Cristin keyword and funding source endpoints (#26)
- Add tests for Cristin project endpoints (#25)
- **BREAKING**: Rename `results` parameter to `limit` across all functions (#12)
- **BREAKING**: Rename `from` parameter to `offset` for NVA endpoints (#12)
- **BREAKING**: Rename resource ID parameters to `id` (was `cristin_id`, `project_id`, etc.) (#12)
- **BREAKING**: Rename `identifier`/`identifiers` params to `id`/`ids` in publication functions (#12)
- **BREAKING**: Rename `institution` parameter to `organization` (#12)
- **BREAKING**: Rename search functions to use `_search` suffix:
  - `nva_cristin_projects()` → `nva_cristin_project_search()`
  - `nva_cristin_keywords()` → `nva_cristin_keyword_search()`
  - `nva_cristin_funding_sources()` → `nva_cristin_funding_source_search()`
  - `nva_verified_funding_nfr()` → `nva_verified_funding_nfr_search()`
  - `nva_verified_funding_nfr_item()` → `nva_verified_funding_nfr()` (#12)
- **BREAKING**: Output columns now use snake_case: `first_name`, `last_name`, `start_date`, `end_date`, `administrative_agreement` (#12)
- Implement core endpoints (search, publication, basic Cristin) (#3)
- Set up core infrastructure (utils-request.R, auth.R, utils-response.R) (#2)
