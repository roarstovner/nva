# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Utility functions: `nva_extract_id()`, `nva_get_label()`, `nva_empty_tibble()`, `nva_get_tibble()` (#12)
- Schema functions for consistent empty result structures (#12)
- Set up testthat infrastructure and mock helpers (#7)
- Implement extended Cristin endpoints (projects, keywords, funding) (#4)

### Fixed

### Changed
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
