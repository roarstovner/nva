# nva: R Client for the Norwegian National Research Archive (NVA) API

Provides functions for accessing the Norwegian National Research Archive
(NVA) API. Includes support for searching publications, retrieving
metadata, accessing Cristin data, and downloading files.

## Details

### Return Types

Functions in this package follow a consistent return type convention:

- **Singular functions**
  ([`nva_publication()`](https://roarstovner.github.io/nva/reference/nva_publication.md),
  [`nva_cristin_person()`](https://roarstovner.github.io/nva/reference/nva_cristin_person.md))
  return raw list objects from the API, preserving the full JSON
  structure for maximum flexibility.

- **Plural functions**
  ([`nva_publications()`](https://roarstovner.github.io/nva/reference/nva_publications.md),
  [`nva_cristin_persons()`](https://roarstovner.github.io/nva/reference/nva_cristin_persons.md))
  return tibbles with parsed and cleaned data. These tibbles may have
  different columns than search results since detail endpoints provide
  more information.

- **Search functions** (ending in `_search`, like
  [`nva_publication_search()`](https://roarstovner.github.io/nva/reference/nva_publication_search.md))
  return tibbles with standardized columns optimized for browsing
  results.

## See also

Useful links:

- <https://roarstovner.github.io/nva/>

- Report bugs at <https://github.com/roarstovner/nva/issues>

## Author

**Maintainer**: Roar Bakken Stovner <robast@oslomet.no>
