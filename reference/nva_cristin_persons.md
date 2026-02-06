# Get multiple Cristin persons by identifiers

Retrieves detailed information about multiple persons from the
single-item endpoint. Returns a tibble with the full person record,
including ORCID and preferred name — fields not available from the
search endpoint.

## Usage

``` r
nva_cristin_persons(ids)
```

## Arguments

- ids:

  Character or numeric vector of Cristin person identifiers

## Value

A tibble with columns:

- id:

  Cristin person ID

- first_name:

  First name

- last_name:

  Last name

- preferred_first_name:

  Preferred first name, if available

- orcid:

  ORCID identifier, if available

- affiliations:

  List of affiliations with organization ID and active status

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a single person as a tibble
nva_cristin_persons(788603)

# Get multiple persons
nva_cristin_persons(c(788603, 12345))
} # }
```
