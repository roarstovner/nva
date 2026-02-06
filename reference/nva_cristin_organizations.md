# Get multiple Cristin organizations by identifiers

Retrieves detailed information about multiple organizations from the
single-item endpoint. Returns a tibble with the full organization
record.

## Usage

``` r
nva_cristin_organizations(ids)
```

## Arguments

- ids:

  Character or numeric vector of Cristin organization identifiers

## Value

A tibble with columns:

- id:

  Cristin organization ID

- name:

  Organization name (prefers English, falls back to Norwegian)

- acronym:

  Organization acronym

- country:

  Country code

- parent_id:

  Parent organization ID, if applicable

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a single organization as a tibble
nva_cristin_organizations(185)

# Get multiple organizations
nva_cristin_organizations(c(185, 194))
} # }
```
