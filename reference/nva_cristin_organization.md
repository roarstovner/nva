# Get a Cristin organization by ID

Retrieves information about an organization from the Cristin registry.

## Usage

``` r
nva_cristin_organization(id)
```

## Arguments

- id:

  Cristin organization identifier (numeric or character). Top-level
  institution IDs are typically 3 digits (e.g., "185" for UiO).

## Value

A list containing the organization record:

- id:

  Cristin organization ID

- labels:

  Organization name in different languages (nb, en, nn)

- acronym:

  Organization acronym if available

- partOf:

  Parent organization hierarchy

- hasPart:

  Child organizations

## Examples

``` r
if (FALSE) { # \dontrun{
# Get University of Oslo
org <- nva_cristin_organization(185)
org$labels$en

# Get a department
dept <- nva_cristin_organization("185.15.2.10")
} # }
```
