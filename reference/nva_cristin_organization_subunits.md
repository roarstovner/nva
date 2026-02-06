# Get subunits of a Cristin organization

Retrieves the organizational hierarchy below a given organization.

## Usage

``` r
nva_cristin_organization_subunits(id, depth = 1L)
```

## Arguments

- id:

  Cristin organization identifier

- depth:

  Maximum depth of subunits to retrieve (default: 1 for immediate
  children)

## Value

A tibble with subunit information:

- id:

  Cristin organization ID

- name:

  Organization name

- acronym:

  Organization acronym

- level:

  Depth level from parent (1 = immediate child)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get faculties of UiO
faculties <- nva_cristin_organization_subunits(185)

# Get deeper hierarchy (faculties and departments)
all_units <- nva_cristin_organization_subunits(185, depth = 2)
} # }
```
