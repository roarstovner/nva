# Get a Cristin person by ID

Retrieves information about a researcher from the Cristin registry.

## Usage

``` r
nva_cristin_person(id)
```

## Arguments

- id:

  Cristin person identifier (numeric or character)

## Value

A list containing the person record:

- id:

  Cristin person ID

- names:

  List with firstName and lastName

- affiliations:

  List of organizational affiliations

- identifiers:

  External identifiers (ORCID, etc.)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a researcher by Cristin ID
person <- nva_cristin_person(12345)

# Access their name
paste(person$names$firstName, person$names$lastName)
} # }
```
