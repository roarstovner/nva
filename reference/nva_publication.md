# Get a publication by identifier

Retrieves detailed information about a single publication from the NVA
API.

## Usage

``` r
nva_publication(id)
```

## Arguments

- id:

  Publication identifier (the UUID-based NVA ID)

## Value

A list containing the full publication record, including:

- identifier:

  Publication identifier

- entityDescription:

  Title, contributors, publication date, etc.

- status:

  Publication status

- associatedArtifacts:

  Files and links associated with the publication

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a specific publication
pub <- nva_publication("01907b56-a6b0-7b8c-8f79-12345abcdef")

# Access the title
pub$entityDescription$mainTitle
} # }
```
