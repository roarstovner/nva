# Get files associated with a publication

Extracts file information from a publication record.

## Usage

``` r
nva_publication_files(id)
```

## Arguments

- id:

  Publication identifier

## Value

A tibble with file information:

- identifier:

  File identifier

- name:

  File name

- mimetype:

  MIME type

- size:

  File size in bytes

- license:

  License URI if available

- administrative_agreement:

  Whether file is under admin agreement

## Examples

``` r
if (FALSE) { # \dontrun{
# Get files for a publication
files <- nva_publication_files("01907b56-a6b0-7b8c-8f79-12345abcdef")
} # }
```
