# Download a file from a publication

Downloads a file associated with a publication to a local path.

## Usage

``` r
nva_download_file(id, destfile = NULL, overwrite = FALSE, progress = TRUE)
```

## Arguments

- id:

  File identifier (from
  [`nva_publication_files()`](https://roarstovner.github.io/nva/reference/nva_publication_files.md))

- destfile:

  Destination file path. If NULL, uses the original filename in the
  current working directory.

- overwrite:

  Logical. Should existing files be overwritten? Default FALSE.

- progress:

  Logical. Show download progress bar? Default TRUE for

## Value

Invisibly returns the path to the downloaded file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get files for a publication
files <- nva_publication_files("01907b56-a6b0-7b8c-8f79-12345abcdef")

# Download the first file
nva_download_file(files$identifier[1], destfile = "paper.pdf")

# Download with original filename
nva_download_file(files$identifier[1])
} # }
```
