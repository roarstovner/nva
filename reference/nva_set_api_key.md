# Set NVA API key

Sets the NVA API key for the current R session. For persistent
configuration, add `NVA_API_KEY=your_key` to your `.Renviron` file.

## Usage

``` r
nva_set_api_key(key)
```

## Arguments

- key:

  The API key to set

## Value

Invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
nva_set_api_key("your-api-key-here")
} # }
```
