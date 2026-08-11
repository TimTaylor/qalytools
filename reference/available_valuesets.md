# Available value sets

Generic function that lists value sets available.

## Usage

``` r
available_valuesets(x, ...)

# Default S3 method
available_valuesets(x, ...)

# S3 method for class 'EQ5D5L'
available_valuesets(x, ...)

# S3 method for class 'EQ5D3L'
available_valuesets(x, ...)

# S3 method for class 'EQ5DY3L'
available_valuesets(x, ...)

# S3 method for class 'character'
available_valuesets(x, ...)
```

## Arguments

- x:

  An R object.

- ...:

  Further arguments passed to or from other methods.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)
containing the available value sets for the given object or survey type.

## Details

`available_valuesets()` returns the available valuesets from the
[eq5d](https://cran.r-project.org/package=eq5d) package. It is an s3
generic that wraps the
[`eq5d::valuesets()`](https://rdrr.io/pkg/eq5d/man/valuesets.html)
function providing additional methods for
[`eq5d`](https://timtaylor.github.io/qalytools/reference/as_eq5d.md) and
`character` objects.

For `character` objects, the input is expected to be the survey type
with various forms permitted:

- "eq5d5l", "EQ5D5L", "eq-5d-5l", "EQ-5D-5L", "5L", "5l"

- "eq5d3l", "EQ5D3L", "eq-5d-3l", "EQ-5D-3L", "3L", "3l"

- "eq5dy3l" , "EQ5DY3L" , "eq-5d-y-3l" , "EQ-5D-Y-3L" , "Y3L" , "y3l"

If called with no arguments value sets for all available version, type
and country combination are returned.

## Examples

``` r

data(eq5d3l_example)
dat <- as_eq5d3l(
    eq5d3l_example,
    respondentID = "respondentID",
    surveyID = "surveyID",
    mobility = "MO",
    self_care = "SC",
    usual = "UA",
    pain = "PD",
    anxiety = "AD",
    vas = "vas"
)
available_valuesets(dat)
#> # A tibble: 104 × 8
#>    Version  Type  Country       PubMed DOI               ISBN  ExternalURL Notes
#>    <chr>    <chr> <chr>          <int> <chr>             <chr> <chr>       <chr>
#>  1 EQ-5D-3L DSU   China             NA NA                NA    https://sh… NA   
#>  2 EQ-5D-3L DSU   Germany           NA NA                NA    https://sh… NA   
#>  3 EQ-5D-3L DSU   Japan             NA NA                NA    https://sh… NA   
#>  4 EQ-5D-3L DSU   Netherlands       NA NA                NA    https://sh… NA   
#>  5 EQ-5D-3L DSU   SouthKorea        NA NA                NA    https://sh… NA   
#>  6 EQ-5D-3L DSU   Spain             NA NA                NA    https://sh… NA   
#>  7 EQ-5D-3L DSU   UK          36449173 10.1007/s40273-0… NA    https://sh… NA   
#>  8 EQ-5D-3L RCW   England           NA NA                NA    https://eu… Euro…
#>  9 EQ-5D-3L RCW   Germany           NA NA                NA    https://eu… Euro…
#> 10 EQ-5D-3L RCW   Netherlands       NA NA                NA    https://eu… Euro…
#> # ℹ 94 more rows
available_valuesets("eq5d5l")
#> # A tibble: 69 × 7
#>    Version  Type  Country       PubMed DOI                     ISBN  ExternalURL
#>    <chr>    <chr> <chr>          <int> <chr>                   <chr> <chr>      
#>  1 EQ-5D-5L CW    Bermuda     38982011 10.1007/s10198-024-017… NA    NA         
#>  2 EQ-5D-5L CW    Denmark     22867780 10.1016/j.jval.2012.02… NA    NA         
#>  3 EQ-5D-5L CW    France      22867780 10.1016/j.jval.2012.02… NA    NA         
#>  4 EQ-5D-5L CW    Germany     22867780 10.1016/j.jval.2012.02… NA    NA         
#>  5 EQ-5D-5L CW    Japan       22867780 10.1016/j.jval.2012.02… NA    NA         
#>  6 EQ-5D-5L CW    Jordan      39225720 10.1007/s10198-024-017… NA    NA         
#>  7 EQ-5D-5L CW    Netherlands 22867780 10.1016/j.jval.2012.02… NA    NA         
#>  8 EQ-5D-5L CW    Russia      33713323 10.1007/s11136-021-028… NA    NA         
#>  9 EQ-5D-5L CW    Spain       22867780 10.1016/j.jval.2012.02… NA    NA         
#> 10 EQ-5D-5L CW    Thailand    22867780 10.1016/j.jval.2012.02… NA    NA         
#> # ℹ 59 more rows
```
