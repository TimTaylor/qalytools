# Calculate limitation

Generic function that calculates the limitation of survey responses
across response dimensions. Here we define a limitation to be a
dimension value not equal to 1. Methods are provided for
[EQ5D](https://timtaylor.github.io/qalytools/reference/as_eq5d.md)
objects.

## Usage

``` r
calculate_limitation(x, ...)

# Default S3 method
calculate_limitation(x, ...)

# S3 method for class 'EQ5D'
calculate_limitation(x, ...)
```

## Arguments

- x:

  An R object.

- ...:

  Further arguments passed to or from other methods.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html) of
the fraction of individuals without limitation.

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
calculate_limitation(dat)
#> # A tibble: 5 × 3
#>   surveyID dimension without_limitation
#>   <fct>    <fct>                  <dbl>
#> 1 survey01 MO                     0.525
#> 2 survey01 SC                     0.6  
#> 3 survey01 UA                     0.395
#> 4 survey01 PD                     0.17 
#> 5 survey01 AD                     0.655
```
