# Calculate utility

Generic function that calculates EQ5D index scores for given value sets.

## Usage

``` r
calculate_utility(x, type, country, ...)

# Default S3 method
calculate_utility(x, type, country, ...)

# S3 method for class 'EQ5D5L'
calculate_utility(
  x,
  type = "VT",
  country = "England",
  drop = TRUE,
  age = NULL,
  sex = NULL,
  ...
)

# S3 method for class 'EQ5D3L'
calculate_utility(
  x,
  type = "VT",
  country = "England",
  drop = TRUE,
  age = NULL,
  sex = NULL,
  ...
)

# S3 method for class 'EQ5DY3L'
calculate_utility(
  x,
  type = "VT",
  country = "England",
  drop = TRUE,
  age = NULL,
  sex = NULL,
  ...
)

add_utility(x, type, country, ...)

# Default S3 method
add_utility(x, type, country, ...)

# S3 method for class 'EQ5D5L'
add_utility(x, type = "VT", country = "England", age = NULL, sex = NULL, ...)

# S3 method for class 'EQ5D3L'
add_utility(x, type = "VT", country = "England", age = NULL, sex = NULL, ...)

# S3 method for class 'EQ5DY'
add_utility(x, type = "VT", country = "England", age = NULL, sex = NULL, ...)
```

## Arguments

- x:

  An R object.

- type:

  `[character]`

  Method type(s) used for calculating the value sets.

  For EQ5D3L inputs this can be:

  - "TTO", the time trade-off valuation technique;

  - "VAS", the visual analogue scale valuation technique;

  - "RCW", a reverse crosswalk conversion to EQ5D5L values; or

  - "DSU", the NICE Decision Support Unit's model that allows mappings
    on to EQ5D5L values accounting for both age and sex.

  For EQ5D5L inputs this can be:

  - "VT", value sets generated via a EuroQol standardised valuation
    study protocol;

  - "CW", a crosswalk conversion EQ5D3L values; or

  - "DSU", the NICE Decision Support Unit's model that allows mappings
    on to EQ5D5L values accounting for both age and sex.

- country:

  `[character]`

  Value set countries to use.

- ...:

  Further arguments passed to or from other methods.

- drop:

  `[logical]`

  If TRUE (default), only columns corresponding to the surveyID and
  respondentID are kept from the input `x`.

- age:

  `[character]`

  Column in `x` representing the age, in years, of the respondent.

  Only used if `type = "DSU"`.

- sex:

  `[character]`

  Column in `x` representing the sex, in years, of the respondent.

  Only used if `type = "DSU"`.

  Column entries must be one of "Male", "M", "Female" or "F" (case
  insensitive).

## Value

A data frame of utility values linked to responses.

## Details

- `calculate_utility()` returns the utility index scores for a given
  object and value set combination. Methods that wrap functionality of
  the [eq5d](https://cran.r-project.org/package=eq5d) package are
  provided for EQ5D objects.

- `add_utility()` is a wrapper around `calculate_utility()` which keeps
  all columns of the input data `x`.

## Note

The methods for
[EQ5D](https://timtaylor.github.io/qalytools/reference/as_eq5d.md)
objects expect `type` and `country` to be of the same length but will
recycle those of length one to a common size.

## See also

[`available_valuesets()`](https://timtaylor.github.io/qalytools/reference/available_valuesets.md)
for the method / country combinations available for each survey type.

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
calculate_utility(dat, type = "TTO", country = c("UK", "Germany"))
#> # A tibble: 400 × 5
#>    respondentID surveyID .utility_country .utility_type .value
#>           <int> <fct>    <chr>            <chr>          <dbl>
#>  1            1 survey01 UK               TTO            0.76 
#>  2            2 survey01 UK               TTO            0.796
#>  3            3 survey01 UK               TTO           -0.003
#>  4            4 survey01 UK               TTO            0.796
#>  5            5 survey01 UK               TTO            0.656
#>  6            6 survey01 UK               TTO            1    
#>  7            7 survey01 UK               TTO            0.691
#>  8            8 survey01 UK               TTO            1    
#>  9            9 survey01 UK               TTO            0.656
#> 10           10 survey01 UK               TTO            0.779
#> # ℹ 390 more rows
```
