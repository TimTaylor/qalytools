# Calculate quality of life years

Generic for calculating quality of life years (QALY) metrics for EQ5D
survey respondents.

## Usage

``` r
calculate_qalys(x, ...)

# Default S3 method
calculate_qalys(x, ...)

# S3 method for class 'EQ5D'
calculate_qalys(
  x,
  time_index,
  type,
  country,
  units = c("days", "weeks", "months", "quarters", "years"),
  baseline_survey = NULL,
  ...
)

# S3 method for class 'utility'
calculate_qalys(
  x,
  time_index,
  units = c("days", "weeks", "months", "quarters", "years"),
  baseline_survey = NULL,
  ...
)
```

## Arguments

- x:

  An R object.

- ...:

  Further arguments passed to or from other methods.

- time_index:

  `[character]`

  Name of variable in x representing the relative time within the survey
  framework.

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

- units:

  [`character`](https://rdrr.io/r/base/character.html)

  The units of the `time_index` column of `x`. Can be one of "days",
  "weeks", "months", "quarters" or "years".

  Note that the output will always be a QALY (i.e years) irrespective of
  the unit input.

- baseline_survey:

  (optional)

  Either a `character` string specifying the surveyID, to use as a
  baseline or a data frame.

  If a data frame, it must have at least two columns; one for the
  respondentID (with name matching that in `x` input) and another (of
  any name) for the associated utility values. If desired you can also
  specify columns that match on the utility and country type columns of
  the input `x`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Details

The methods provided for
[utility](https://timtaylor.github.io/qalytools/reference/as_utility.md)
and [EQ5D](https://timtaylor.github.io/qalytools/reference/as_eq5d.md)
objects, return two metrics by default:

- Firstly, a `raw` QALY. This is the area under the utility curve scaled
  to the proportion of the year it corresponds to.

- Secondly, a `loss_v_fullhealth` value. This represents the loss from
  perfect health which is calculated by assuming all dimensions are 1 to
  calculate a full health QALY value.

- Optionally, a third metric can also be returned, `loss_v_baseline`.
  This represents the loss from a specified baseline utility value.

## Note

If a character string `baseline_survey` argument is given then this must
match a surveyID to match against. In this situation the survey **is**
still included in the unadjusted, `raw`, calculation, prior to the
calculation of loss.

Alternatively the `baseline_survey` argument can be specified as a data
frame with a column corresponding to the respondentID and another
representing the associated utility. Optionally columns corresponding to
the utility country and utility type can be included to allow more
granular comparisons. For this specification of baseline, it **is not**
included in the unadjusted, `raw`, calculation.

## Examples

``` r

data(EQ5D5L_surveys)
dat <- as_eq5d5l(
    EQ5D5L_surveys,
    surveyID = "surveyID",
    respondentID = "respondentID",
    mobility = "mobility",
    self_care = "self_care",
    usual = "usual",
    pain = "pain",
    anxiety = "anxiety",
    vas = "vas"
)
calculate_qalys(
    dat,
    time_index = "time_index",
    type = "VT",
    country = c("Denmark", "France")
)
#> # A tibble: 4,000 × 5
#>    respondentID .utility_type .utility_country .qaly .value
#>           <int> <chr>         <chr>            <chr>  <dbl>
#>  1            1 VT            Denmark          raw    0.544
#>  2            1 VT            France           raw    0.573
#>  3            2 VT            Denmark          raw    0.487
#>  4            2 VT            France           raw    0.503
#>  5            3 VT            Denmark          raw    0.530
#>  6            3 VT            France           raw    0.577
#>  7            4 VT            Denmark          raw    0.276
#>  8            4 VT            France           raw    0.430
#>  9            5 VT            Denmark          raw    0.506
#> 10            5 VT            France           raw    0.575
#> # ℹ 3,990 more rows
```
