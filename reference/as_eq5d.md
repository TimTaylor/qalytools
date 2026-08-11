# Coerce to an EQ5D object

We define an `<EQ5D>` object as a data frame meeting the following
criteria:

- It contains columns that represent dimensions from the [EQ5D
  survey](https://euroqol.org/information-and-support/euroqol-instruments/)
  specification as well as a column representing the Visual Analogue
  Score.

- It contains a column that acts as a unique respondent identifier and
  another that identifies different surveys over time. Together these
  should uniquely identify a response and no combination should be
  duplicated within the data frame.

`<EQ5D3L>`, `<EQ5D5L>` and `<EQ5DY3L>` objects are defined as a subclass
of EQ5D objects with the additional restriction that the corresponding
dimension columns in `x` are either NA or whole numbers bounded below by
1 and above by 3 or 5 (depending on the survey type).

## Usage

``` r
as_eq5d5l(
  x,
  respondentID,
  surveyID,
  mobility,
  self_care,
  usual,
  pain,
  anxiety,
  vas,
  ...
)

as_eq5d3l(
  x,
  respondentID,
  surveyID,
  mobility,
  self_care,
  usual,
  pain,
  anxiety,
  vas,
  ...
)

as_eq5dy3l(
  x,
  respondentID,
  surveyID,
  mobility,
  self_care,
  usual,
  pain,
  anxiety,
  vas,
  ...
)
```

## Arguments

- x:

  A data frame like object.

- respondentID:

  `[character]` The name of a variable in `x` that uniquely identifies
  respondents.

- surveyID:

  `[character]` Name of variable `x` that uniquely identifies surveys
  over time.

- mobility:

  `[character]` Name of the 'mobility' dimension in `x`.

- self_care:

  `[character]` Name of the 'self-care' dimension in `x`.

- usual:

  `[character]` Name of the 'usual activities' dimension in `x`.

- pain:

  `[character]` Name of the 'pain / discomfort' dimension in `x`.

- anxiety:

  `[character]` Name of the 'anxiety / depression' dimension in `x`.

- vas:

  `[character]` Name of the 'visual analogue score' variable in `x`.

- ...:

  Not currently used.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)
like `<EQ5D3L>`, `<EQ5D5L>` or `<EQ5DY3L>` object.

## Examples

``` r

data(eq5d3l_example)
as_eq5d3l(
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
#> # EQ-5D-3L: 200 x 10
#>       MO    SC    UA    PD    AD Group  surveyID respondentID   vas  time
#>    <int> <int> <int> <int> <int> <chr>  <fct>           <int> <dbl> <dbl>
#>  1     1     1     2     2     1 Group1 survey01            1    82     0
#>  2     1     1     1     2     1 Group2 survey01            2    92     0
#>  3     2     2     3     3     1 Group1 survey01            3    11     0
#>  4     1     1     1     2     1 Group2 survey01            4    90     0
#>  5     1     2     2     2     1 Group1 survey01            5    61     0
#>  6     1     1     1     1     1 Group2 survey01            6    96     0
#>  7     2     1     2     2     1 Group1 survey01            7    69     0
#>  8     1     1     1     1     1 Group2 survey01            8    80     0
#>  9     1     2     2     2     1 Group1 survey01            9    61     0
#> 10     1     2     2     1     1 Group2 survey01           10    79     0
#> # ℹ 190 more rows
```
