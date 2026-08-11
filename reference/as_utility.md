# Coerce to a utility object

Coerce data frames to utility objects

A utility object contains is a data frame that meets the following
criteria:

- It contains a column that acts as a unique respondent identifier and
  another that identifies different surveys over time.

- It contains additional columns that represent the country, type and
  value of a utility that has previously been calculated.

- Together, Each combination of respondent identifier, survey
  identifier, utility country and utility type should be unique and not
  duplicated across rows.

## Usage

``` r
as_utility(x, respondentID, surveyID, country, type, value)
```

## Arguments

- x:

  `[data.frame]`.

- respondentID:

  `[character]` Unique respondent identifier. The name of a variable in
  `x` that uniquely identifies respondents.

- surveyID:

  `[character]` Name of variable in `x` that uniquely identifies surveys
  over time. To avoid ambiguity the specified variable must be either
  numeric or a factor (in which case the order will be taken as that
  given by the factor levels).

- country:

  `[character]` Name of variable in `x` representing the utility
  country.

- type:

  `[character]` Name of variable in `x` representing the utility
  country.

- value:

  `[character]` Name of variable in `x` representing the utility value.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)
like `<utility>` object.
