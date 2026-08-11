# EQ-5D-3L example data

A dataset containing dimensions and grouping for 200 observations. Data
was obtained from the [eq5d](https://cran.r-project.org/package=eq5d)
package on 2022-06-15, with additional variables added for package
compatibility.

## Usage

``` r
eq5d3l_example
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with 200 rows and 6 variables:

- respondentID:

  Unique respondent identifier

- surveyID:

  Unique survey identifier

- MO:

  Mobility dimension valuea

- SC:

  Self-care dimension values

- UA:

  Usual activities dimension values

- PD:

  Pain/discomfort dimension values

- AD:

  Anxiety/depression dimension values

- Group:

  Observation group

- vas:

  VAS score

- time:

  Relative time within survey framework

## Source

<https://github.com/fragla/eq5d/raw/master/inst/extdata/eq5d3l_example.xlsx>
