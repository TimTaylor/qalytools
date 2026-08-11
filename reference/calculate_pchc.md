# Calculate the Paretian Classification of Health Change

`calculate_pchc()` calculates the Paretian Classification of Health
Change (PCHC) in an individuals health state between two surveys. It
wraps the [`eq5d::pchc()`](https://rdrr.io/pkg/eq5d/man/pchc.html)
function providing methods for
[EQ5D](https://timtaylor.github.io/qalytools/reference/as_eq5d.md)
objects.

## Usage

``` r
calculate_pchc(pre, post, no.problems = TRUE, by.dimension = FALSE)
```

## Arguments

- pre:

  An EQ5D object for a single survey.

- post:

  An EQ5D object for a single survey.

- no.problems:

  boolean. Summarise 11111 "No change" subjects in a "No problems"
  group.

- by.dimension:

  boolean. Summarise results by each EQ-5D dimension rather than by the
  whole dataset.

## Value

For `by.dimension = FALSE`:

- A data frame with columns 'Change', 'Number' and 'Percent'.

For `by.dimension = TRUE`:

- A data frame with columns 'Dimension', 'Change', 'Number' and
  'Percent'.

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
grp1 <- subset(dat, Group == "Group1")
grp2 <- subset(dat, Group == "Group2")
calculate_pchc(grp1, grp2)
#> # A tibble: 5 × 3
#>   Change       Number Percent
#>   <chr>         <dbl>   <dbl>
#> 1 No change        14      14
#> 2 Improve          59      59
#> 3 Worsen           14      14
#> 4 Mixed change     13      13
#> 5 No problems       0       0
calculate_pchc(grp1, grp2, by.dimension = TRUE)
#> # A tibble: 20 × 4
#>    .Dimension Change      Number Percent
#>    <chr>      <chr>        <dbl>   <dbl>
#>  1 MO         No change       31      31
#>  2 MO         Improve         26      26
#>  3 MO         Worsen           7       7
#>  4 MO         No problems     36      36
#>  5 SC         No change       21      21
#>  6 SC         Improve         31      31
#>  7 SC         Worsen           7       7
#>  8 SC         No problems     41      41
#>  9 UA         No change       33      33
#> 10 UA         Improve         43      43
#> 11 UA         Worsen           6       6
#> 12 UA         No problems     18      18
#> 13 PD         No change       59      59
#> 14 PD         Improve         34      34
#> 15 PD         Worsen           3       3
#> 16 PD         No problems      4       4
#> 17 AD         No change       14      14
#> 18 AD         Improve         22      22
#> 19 AD         Worsen          12      12
#> 20 AD         No problems     52      52
```
