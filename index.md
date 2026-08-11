# qalytools

  
***This package is currently under active development and the code
subject to change.***

*qalytools* provides a simple and intuitive user interface for the
analysis of
[EQ-5D](https://euroqol.org/information-and-support/euroqol-instruments/)
surveys. It builds upon the
[eq5d](https://cran.r-project.org/package=eq5d) package to facilitate
the calculation of QALY metrics, and other related values, across
multiple surveys.

## Installation

The development version of the package can be installed via:

``` r

    repos <- c("https://timtaylor.r-universe.dev", getOption("repos"))
    install.packages("qalytools", repos = repos)
```

## Guidance

Once installed the best way to familiarise yourself with the package is
to view the long form documentation:

- [`vignette("qalytools")`](https://timtaylor.github.io/qalytools/articles/qalytools.md)
  provides an introduction to the main functionality of the package.
- The package website also has an ‘EQ5D Analysis’
  [article](https://timtaylor.github.io/qalytools/articles/example_analysis.html)
  that illustrates how to use the package with more context.
