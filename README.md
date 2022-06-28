
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

<a href="https://github.com/TimTaylor/surveyTools/actions" class="pkgdown-devel"><img src="https://github.com/TimTaylor/surveyTools/workflows/R-CMD-check/badge.svg" alt="R-CMD-check" /></a>
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<br> <span style="color:red"> ***This package is currently under active
development. The code has yet been reviewed and is likely to change over
the following months. We recommend it is not used for analysis for the
time being.*** </span>

# Overview

***surveyTools*** provides a simple and intuitive user interface for the
analysis of [EQ-5D](https://euroqol.org/eq-5d-instruments) surveys. It
builds upon the [eq5d package](https://cran.r-project.org/package=eq5d)
to facilitate the calculation of QALY values and other related metrics
across multiple surveys.

# Installation

The development version of the package can be installed via:

``` r
    repos <- c("https://timtaylor.r-universe.dev", getOption("repos"))
    install.packages("surveyTools", repos = repos)
```

# Guidance

Once installed the best way to familiarise yourself with the package is
to view the included documentation:

-   `vignette("surveyTools")` provides an introduction to the main
    functionality of the package.
-   `vignette("example_analysis")` illustrates how to use the package as
    part of a larger analysis.
