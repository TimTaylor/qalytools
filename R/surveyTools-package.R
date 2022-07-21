#' Tools for Analysing EQ5D Surveys
#'
#' @description
#'
#' ***qalytools*** provides a simple and intuitive user interface for the
#' analysis of repeated [EQ-5D](https://euroqol.org/eq-5d-instruments) surveys,
#' facilitating calculation of QALY values and other related metrics.
#'
#' The package provides a range of functions:
#'
#' - Constructors (and validators) for EQ5D data frame subclasses (EQ5D3L,
#'   EQ5D5L and EQ5D): `new_eq5d3l()` (`validate_eq5d3l()`), `new_eq5d5l()`
#'   (`validate_eq5d5l()`) and `new_eq5dy()` (`validate_eq5dy()`).
#' - User-friendly coercion functions `as_eq5d3l()`, `as_eq5d5l()` and
#'   `as_eq5dy()`.
#' - The calculation of utility values based on a range of different value sets.
#'   This functionality is provided via the `calculate_utility()`, `add_utility()`
#'   and `available_valuesets()` functions which are wrappers around the
#'   [eq5d](https://cran.r-project.org/package=eq5d) package.
#' - The calculation of different Quality of Life Years (QALY) metrics including
#'   unadjusted 'raw' values, and the disutility from both perfect health and,
#'   optionally, a specified baseline. See `calculate_qalys()`.
#' - The calculation of the Paretian Classification of Health Change (PCHC) in an
#'   individual's health state between two surveys via `calculate_pchc()` (again
#'   wrapping the [eq5d](https://cran.r-project.org/package=eq5d) package).
#' - Easy calculation of responses with a health limitation (i.e. a non-one
#'   response in one of the dimensions) via `calculate_limitation()`.
#'
#' See `vignette("qalytools")` for an introduction to the main functionality
#' of the package and `vignette("example_analysis")` for an illustration of how
#' the package can be used as part of a larger analysis.
#'
#' @importFrom stats setNames
#' @importFrom utils stack head tail
#' @importFrom eq5d eq5d pchc valuesets
#' @importFrom ympes imp_assert_bool imp_assert_scalar_chr imp_assert_data_frame
#' @importFrom pillar tbl_sum
#' @import data.table
#' @keywords internal
"_PACKAGE"
