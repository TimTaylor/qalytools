#' EQ-5D-3L example data
#'
#' A dataset containing dimensions and grouping for 200 observations. Data
#'   was obtained from the [eq5d](https://cran.r-project.org/package=eq5d)
#'   package on 2021-09-16.
#'
#' @format A data frame with 200 rows and 6 variables:
#' \describe{
#'   \item{respondentID}{Unique respondent identifier}
#'   \item{surveyID}{Unique survey identifier}
#'   \item{mobility}{Mobility dimension valuea}
#'   \item{self_care}{Self-care dimension values}
#'   \item{usual}{Usual activities dimension values}
#'   \item{pain}{Pain/discomfort dimension values}
#'   \item{anxiety}{Anxiety/depression dimension values}
#'   \item{Group}{Observation group}
#'   \item{vas}{VAS score}
#'   \item{time_index}{Relative time within survey framework}
#' }
#' @source \url{https://github.com/fragla/eq5d/raw/master/inst/extdata/eq5d3l_example.xlsx}
"eq5d3l_example"

# -------------------------------------------------------------------------

#' EQ-5D-5L multiple survey example data
#'
#' A dataset containing dimension values and vas score for 1000 respondents
#'   across 10 surveys. Data was synthetically generated.
#'
#' @format A data frame with 10000 rows and 11 variables:
#' \describe{
#'   \item{surveyID}{Survey ID}
#'   \item{respondentID}{Respondent ID}
#'   \item{sex}{Respondent sex}
#'   \item{age}{Respondent age}
#'   \item{mobility}{Mobility dimension}
#'   \item{self_care}{Self-care dimension}
#'   \item{usual}{Usual activities dimension}
#'   \item{pain}{Pain/discomfort dimension}
#'   \item{anxiety}{Anxiety/depression dimension}
#'   \item{vas}{VAS score}
#'   \item{time_index}{Relative time within survey framework}
#' }
"EQ5D5L_surveys"
