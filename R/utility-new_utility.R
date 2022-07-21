#' Utility constructor and validator
#'
#' @description
#'
#' Construct and validate utility objects.
#'
#' ## The utility class
#'
#' A utility object contains is an object that meets the following criteria:
#'
#'   - It contains a column that acts as a unique respondent identifier and
#'     another that identifies different surveys over time.
#'   - It contains additional columns that represent the country, type and value
#'     of a utility that has previously been calculated.
#'   - Together, Each combination of respondent identifier, survey identifier,
#'     utility country and utility type should be unique and not duplicated
#'     across rows.
#'
#' @details
#'
#' `new_utility()` creates a utility object with minimal checking of the input
#' arguments. It is mainly for developer use and should normally be accompanied
#' by a call to `validate_utility()` which performs further checking of the
#' underlying data.
#'
#' @param x `[data.frame]`.
#'
#' @param respondentID `[character]` Unique respondent identifier. The name of a
#' variable in `x` that uniquely identifies respondents.
#'
#' @param surveyID `[character]` Name of variable in `x` that uniquely
#' identifies surveys over time. To avoid ambiguity the specified variable must
#' be either numeric or a factor (in which case the order will be taken as that
#' given by the factor levels).
#'
#' @param country `[character]` Name of variable in `x` representing the utility
#' country.
#'
#' @param type `[character]` Name of variable in `x` representing the utility
#' country.
#'
#' @param value `[character]` Name of variable in `x` representing the utility
#' value.
#'
#' @param xx An \R Object to validate.
#'
#' @return
#'
#' A utility object (invisibly for `validate_utility()`).
#'
#' @export
new_utility <- function(
    x,
    respondentID,
    surveyID,
    country,
    type,
    value
) {

    # only check class of inputs at this stage
    x <- imp_assert_data_frame(x)
    respondentID <- imp_assert_scalar_chr(respondentID)
    surveyID <- imp_assert_scalar_chr(surveyID)
    country <- imp_assert_scalar_chr(country)
    type <- imp_assert_scalar_chr(type)
    value <- imp_assert_scalar_chr(value)

    # tbl for nice-printing
    # https://pillar.r-lib.org/#custom-table-classes

    structure(
        x,
        respondentID = respondentID,
        surveyID = surveyID,
        country = country,
        type = type,
        value = value,
        class = c("utility", "tbl", "data.frame")
    )
}

# -------------------------------------------------------------------------
#' @rdname new_utility
#' @export
validate_utility <- function(xx) {

    # Error if not passed an utility object
    if (!inherits(xx, "utility")) {
        stop("`xx` must be of class <utility>.")
    }

    # pull out relevant variables and check lengths
    respondentID <- imp_assert_scalar_chr(attr(xx, "respondentID"))
    surveyID <- imp_assert_scalar_chr(attr(xx, "surveyID"))
    country <- imp_assert_scalar_chr(attr(xx, "country"))
    type <- imp_assert_scalar_chr(attr(xx, "type"))
    value <- imp_assert_scalar_chr(attr(xx, "value"))

    # check columns presence
    names_x <- names(xx)
    vars <- c(
        respondentID = respondentID,
        surveyID = surveyID,
        country = country,
        type = type,
        value = value
    )
    for (i in seq_along(vars)) {
        v <- vars[i]
        if (!v %in% names_x) {
            stop(sprintf("`%s` variable (%s) not present in `xx`", names(v), sQuote(v)))
        }
    }

    # check unique combinations of survey, respondent ID, country and type
    combos <- xx[, c(respondentID, surveyID, country, type)]
    if (anyDuplicated(combos)) {
        stop("`respondentID`, `surveyID`, `country` and `type` combinations must not be duplicated.")
    }

    # check value data is numeric
    if (!is.numeric(.subset2(xx, value))) {
        stop("`value` column must be numeric.")
    }

    invisible(xx)
}
